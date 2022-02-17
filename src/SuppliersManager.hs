{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SuppliersManager
  ( supManAPI
  , supManApp
  , notifySuppliersSM
  , relayUpdatesSM
  ) where

import Servant
import Servant.Client
import Control.Monad.Reader (ask)

import Types ( Configs (suppliersManager)
             , OrderData (_id, items)
             , OrderFragment (OrderFragment)
             , OrderUpdates (OrderUpdates, updates)
             , ItemData (supplier)
             , Supplier
             )
import Utils ( HeaderBasicAuth
             , CombinedM
             , combinedMServer
             , combinedMClients
             , createCliEnvFromLookup
             , addAuthCheck1
             , removeAuthCheck1
             )
import OrdManQuery (statusUpdateOM)
import SupplierAdapterQuery (notifySupplierSA)

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Control.Monad (join)

------
-- API Type Declaration

type SupManAPI = 
       "notify-suppliers" :> HeaderBasicAuth :> ReqBody '[JSON] OrderData :> Put '[JSON] OrderUpdates
  :<|> "relay-updates" :> HeaderBasicAuth :> ReqBody '[JSON] OrderUpdates :> Post '[JSON] NoContent

supManAPI :: Proxy SupManAPI
supManAPI = Proxy

------
-- Custom Types


------
-- Internal constants and functions

orderToFragmentsBySupplier :: OrderData -> [(Supplier, OrderFragment)]
orderToFragmentsBySupplier order = 
  let
    supplierI             = supplier :: ItemData -> Supplier
    suppliersPresent      = HS.toList . HS.fromList . (map supplierI) . HM.elems $ items order
    filterBySupplier hm s = HM.filter (\i -> supplierI i == s) hm 
    groupedItems          = map (snd . unzip . HM.toList . (filterBySupplier (items order))) suppliersPresent
  in 
    zip suppliersPresent $ map (OrderFragment (_id order)) groupedItems

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d 
uncurry3 f (x, y, z) = f x y z

------
-- Handler Functions

supManServerT :: ServerT SupManAPI CombinedM
supManServerT = notifySuppliersAuth
           :<|> relayUpdatesAuth
  
  where

    addAuthSM1 = addAuthCheck1 suppliersManager

    notifySuppliers :: OrderData -> CombinedM OrderUpdates
    notifySuppliers d = do
      supResponses <- mapM (uncurry notifySupplierSA) (orderToFragmentsBySupplier d)
      -- TODO: Code to catch when any suppliers can't fulfil order
      pure $ OrderUpdates (_id d) (join (map updates supResponses)) 
    notifySuppliersAuth = addAuthSM1 notifySuppliers

    relayUpdates :: OrderUpdates -> CombinedM NoContent
    relayUpdates (OrderUpdates oID xs) = do
      let (gtins, statuses) = unzip xs
      mapM (uncurry3 statusUpdateOM) (zip3 (repeat oID) gtins statuses)
      pure NoContent
    relayUpdatesAuth = addAuthSM1 relayUpdates

supManServer :: Configs -> Server SupManAPI
supManServer = combinedMServer supManAPI supManServerT

supManApp :: Configs -> Application
supManApp = (serve supManAPI) . supManServer


------
-- Pre-Authorized Query Functions

removeAuthSM1 = removeAuthCheck1 suppliersManager

notifySuppliersSM :: OrderData -> CombinedM OrderUpdates
notifySuppliersSM = removeAuthSM1 notifySuppliersSMAuth

relayUpdatesSM :: OrderUpdates -> CombinedM NoContent
relayUpdatesSM = removeAuthSM1 relayUpdatesSMAuth

notifySuppliersSMAuth :<|> relayUpdatesSMAuth = 
  combinedMClients supManAPI (createCliEnvFromLookup suppliersManager) 

------
-- Test Code

-- curl -X PUT -d '{"_id":"id003","productEAN":789}' -H 'Accept: application/json' -H 'Content-type: application/json' 'http://localhost:8082/notify'
-- curl -X POST -d '{"_id":"id003"}' -H 'Content-type: application/json' 'http://localhost:8082/relayUpdates'