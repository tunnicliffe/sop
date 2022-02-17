{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DeliverersManager
  ( {-devManAPI
  , devManApp
  , notifyDM
  , dispatchedFromSM-}
  ) where

-- PLACEHOLDER ENDPOINTS BELOW
-- WIP

import Servant
import Servant.Client
import Control.Monad.Reader (ask)

import Types ( Configs (deliverersManager)
             , ItemStatus 
             , OrderFragment (items)
             , OrderUpdates
             , ItemData (supplier)
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

------
-- API Type Declaration
{-
type DevManAPI = 
       "notifyDM"   :> HeaderBasicAuth :> ReqBody '[JSON] OrderFragment :> Put '[JSON] OrderUpdates
  :<|> "dispatched" :> HeaderBasicAuth :> ReqBody '[JSON] OrderUpdates :> Post '[JSON] NoContent

devManAPI :: Proxy DevManAPI
devManAPI = Proxy

------
-- Custom Types


------
-- Constants and functions

------
-- Handler Functions

devManServerT :: ServerT DevManAPI CombinedM
devManServerT = notifyAuth
           :<|> dispatchedAuth
  
  where

    addAuthDM1 = addAuthCheck1 deliverersManager

    notify :: OrderFragment -> CombinedM OrderUpdates
    notify d = do
      let sup = supplier . head . HM.elems $ items d
      notifySupplierSA sup d
    notifyAuth = addAuthDM1 notify

    dispatched :: OrderUpdates -> CombinedM NoContent
    dispatched d = do
      statusUpdateOM (_id d) (gtin d) LeftSupplier
      pure NoContent
    dispatchedAuth = addAuthDM1 dispatched

devManServer :: Configs -> Server DevManAPI
devManServer = combinedMServer devManAPI devManServerT

devManApp :: Configs -> Application
devManApp = (serve devManAPI) . devManServer


------
-- Pre-Authorized Query Functions

removeAuthDM1 = removeAuthCheck1 deliverersManager

notifyDM :: OrderFragment -> CombinedM OrderUpdates
notifyDM = removeAuthDM1 notifyDMAuth

dispatchedFromSM :: OrderUpdates -> CombinedM NoContent
dispatchedFromSM = removeAuthDM1 dispatchedFromDMAuth

notifyDMAuth :<|> dispatchedFromDMAuth = 
  combinedMClients devManAPI (createCliEnvFromLookup deliverersManager) 
-}
------
-- Test Code

-- curl -X PUT -d '{"_id":"id003","productEAN":789}' -H 'Accept: application/json' -H 'Content-type: application/json' 'http://localhost:8082/notify'
-- curl -X POST -d '{"_id":"id003"}' -H 'Content-type: application/json' 'http://localhost:8082/dispatched'