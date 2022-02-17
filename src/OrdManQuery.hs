{-# LANGUAGE DataKinds #-} 
{-# LANGUAGE TypeOperators #-}

module OrdManQuery
  ( OrdManAPI
  , ordManAPI
  , statusUpdateOM
  ) where

import Servant
import Data.Aeson (Value)

import Utils ( HeaderBasicAuth
             , CombinedM
             , combinedMClients
             , createCliEnvFromLookup
             , removeAuthCheck1, removeAuthCheck2, removeAuthCheck3)
import Types ( Configs (orderManager)
             , ItemStatus
             , OrderData
             , OrderID
             , ItemData
             , GTIN
             )

------
-- API Type Declaration

type OrdManAPI = 
       "new-order" :> HeaderBasicAuth :> ReqBody '[JSON] OrderData :> PutNoContent
  :<|> "check" :> HeaderBasicAuth :> Capture "orderID" OrderID :> Get '[JSON] OrderData
  :<|> "check" :> HeaderBasicAuth :> Capture "orderID" OrderID :>  Capture "gtin" GTIN :> Get '[JSON] ItemData
  :<|> "status-update" :> HeaderBasicAuth :> Capture "orderID" OrderID :> Capture "gtin" GTIN :> Capture "newStatus" ItemStatus :> PutNoContent
  :<|> "delete-order" :> HeaderBasicAuth :> Capture "orderID" OrderID :> DeleteNoContent

ordManAPI :: Proxy OrdManAPI
ordManAPI = Proxy

------
-- Pre-Authorized Query Functions

removeAuthOM1 = removeAuthCheck1 orderManager
removeAuthOM2 = removeAuthCheck2 orderManager
removeAuthOM3 = removeAuthCheck3 orderManager

newOrderOM :: OrderData -> CombinedM NoContent
newOrderOM = removeAuthOM1 newOrderOMAuth

checkOrderOM :: OrderID -> CombinedM OrderData
checkOrderOM = removeAuthOM1 checkOrderOMAuth

checkItemOM :: OrderID -> GTIN -> CombinedM ItemData
checkItemOM = removeAuthOM2 checkItemOMAuth

statusUpdateOM :: OrderID -> GTIN -> ItemStatus -> CombinedM NoContent
statusUpdateOM = removeAuthOM3 statusUpdateOMAuth

deleteOrderOM :: OrderID -> CombinedM NoContent
deleteOrderOM = removeAuthOM1 deleteOrderOMAuth

newOrderOMAuth :<|> checkOrderOMAuth :<|> checkItemOMAuth :<|> statusUpdateOMAuth :<|> deleteOrderOMAuth = 
  combinedMClients ordManAPI (createCliEnvFromLookup orderManager)

------
-- Test via

-- curl -X PUT -d '{"_id":"id003","storeID":"Lucky Store","deliveryAddress":"The Moon","productEAN":789,"customerName":"Sally"}' -H 'Accept: application/json' -H 'Content-type: application/json' 'http://localhost:8081/submit'
-- curl -X GET 'http://localhost:8081/test'
-- curl -X GET 'http://localhost:8081/check/id003'