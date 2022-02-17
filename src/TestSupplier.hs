{-# LANGUAGE DataKinds, TypeOperators, DuplicateRecordFields #-}

module TestSupplier
  ( testSupAPI
  , testSupApp
  ) where

import Servant
import Servant.Client

import Types ( Configs (supplierAdapters)
             , Supplier (TestSupplier)
             , OrderFragment (OrderFragment)
             , OrderUpdates (OrderUpdates)
             , ItemData (gtin)
             , ItemStatus (SupplierConfirmed, WithDeliverer)
             , Deliverer (TestDeliverer)
             )
import Utils ( HeaderBasicAuth
             , CombinedM
             , combinedMServer
             , combinedMClients
             , createCliEnvFromLookup
             , lookupSupplierAdapter
             , addAuthCheck1)
import SuppliersManager (relayUpdatesSM)
import SupplierAdapterQuery (SupAdapterAPI)
-- SupAdapterAPI must be used for this supplier-adapter to conform to the API specifications
-- This ensures the notifySA function will work for this supplier-adapter

------
-- API Type Declaration

type TestSupAPI = 
       SupAdapterAPI
  :<|> "testSupUpdates" :> HeaderBasicAuth :> ReqBody '[JSON] OrderUpdates :> PutNoContent

testSupAPI :: Proxy TestSupAPI
testSupAPI = Proxy

------
-- Handler Functions

testSupServerT :: ServerT TestSupAPI CombinedM
testSupServerT = notifyAuth
            :<|> testSupUpdatesAuth
  
  where

    addAuthTS1 = addAuthCheck1 (lookupSupplierAdapter TestSupplier)

    notify :: OrderFragment -> CombinedM OrderUpdates
    notify (OrderFragment orderID items) = do
      let responses = zip (map gtin items) (repeat SupplierConfirmed)
      pure $ OrderUpdates orderID responses
    notifyAuth = addAuthTS1 notify

    testSupUpdates :: OrderUpdates -> CombinedM NoContent
    testSupUpdates = relayUpdatesSM
    testSupUpdatesAuth = addAuthTS1 testSupUpdates

testSupServer :: Configs -> Server TestSupAPI
testSupServer = combinedMServer testSupAPI testSupServerT

testSupApp :: Configs -> Application
testSupApp = (serve testSupAPI) . testSupServer