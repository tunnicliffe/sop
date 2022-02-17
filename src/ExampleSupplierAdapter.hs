{-# LANGUAGE DataKinds, TypeOperators #-}

module ExampleSupplierAdapter
  ( testSupApp
  ) where

import Servant
import Servant.Client

import Types ( Configs (supplierAdapters)
             , Supplier (TestSupplier)
             , OrderFragment (OrderFragment)
             , OrderUpdates (OrderUpdates)
             )
import Utils ( HeaderBasicAuth
             , CombinedM
             , combinedMServer
             , combinedMClients
             , createCliEnvFromLookup
             , lookupSupplierAdapter
             , addAuthCheck1)
import SupplierAdapterQuery (SupAdapterAPI, supAdapterAPI)
-- SupAdapterAPI must be used for this supplier-adapter to conform to the API specifications
-- This ensures the notifySA function will work for this supplier-adapter

import qualified Data.HashMap.Strict as HM

------
-- Handler Functions

testSupServerT :: ServerT SupAdapterAPI CombinedM
testSupServerT = notifyAuth
  
  where

    addAuthTS1 = addAuthCheck1 (lookupSupplierAdapter TestSupplier)

    notify :: OrderFragment -> CombinedM OrderUpdates
    notify (OrderFragment orderID items) = pure $ OrderUpdates orderID [] -- TODO
    notifyAuth = addAuthTS1 notify

testSupServer :: Configs -> Server SupAdapterAPI
testSupServer = combinedMServer supAdapterAPI testSupServerT

testSupApp :: Configs -> Application
testSupApp = (serve supAdapterAPI) . testSupServer