{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module SupplierAdapterQuery
  ( SupAdapterAPI
  , supAdapterAPI
  , notifySupplierSA
  ) where

import Servant
import Servant.Client

import Types (Supplier, OrderFragment, OrderUpdates)
import Utils ( HeaderBasicAuth
             , CombinedM
             , removeAuthCheck1
             , combinedMClients
             , createCliEnvFromLookup
             , lookupSupplierAdapter
             )

type SupAdapterAPI = 
  "notifySupplierSA" :> HeaderBasicAuth :> ReqBody '[JSON] OrderFragment :> Put '[JSON] OrderUpdates
-- TODO: add editOrder and cancelOrder endpoints

supAdapterAPI :: Proxy SupAdapterAPI
supAdapterAPI = Proxy

-- We generate query functions without actually serving the API, just like for CouchDB
notifySupplierSA :: Supplier -> OrderFragment -> CombinedM OrderUpdates
notifySupplierSA sup = (removeAuthSA1 sup) (notifySupplierSAAuth sup)

removeAuthSA1 sup = removeAuthCheck1 (lookupSupplierAdapter sup)

notifySupplierSAAuth sup = combinedMClients supAdapterAPI (createCliEnvFromLookup (lookupSupplierAdapter sup))