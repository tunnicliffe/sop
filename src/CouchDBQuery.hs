{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CouchDBQuery
  ( couchDBAPI
  , putDoc
  , getDoc
  , headDoc
  , deleteDoc
  ) where

import Servant
import Data.Aeson (Value)

import Types (CouchDBResponse, Configs (database))
import Utils (HeaderBasicAuth, CombinedM, combinedMClients, createCliEnvFromLookup, removeAuthCheck1, removeAuthCheck2, removeAuthCheck3)

------
-- API Type Declaration

type Head = Verb HEAD 200

type CouchDBAPI = 
       HeaderBasicAuth :> Capture "doc-id" String :> Header "If-Match" String :> ReqBody '[JSON] Value :> Put '[JSON] CouchDBResponse
  :<|> HeaderBasicAuth :> Capture "doc-id" String :> Get '[JSON] (Headers '[Header "ETag" String] Value) 
  :<|> HeaderBasicAuth :> Capture "doc-id" String :> Head '[JSON] (Headers '[Header "ETag" String] NoContent) 
  :<|> HeaderBasicAuth :> Capture "doc-id" String :> Header "If-Match" String :> Delete '[JSON] CouchDBResponse

couchDBAPI :: Proxy CouchDBAPI
couchDBAPI = Proxy

------
-- Pre-Authorized Query Functions

removeAuthDB1 = removeAuthCheck1 database
removeAuthDB2 = removeAuthCheck2 database
removeAuthDB3 = removeAuthCheck3 database

putDoc :: String -> Maybe String -> Value -> CombinedM CouchDBResponse
putDoc = removeAuthDB3 putDocAuth

getDoc :: String -> CombinedM (Headers '[Header "ETag" String] Value) 
getDoc = removeAuthDB1 getDocAuth

headDoc :: String -> CombinedM (Headers '[Header "ETag" String] NoContent)
headDoc = removeAuthDB1 headDocAuth

deleteDoc :: String -> String -> CombinedM CouchDBResponse
deleteDoc docID rev = (removeAuthDB2 deleteDocAuth) docID (Just rev)
-- 'rev' field is always required for a deletion, there is no Maybe about it

putDocAuth :<|> getDocAuth :<|> headDocAuth :<|> deleteDocAuth = 
  combinedMClients couchDBAPI (createCliEnvFromLookup database) 

------
-- TODO