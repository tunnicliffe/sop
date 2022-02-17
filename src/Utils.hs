{-# LANGUAGE DataKinds, 
             FlexibleContexts, 
             OverloadedStrings, 
             DuplicateRecordFields,
             DeriveGeneric,
             GeneralizedNewtypeDeriving #-} 

module Utils 
  ( runApp
  , runAppTLS
  , runAppTLSTest
  , HeaderBasicAuth
  , parseConfigs
  , CombinedM
  , runCombinedM
  , combinedMServer
  , combinedMClients
  , addAuthCheck0, addAuthCheck1, addAuthCheck2, addAuthCheck3
  , removeAuthCheck0, removeAuthCheck1, removeAuthCheck2, removeAuthCheck3
  , credentialsToBS
  , printM
  , lookupSupplierAdapter
  , createCliEnvFromLookup
  , fromResult, fromResultM
  , fromHeader, fromHeaderM
  ) where

import Network.Wai (Request)
import Network.Wai.Handler.Warp (Settings, runSettings, setPort, defaultSettings, setLogger, setBeforeMainLoop)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Servant
import Servant.Client
import Servant.Client.Internal.HttpClient (unClientM)
import Servant.Client.Core (parseBaseUrl, baseUrlPort)
import Servant.API (BasicAuthData (BasicAuthData))
import Servant.Server
import Network.HTTP.Types (Status, statusCode, statusMessage)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Types ( Configs (..)
             , CoreComponent (CoreComponent)
             , Supplier
             , SupplierAdapter (SupplierAdapter, supplier, url, credentials)
             , IsAPI (..)
             , OrderData (_id, items)
             , ItemData (supplier)
             , ContactData
             )

import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, mapReaderT, withReaderT, ask)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, withExceptT, throwError, catchError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow, MonadCatch)

import GHC.Generics (Generic)
import Data.Foldable (toList)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as L8
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.List (sortBy, groupBy)
import Data.CaseInsensitive (mk)
import Data.Aeson (Result (Success, Error), FromJSON, fromJSON, toJSON, encode, decode)
import Data.Binary.Builder (fromLazyByteString)
import Data.Functor.Alt (Alt, (<!>))

--- Core run Application functions

runApp :: (IsAPI a) => Maybe FilePath -- Log destination
                    -> Configs 
                    -> (Configs -> a) 
                    -> (Configs -> Application) 
                    -> IO ()
runApp maybeLogFP configs api app = runSettings s (app configs)
  where s = setPort (getPort $ api configs) $ maybeSetLogger maybeLogFP defaultSettings 

runAppTLS :: (IsAPI a) => Maybe FilePath 
                       -> Configs 
                       -> (FilePath, FilePath)
                       -> (Configs -> a) 
                       -> (Configs -> Application) 
                       -> IO ()
runAppTLS maybeLogFP configs (certFP, keyFP) api app = do
  let 
    tlsSet  = tlsSettings certFP keyFP
    warpSet = setPort (getPort $ api configs) $ maybeSetLogger maybeLogFP defaultSettings 
  runTLS tlsSet warpSet (app configs)

maybeSetLogger :: Maybe FilePath -> Settings -> Settings 
maybeSetLogger Nothing      = id 
maybeSetLogger (Just logFP) = setLogger (basicLogger logFP)

basicLogger :: FilePath -> Request -> Status -> Maybe Integer -> IO ()
basicLogger fp req s maybeFS = appendFile fp logStr
  where logStr = "\nREQUEST: " ++ show req ++ "\nSTATUS: " ++ show s ++ "\nFILE-SIZE: " ++ show maybeFS
-- TODO: something cleaner with the file-size. "Nothing" and "Just" look bad.

runAppTLSTest :: (IsAPI a) => Maybe FilePath
                           -> Configs 
                           -> (FilePath, FilePath)
                           -> (Configs -> a)
                           -> (Configs -> Application)
                           -> IO ()
                           -> IO ()
runAppTLSTest maybeLogFP configs (certFP, keyFP) api app action = do 
  let 
    tlsSet  = tlsSettings certFP keyFP
    warpSet = setBeforeMainLoop action $ setPort (getPort $ api configs) $ maybeSetLogger maybeLogFP defaultSettings 
  runTLS tlsSet warpSet (app configs)
-- Useful for running test as soon as the API is listening.

--- Convinience type aliases

type HeaderBasicAuth = Header "Authorization" S8.ByteString


--- Config parsing functions

data RawConfigs = RawConfigs
  { database          :: RawCoreComponent
  , orderManager      :: RawCoreComponent
  , suppliersManager  :: RawCoreComponent
  , deliverersManager :: RawCoreComponent
  , supplierAdapters  :: [RawSupplierAdapter]
  , deliveryContact   :: ContactData
  , adminContact      :: ContactData
  } deriving (Generic)
instance FromJSON RawConfigs

data RawCoreComponent = RawCoreComponent
  { name        :: String
  , url         :: String 
  , credentials :: (String, String)
  } deriving (Generic)
instance FromJSON RawCoreComponent

data RawSupplierAdapter = RawSupplierAdapter
  { supplier    :: Supplier
  , url         :: String
  , credentials :: (String, String)
  } deriving (Generic)
instance FromJSON RawSupplierAdapter

parseConfigs :: FilePath -> IO Configs
parseConfigs path = do 
  raw <- Yaml.decodeFileThrow path 
  db <- parseCoreComponent $ database (raw :: RawConfigs) 
  om <- parseCoreComponent $ orderManager (raw :: RawConfigs) 
  sm <- parseCoreComponent $ suppliersManager (raw :: RawConfigs) 
  dm <- parseCoreComponent $ deliverersManager (raw :: RawConfigs)
  saList <- mapM parseSupplierAdapter $ supplierAdapters (raw :: RawConfigs)
  let 
    sa = HM.fromList $ zip (map (supplier :: SupplierAdapter -> Supplier) saList) saList 
    dc = deliveryContact (raw :: RawConfigs)
    ac = adminContact (raw :: RawConfigs)
  cm <- newManager tlsManagerSettings
  pure $ Configs db om sm dm sa dc ac cm

parseCoreComponent :: RawCoreComponent -> IO CoreComponent
parseCoreComponent (RawCoreComponent n u c) = do 
  safeUrl <- parseBaseUrl u 
  pure $ CoreComponent n safeUrl (baseUrlPort safeUrl) (mkBAD c)

parseSupplierAdapter :: RawSupplierAdapter -> IO SupplierAdapter
parseSupplierAdapter (RawSupplierAdapter s u c) = do
  safeUrl <- parseBaseUrl u 
  pure $ SupplierAdapter s safeUrl (baseUrlPort safeUrl) (mkBAD c)

mkBAD :: (String, String) -> BasicAuthData
mkBAD (u, p) = BasicAuthData (S8.pack u) (S8.pack p)


--- A monad that the ClientM and Handler monads can both be 'hoisted' into

-- We want to convert: Handler   a =                    ExceptT ServerError IO  a
-- and:                ClientM   a = ReaderT ClientEnv (ExceptT ClientError IO) a
-- To:                 CombinedM a = ReaderT Configs   (ExceptT ServerError IO) a

newtype CombinedM a = CombinedM
  { unCombinedM :: ReaderT Configs (ExceptT ServerError IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader Configs, MonadError ServerError, MonadThrow
           , MonadCatch)

instance Alt CombinedM where
  a <!> b = a `catchError` (\_ -> b) 
{-
instance RunClient CombinedM where
  runRequest = (clientMToCombinedM (const ask)) . runRequest 
  throwClientError = throwError . clientErrorToServerError 
-- This instance has to be here, and not in Types.hs, to avoid cyclic dependency.
-- All queries and being made through Servant's generated query functions,
-- so no additional functionality gained through this class for now.
-}
runCombinedM :: Configs -> CombinedM a -> IO (Either ServerError a)
runCombinedM c m = runExceptT $ runReaderT (unCombinedM m) c

combinedMServer :: HasServer api '[] => Proxy api -> ServerT api CombinedM -> Configs -> Server api 
combinedMServer proxyAPI combinedServerT configs = 
  hoistServer proxyAPI (combinedMToHandler configs) combinedServerT
-- This depends on the API as it is used across the different APIs
  
combinedMToHandler :: Configs -> CombinedM a -> Handler a
combinedMToHandler configs comM = Handler $ runReaderT (unCombinedM comM) configs

combinedMClients :: HasClient ClientM api => Proxy api -> (Configs -> ClientEnv) -> Client CombinedM api
combinedMClients proxyAPI confToCE = hoistClient proxyAPI (clientMToCombinedM confToCE) (client proxyAPI) 

clientMToCombinedM :: (Configs -> ClientEnv) -> ClientM a -> CombinedM a
clientMToCombinedM confToCE cliM = 
  CombinedM $ mapReaderT (withExceptT clientErrorToServerError) $ withReaderT confToCE $ unClientM cliM

clientErrorToServerError :: ClientError -> ServerError 
clientErrorToServerError (FailureResponse req res) = 
  addCliErrHeader "FailureResponse" $
  appendToBody ("\nRequest was:\n" ++ show req) $
  convResToSE res 
clientErrorToServerError (DecodeFailure text res)  = 
  addCliErrHeader "DecodeFailure" $
  appendToBody ("\nText:\n" ++ T.unpack text) $ --TODO: What does text represent?
  convResToSE res
clientErrorToServerError (UnsupportedContentType mediaType res) = 
  addCliErrHeader "UnsupportedContentType" $
  appendToBody ("\nContent-type is:\n" ++ show mediaType) $
  convResToSE res
clientErrorToServerError (InvalidContentTypeHeader res) = 
  addCliErrHeader "InvalidContentTypeHeader" $
  convResToSE res
clientErrorToServerError (ConnectionError exception) = 
  addCliErrHeader "ConnectionError" $
  err500 { errBody = L8.fromString $ show exception }

addCliErrHeader :: String -> ServerError -> ServerError
addCliErrHeader x se = se { errHeaders = (mk "client-error", S8.pack x) : (errHeaders se) }

appendToBody :: String -> ServerError -> ServerError
appendToBody x se = se { errBody = L.append (errBody se) (L8.fromString x) }

convResToSE :: Response -> ServerError
convResToSE res = 
  ServerError (statusCode $ responseStatusCode res) 
              (show . statusMessage $ responseStatusCode res)
              (responseBody res)
              (toList $ responseHeaders res)


--- Basic authorization related functions

buildBasicAuth :: BasicAuthData -> S8.ByteString
buildBasicAuth (BasicAuthData u p) = 
  S8.append "Basic " (BAE.convertToBase BAE.Base64 (S8.concat [u, ":", p]))

addAuthCheck0 :: (IsAPI a) => (Configs -> a) -> CombinedM b -> Maybe S8.ByteString -> CombinedM b 
addAuthCheck0 _      _ Nothing   = throwError err401 {errBody = "Basic Authentication header missing."} 
addAuthCheck0 lookup f (Just bs) = do 
  configs <- ask
  if bs == (buildBasicAuth . getCredentials $ lookup configs)
    then do
      liftIO $ putStrLn "Basic Authentication check passed."
      f
    else throwError err401 {errBody = "Basic Authentication denied."}

addAuthCheck1 lookup f bs x = addAuthCheck0 lookup (f x) bs

addAuthCheck2 lookup f bs x y = addAuthCheck0 lookup (f x y) bs

addAuthCheck3 lookup f bs x y z = addAuthCheck0 lookup (f x y z) bs

removeAuthCheck0 :: (IsAPI a) => (Configs -> a) -> (Maybe S8.ByteString -> CombinedM b) -> CombinedM b
removeAuthCheck0 lookup f = do 
  configs <- ask
  f (Just . buildBasicAuth . getCredentials $ lookup configs)

removeAuthCheck1 :: (IsAPI a) => (Configs -> a) -> (Maybe S8.ByteString -> b -> CombinedM c) -> b -> CombinedM c
removeAuthCheck1 lookup f x = do
  configs <- ask
  f (Just . buildBasicAuth . getCredentials $ lookup configs) x

removeAuthCheck2 lookup f x y = removeAuthCheck1 lookup (uncurryPair3 f) (x, y)

removeAuthCheck3 lookup f x y z = removeAuthCheck1 lookup (uncurryPair4 f) (x, y, z)

credentialsToBS :: BasicAuthData -> Maybe S8.ByteString
credentialsToBS = Just . buildBasicAuth

uncurryPair3 :: (a -> b -> c -> d) -> a -> (b, c) -> d
uncurryPair3 f a (b, c) = f a b c

uncurryPair4 :: (a -> b -> c -> d -> e) -> a -> (b, c, d) -> e
uncurryPair4 f a (b, c, d) = f a b c d


--- Misc

printM :: (MonadIO m, Show a) => a -> m ()
printM = liftIO . print

createCliEnvFromLookup :: (IsAPI a) => (Configs -> a) -> Configs -> ClientEnv
createCliEnvFromLookup l c = mkClientEnv (connectionManager c) (getUrl $ l c)

lookupSupplierAdapter :: Supplier -> Configs -> SupplierAdapter
lookupSupplierAdapter s c = HM.lookupDefault err s (supplierAdapters (c :: Configs)) where
  err = error $ "Supplier not found in supplierAdapters:" ++ show s

fromResult :: Result a -> a
fromResult (Success x) = x
fromResult (Error y)   = error y

fromResultM :: Result a -> CombinedM a
fromResultM (Success x) = pure x
fromResultM (Error y)   = 
  throwError $ err500 { errBody = L8.fromString $ "fromResultM parse error: " ++ y }

fromHeader :: ResponseHeader n v -> v
fromHeader (Header x) = x
fromHeader MissingHeader = error "Missing header."
fromHeader (UndecodableHeader _) = error "Undecodable header."

fromHeaderM :: ResponseHeader n v -> CombinedM v
fromHeaderM (Header x) = pure x
fromHeaderM MissingHeader = throwError $ err500 { errBody = "Missing header." }
fromHeaderM (UndecodableHeader _) = throwError $ err500 { errBody = "Undecodable header."}