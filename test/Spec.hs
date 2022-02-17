{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DataKinds #-}

import CouchDBQuery (couchDBAPI)
import OrdManQuery (ordManAPI)
import OrdManServe (timestampOrderData, updateItemStatusUnsafe, ordManApp)
import SuppliersManager (supManAPI, supManApp)
import TestSupplier (testSupAPI, testSupApp)
import EditJSON (removeField)
import Types ( OrderData (..)
             , Configs (..)
             , Shop (TestShop)
             , IsAPI (..)
             , ItemData (ItemData, gtin)
             , CouchDBResponse (..)
             , ItemStatus (..)
             , Supplier (TestSupplier)
             , Deliverer (TestDeliverer)
             , CustomerData (CustomerData)
             , Address (Address)
             , OrderUpdates (OrderUpdates)
             )
import Utils (runAppTLSTest, parseConfigs, credentialsToBS, lookupSupplierAdapter)
import EditJSON (removeField)

import Control.Concurrent (forkIO, killThread, threadDelay, myThreadId)
import Control.Concurrent.Async (async, waitCatch)
import Control.Exception.Base (SomeException, displayException, fromException, AsyncException(ThreadKilled), throw)
import Test.QuickCheck (Arbitrary, generate, arbitrary, infiniteListOf, elements)
import System.Directory (createDirectoryIfMissing)

import Control.Monad.Free (Free(Pure, Free))
import Control.Monad (when)
import Servant.API
import Servant.Client (BaseUrl)
import Servant.Client.Free (ClientF (RunRequest, Throw), client)
import Servant.Client.Internal.HttpClient (defaultMakeClientRequest, clientResponseToResponse)
import Network.HTTP.Client (Request (requestBody), RequestBody(..), Manager, httpLbs, newManager)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Connection (TLSSettings (TLSSettingsSimple))

import Data.Aeson (Value, toJSON)
import Data.Time.Calendar (Day (ModifiedJulianDay))
import Data.Time.Clock (getCurrentTime)
import Data.Binary.Builder (toLazyByteString)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM

--

instance Arbitrary OrderData where
  arbitrary = stringsToOrdData <$> randStrings where 
    randChars = infiniteListOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    randStrings = (chunksOf 10) <$> randChars
    day0 = ModifiedJulianDay 0
    stringsToOrdData s = 
      OrderData 
        (s!!1) 
        TestShop 
        (HM.singleton (s!!2) (ItemData (s!!2) (s!!3) (fromEnum $ head (s!!4)) TestSupplier Nothing (ModifiedJulianDay 0) Nothing Created []))
        (CustomerData (s!!5) (s!!6) (Address (s!!7) (s!!8) (s!!9) (s!!10) (s!!11) (s!!12)) (s!!13) (s!!14))
    chunksOf n xs = repeatedly (splitAt n) xs
    repeatedly f ys = z : repeatedly f zs where (z, zs) = f ys

instance Show RequestBody where
  show (RequestBodyLBS lazyBS)      = show lazyBS
  show (RequestBodyBS strictBS)     = show strictBS
  show (RequestBodyBuilder _ b)     = show $ toLazyByteString b 
  show (RequestBodyStream _ _)      = "RequestBodyStream"
  show (RequestBodyStreamChunked _) = "RequestBodyStreamChunked"
  show (RequestBodyIO _)            = "RequestBodyIO"

--

class TestEq a where 
  (~=) :: a -> a -> Bool
-- We use this class instead of Eq to ignore timing discrepencies and ignore the '_rev' field from CouchDB

instance (TestEq a) => TestEq [a] where
  (~=) (x:xs) (y:ys) = (x ~= y) && (xs ~= ys)
  (~=) []     []     = True
  (~=) _      _      = False

instance (TestEq a, TestEq b) => TestEq (a, b) where
  (~=) (a1, b1) (a2, b2) = (a1 ~= a2) && (b1 ~= b2) 

instance (Eq k, Hashable k, TestEq v) => TestEq (HM.HashMap k v) where
  (~=) hm1 hm2 = (HM.size hm1 == HM.size hm2) && (HM.size hm1 == HM.size x) && (and $ HM.elems x) where 
    x = HM.intersectionWith (~=) hm1 hm2
-- They have the same keys and each value pair satisfies (~=)

instance TestEq NoContent where 
  (~=) _ _ = True

instance TestEq OrderData where
  (~=) (OrderData id1 shop1 items1 customer1) 
       (OrderData id2 shop2 items2 customer2) = and
    [ id1       == id2
    , shop1     == shop2
    , items1    ~= items2
    , customer1 == customer2
    ] -- Ignore _rev field

instance TestEq ItemData where
  (~=) (ItemData gtin1 description1 quantity1 supplier1 deliverer1 deliveryDateRequested1 _ status1 statusHistory1) 
       (ItemData gtin2 description2 quantity2 supplier2 deliverer2 deliveryDateRequested2 _ status2 statusHistory2) = and
    [ gtin1                       == gtin2
    , description1                == description2
    , quantity1                   == quantity2
    , supplier1                   == supplier2
    , deliverer1                  == deliverer2
    , deliveryDateRequested1      == deliveryDateRequested2
    , status1                     == status2
    , fst (unzip statusHistory1)  == fst (unzip statusHistory2)
    ] -- Ignore Day and Time values

instance TestEq CouchDBResponse where
  (~=) (CouchDBResponse ok1 id1 _) 
       (CouchDBResponse ok2 id2 _) = 
    (ok1 == ok2) && (id1 == id2) -- Ignore rev field

instance TestEq Value where
  (~=) v1 v2 = 
    (==) (removeField "_rev" v1) (removeField "_rev" v2) -- Ignore _rev field if it exists

instance (Show a) => Show (Headers hlist a) where 
  show = show . getResponse

instance (TestEq a) => TestEq (Headers hlist a) where 
  (~=) x y = (~=) (getResponse x) (getResponse y) 

---

--We use the Free monad to inspect the requests and responses directly.
--Note that any intermediary requests and responses aren't available.
--To inspect those, some sort of logging needs to be implemented.
--Could be through a Writer Monad effect, couple be through Warp settings.

clientFTester :: (Show a, TestEq a) => (Manager, BaseUrl) -> Free ClientF a -> a -> IO a
clientFTester (mgr, burl) query expAns = case query of
  Pure n                  -> error $ "Error: got pure result: " ++ show n
  Free (Throw err)        -> error $ "Error: got immediate error: " ++ show err
  Free (RunRequest req k) -> do
    let req' = defaultMakeClientRequest burl req -- servant's Request -> http-client's Request
    putStrLn $ "Making request: " ++ show req'
    putStrLn $ "With request body: " ++ (show $ requestBody req')
    res' <- httpLbs req' mgr
    putStrLn $ "Got response: " ++ show res'
    let res = clientResponseToResponse Prelude.id res' -- http-client's Response -> servant's Response
    case k res of
      Pure n -> do
        if n ~= expAns 
        then do
          putStrLn "Result good."
          putStrLn "-----"
          pure n
        else do
          putStrLn "Expected result:"
          print expAns
          putStrLn "But instead got:"
          print n
          error "Unexpected result in clientFTester. Abandoning tests."

-- Pattern followed is as such:
-- 1) Generate random data
-- 2) Calculate expected response body from endpoint given random data
-- 3) Query endpoint using the Free Monad to see the full details of the request and response
-- 4) Compare the response body with the expected value under the TestEq operator (~=)

putDocF :<|> getDocF :<|> headDocF :<|> deleteDocF = 
  client couchDBAPI

newOrderOMF :<|> checkOrderOMF :<|> checkItemOMF :<|> statusUpdateOMF :<|> deleteOrderOMF = 
  client ordManAPI

testSupNotifyF :<|> testSupUpdatesF =
  client testSupAPI

--Note this is 'client' from Servant.Client.Free, not from Servant.Client 

---

main :: IO ()
main = do

  configs     <- parseConfigs "configs/configs.yaml"
  testConMan  <- newManager $ mkManagerSettings (TLSSettingsSimple True False False) Nothing 
  -- True means certificate validation is disabled during tests

  randSubData <- generate arbitrary
  randOrdData <- timestampOrderData randSubData

  time <- getCurrentTime
  
  let 

    testConfigs    = configs {connectionManager = testConMan}

    randOrdDataVal = toJSON randOrdData
    randDocID      = _id randOrdData
    randItemID     = gtin . head . snd . unzip . HM.toList $ items randSubData
    testUpdates    = OrderUpdates randDocID [(randItemID, AtCustomer)]

    expCheckRes1   = randOrdData
    expCheckRes2   = updateItemStatusUnsafe randItemID (WithDeliverer TestDeliverer, time) expCheckRes1
    expCheckRes3   = updateItemStatusUnsafe randItemID (AtCustomer, time) expCheckRes2

    createCEPair   = \x -> (testConMan, getUrl x)
    dbCE           = createCEPair $ database configs
    ordManCE       = createCEPair $ orderManager configs
    supManCE       = createCEPair $ suppliersManager configs
    testSupCE      = (testConMan, getUrl $ lookupSupplierAdapter TestSupplier configs)
    dbBS           = credentialsToBS $ getCredentials $ database configs
    ordManBS       = credentialsToBS $ getCredentials $ orderManager configs
    testSupBS      = credentialsToBS $ getCredentials $ lookupSupplierAdapter TestSupplier configs

    expCouchResp   = CouchDBResponse True randDocID "revision" -- CouchDB response will be the same in both cases 

    certKeyFPPair  = ("tls-test/certificate.pem", "tls-test/key.pem")

  createDirectoryIfMissing True "logs/test"

  putStrLn "Read configs as:"
  print configs

  putStrLn "Submitting a random document to CouchDB (putDoc):"
  res1 <- clientFTester 
            dbCE 
            (putDocF dbBS randDocID Nothing randOrdDataVal) 
            expCouchResp 
  
  putStrLn "Querying the head of the document (headDoc):"
  clientFTester
    dbCE
    (headDocF dbBS randDocID)
    (addHeader "" NoContent)

  putStrLn "Querying the document from CouchDB (getDoc):"  
  clientFTester 
    dbCE 
    (getDocF dbBS randDocID) 
    (addHeader "" randOrdDataVal)
  
  putStrLn "Deleting the document from CouchDB (deleteDoc):"
  clientFTester 
    dbCE 
    (deleteDocF dbBS randDocID (Just $ rev res1)) 
    expCouchResp 

  putStrLn "-----"
  putStrLn "Starting APIs.."

  let 
    actionChain0 = do
      t0 <- myThreadId
      runAppTLSTest 
        (Just "logs/test/OrderManager.log") 
        testConfigs 
        certKeyFPPair 
        orderManager 
        ordManApp
        (actionChain1 t0)
    actionChain1 t0 = do 
      t1 <- myThreadId
      forkIO $  runAppTLSTest 
                  (Just "logs/test/SuppliersManager.log")
                  testConfigs 
                  certKeyFPPair 
                  suppliersManager 
                  supManApp
                  (actionChain2 t0 t1)
      pure () 
    actionChain2 t0 t1 = do 
      t2 <- myThreadId
      forkIO $  runAppTLSTest
                  (Just "logs/test/TestSupplierAdapter.log")
                  testConfigs 
                  certKeyFPPair 
                  (lookupSupplierAdapter TestSupplier) 
                  testSupApp
                  (do {forkIO $ testingAction t0 t1 t2; pure ()})
      pure () 
    testingAction t0 t1 t2 = do 

      putStrLn "All APIs ready."
      putStrLn "-----"

      putStrLn "Submitting order to the Order Manager (sumbitOM):"
      clientFTester 
        ordManCE 
        (newOrderOMF ordManBS randSubData) 
        NoContent

      putStrLn "Checking status of order after submission (checkOM):"
      clientFTester 
        ordManCE 
        (checkOrderOMF ordManBS randDocID) 
        expCheckRes1

      putStrLn "Updating status of order (statusUpdateOM):"
      clientFTester 
        ordManCE 
        (statusUpdateOMF ordManBS randDocID randItemID (WithDeliverer TestDeliverer)) 
        NoContent
  
      putStrLn "Checking status of order after update (checkOM):"
      clientFTester 
        ordManCE 
        (checkOrderOMF ordManBS randDocID) 
        expCheckRes2

      putStrLn "Updating status of order via TestSupplier (testSupUpdatesF):"
      clientFTester 
        testSupCE 
        (testSupUpdatesF testSupBS testUpdates) 
        NoContent

      putStrLn "Checking status of order after update from TestSupplier (checkOM):"
      clientFTester 
        ordManCE 
        (checkOrderOMF ordManBS randDocID) 
        expCheckRes3

      putStrLn "Deleting order (deleteOrderOM):"
      clientFTester 
        ordManCE 
        (deleteOrderOMF ordManBS randDocID) 
        NoContent

      putStrLn "-----"
      putStrLn "All endpoint tests finished."
      putStrLn "Closing component APIs.."

      killThread t0 
      killThread t1
      killThread t2

  a <- async actionChain0
  -- Actions are chained this way to ensure each API is already listening before we proceed with the tests
  waitCatch a >>= exceptionHandle

---

exceptionHandle :: Either SomeException a -> IO ()
exceptionHandle (Left e) = if fromException e == Just ThreadKilled then putStrLn "APIs closed." else throw e
exceptionHandle _        = error "actionChain0 not ended by killThread. This is unexpected."

-- Working from: https://haskell-servant.readthedocs.io/en/stable/cookbook/testing/Testing.html
-- but also see: https://haskell-servant.readthedocs.io/en/stable/cookbook/using-free-client/UsingFreeClient.html

---
-- Servant QuickCheck testing