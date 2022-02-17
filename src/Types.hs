{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
  ( Configs (..)
  , CoreComponent (..)
  , SupplierAdapter (..)
  , IsAPI (..)
  , Blank (..)
  , OrderData (..)
  , OrderID
  , Shop (..)
  , ItemData (..)
  , GTIN
  , Supplier (..)
  , ItemStatus (..)
  , Deliverer (..)
  , CustomerData (..)
  , CustomerID
  , ContactData (..)
  , Address (..)
  , CouchDBResponse (..)
  , OrderFragment (..)
  , OrderUpdates (..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toEncoding), FromJSON, genericToEncoding, defaultOptions)
import Data.Time.Clock (UTCTime)
import Data.Time.Calendar (Day)
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Data.HashMap.Strict (HashMap, empty)
import Data.Hashable (Hashable)
import Text.Read (readEither)
import Servant.Server (ServerError)
import Servant.Client (BaseUrl)
import Servant.API (ToHttpApiData(..), FromHttpApiData(..), BasicAuthData)
import Network.HTTP.Client (Manager)

import Prelude hiding (id, error)  -- To avoid namespace clashes with CouchDB's response fields

--- Config types

-- Configurations for the system
-- Every API can access this data through 'ask'
data Configs = Configs 
  { database          :: CoreComponent
  , orderManager      :: CoreComponent
  , suppliersManager  :: CoreComponent
  , deliverersManager :: CoreComponent
  , supplierAdapters  :: HashMap Supplier SupplierAdapter
  , deliveryContact   :: ContactData
  , adminContact      :: ContactData
  , connectionManager :: Manager -- Any individual API should only use a single Manager
  } deriving (Show)

data CoreComponent = CoreComponent
  { name              :: String
  , url               :: BaseUrl
  , port              :: Int
  , credentials       :: BasicAuthData
  } deriving (Show)

instance Show Manager where
  show = const "Connection Manager" 

data SupplierAdapter = SupplierAdapter
  { supplier          :: Supplier
  , url               :: BaseUrl
  , port              :: Int
  , credentials       :: BasicAuthData
  } deriving (Show)

class IsAPI a where
  getUrl          :: a -> BaseUrl
  getPort         :: a -> Int 
  getCredentials  :: a -> BasicAuthData

instance IsAPI CoreComponent where
  getUrl          = url 
  getPort         = port 
  getCredentials  = credentials

instance IsAPI SupplierAdapter where
  getUrl          = url 
  getPort         = port 
  getCredentials  = credentials

---

class Blank a where
  blank :: a

instance Blank [a] where
  blank = [] 

instance Blank (HashMap k v) where
  blank = empty

instance Blank (Maybe a) where
  blank = Nothing

data OrderData = OrderData
  { _id       :: OrderID
  , shop      :: Shop
  , items     :: HashMap GTIN ItemData
  , customer  :: CustomerData
  } deriving (Generic, Show)
instance ToJSON OrderData where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON OrderData
instance Blank OrderData where
  blank = OrderData blank blank blank blank  

---

type OrderID = String

data Shop = NoShop
          | TestShop 
          | Shop1 
          | Shop2 
  deriving (Generic, Show, Eq, Ord)
instance ToJSON Shop where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Shop
instance Blank Shop where 
  blank = NoShop

data ItemData = ItemData
  { gtin                  :: GTIN
  , description           :: String
  , quantity              :: Int
  , supplier              :: Supplier 
  , deliverer             :: Maybe Deliverer
  , deliveryDateRequested :: Day
  , deliveryDateEstimated :: Maybe Day
  , status                :: ItemStatus
  , statusHistory         :: [(ItemStatus, UTCTime)]
  } deriving (Generic, Show)
instance ToJSON ItemData where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ItemData

type GTIN = String

data Supplier = TestSupplier 
              | Rave
  deriving (Generic, Show, Eq, Ord)
instance ToJSON Supplier where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Supplier
instance Hashable Supplier

data ItemStatus = Created
                 | SupplierNotified
                 | SupplierConfirmed
                 | WithDeliverer Deliverer
                 | AtWarehouse Warehouse
                 | AtCustomer 
  deriving (Generic, Show, Read, Eq, Ord)
instance ToJSON ItemStatus where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ItemStatus
instance ToHttpApiData ItemStatus where
  toUrlPiece = T.pack . show
instance FromHttpApiData ItemStatus where
  parseUrlPiece = (first T.pack) . readEither . T.unpack

data Deliverer = NoDeliverer
               | TestDeliverer
  deriving (Generic, Show, Read, Eq, Ord)
instance ToJSON Deliverer where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Deliverer 
-- Ord instance is for ItemStatus Ord derivation

data Warehouse = NoWarehouse
               | TestWarehouse
  deriving (Generic, Show, Read, Eq, Ord)
instance ToJSON Warehouse where 
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Warehouse
-- Ord instance is for ItemStatus Ord derivation

data CustomerData = CustomerData
  { customerID      :: CustomerID
  , name            :: String
  , deliveryAddress :: Address
  , emailAddress    :: String
  , phoneNum        :: String
  } deriving (Generic, Show, Eq)
instance ToJSON CustomerData where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CustomerData 
instance Blank CustomerData where
  blank = CustomerData blank blank blank blank blank

type CustomerID = String

data ContactData = ContactData
  { name          :: String
  , phoneNum      :: String
  , emailAddress  :: String
  } deriving (Generic, Show, Eq)
instance FromJSON ContactData

data Address = Address 
  { address1    :: String
  , address2    :: String
  , city        :: String
  , postalCode  :: String
  , county      :: String
  , countryCode :: String
  } deriving (Generic, Show, Eq)
instance ToJSON Address where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Address
instance Blank Address where
  blank = Address blank blank blank blank blank blank

---
{-
data UUIDsResponse = UUIDsResponse
  { uuids :: [String] } deriving (Generic, Show)
instance FromJSON UUIDsResponse
-}
data CouchDBResponse = CouchDBResponse
  { ok  :: Bool
  , id  :: String
  , rev :: String
  } deriving (Generic, Show)
instance ToJSON CouchDBResponse where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CouchDBResponse
{-
data CouchDBError = CouchDBError
  { error  :: String
  , reason :: String
  } deriving (Generic, Show)
instance FromJSON CouchDBError
-- In case we ever want to catch CouchDB errors in the future
-}
data OrderFragment = OrderFragment 
  { orderID  :: OrderID
  , items    :: [ItemData]
  } deriving (Generic, Show)
instance ToJSON OrderFragment where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON OrderFragment 
-- Used to notify individual suppliers

data OrderUpdates = OrderUpdates
  { orderID :: OrderID
  , updates :: [(GTIN, ItemStatus)]
  } deriving (Generic, Show)
instance ToJSON OrderUpdates where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON OrderUpdates 
-- Used to update existing orders

--- Misc

instance ToHttpApiData S8.ByteString where
  toUrlPiece = T.pack . S8.unpack
instance FromHttpApiData S8.ByteString where
  parseUrlPiece = Right . S8.pack . T.unpack

instance Show BasicAuthData where
  show = const "<REDACTED>"