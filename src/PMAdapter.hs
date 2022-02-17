{-# LANGUAGE DataKinds, 
             TypeOperators, 
             BlockArguments, 
             DuplicateRecordFields, 
             OverloadedStrings #-}

module PMAdapter
  ( pmAdapterApp
  , test1PMA
  , test2PMA
  , PMOrder
  , blankPMOrder
  ) where

import Servant
import Servant.Client

import Types ( Configs (database, deliveryContact, adminContact)
             , OrderData (OrderData)
             , OrderID
             , ItemData (ItemData, deliveryDateRequested, statusHistory)
             , GTIN
             , CustomerData (CustomerData)
             , CustomerID
             , ContactData (ContactData)
             , Address (..)
             )
import Utils (CombinedM, combinedMServer, combinedMClients, createCliEnvFromLookup)

import Servant.XML
import Xmlbf
import Data.HashMap.Strict (HashMap, empty, singleton, elems)
import Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text as Strict (Text, pack)
import Data.Time.Clock (UTCTime, getCurrentTime, utctDay)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Applicative ((<|>))

------
-- Custom Datatypes

data PMOrder = PMOrder 
  { dateIssued            :: Day
  , buyerParty            :: Party
  , deliveryParty         :: Party
  , documentNum           :: OrderID
  , orderDate             :: Day
  , deliveryDateRequested :: Day
  , createdByContact      :: PMContact
  , itemEntries           :: [ItemEntry]
  , remarks               :: Maybe String
  } deriving (Eq, Show)
blankPMOrder = PMOrder day0 blankParty blankParty "" day0 day0 blankPMContact [] Nothing where
  day0 = ModifiedJulianDay 0

pmOrderVerify :: PMOrder -> Bool
pmOrderVerify = (all itemEntryHasID) . itemEntries 

data Party = Party 
  { partycode     :: CustomerID
  , name          :: String
  , regNum        :: Maybe String
  , vatRegNum     :: Maybe String
  , actualAddress :: Maybe Address 
  } deriving (Eq, Show)
blankParty = Party "" "" Nothing Nothing Nothing

data ItemEntry = ItemEntry
  { lineItemNum           :: String
  , gtin                  :: Maybe GTIN -- One of these three
  , sellerItemCode        :: Maybe String -- needs to be present
  , buyerItemCode         :: Maybe String -- for their system
  , itemDescription       :: String
  , baseUnit              :: String
  , amountOrdered         :: Int
  , deliveryDateRequested :: Maybe Day
  , customerRef           :: Maybe String
  } deriving (Eq, Show)

itemEntryHasID :: ItemEntry -> Bool
itemEntryHasID (ItemEntry _ Nothing Nothing Nothing _ _ _ _ _) = False 
itemEntryHasID _                                               = True

data PMContact = PMContact
  { contactFirstName  :: String
  , phoneNum          :: String
  , emailAddress      :: String
  } deriving (Eq, Show)
blankPMContact = PMContact "" "" ""

------
-- Converter functions

orderDataToPMOrder :: OrderData -> CombinedM PMOrder
orderDataToPMOrder (OrderData oID _ xs c) = do
  t       <- liftIO getCurrentTime
  configs <- ask
  let 
    dI  = utctDay t
    bP  = customerDataToParty c 
    dP  = contactDataToParty $ deliveryContact configs
    dN  = oID 
    xsList = elems xs
    oD  = utctDay . snd . last . statusHistory $ head xsList 
    dDR = minimum $ map Types.deliveryDateRequested xsList 
    cBC = contactDataToPMContact $ adminContact configs
    iEs = map itemDataToItemEntry xsList
    r   = Nothing
  pure $ PMOrder dI bP dP dN oD dDR cBC iEs r

customerDataToParty :: CustomerData -> Party
customerDataToParty (CustomerData cID n dA _ _) = 
  Party cID n Nothing Nothing (Just dA)

contactDataToParty :: ContactData -> Party
contactDataToParty (ContactData n _ _) =
  Party "" n Nothing Nothing Nothing 

contactDataToPMContact :: ContactData -> PMContact
contactDataToPMContact (ContactData n pN eA) =
  PMContact n pN eA 

itemDataToItemEntry :: ItemData -> ItemEntry
itemDataToItemEntry (ItemData g des q sup _ dDR dDE sta sH) = 
  ItemEntry "" (Just g) Nothing Nothing des "" q (Just dDR) Nothing

------
-- ToXml instance

--class ToXml a where
--  toXml :: a -> [Node]

text1 :: Show a => a -> Node 
text1 x = case text' . pack $ show x of
  Left err -> error $ "Text node creation failed: " ++ err 
  Right n  -> n

element1 :: Show a => a -> HashMap Strict.Text Strict.Text -> [Node] -> Node 
element1 x hm cn = case element' (Strict.pack $ show x) hm cn of
  Left err -> error $ "Element node creation failed: " ++ err 
  Right n  -> n

instance ToXml PMOrder where
  toXml pmOrder =
    [ text1 "\r\n"
    , element1 "E-Document" empty 
      [ text1 "\r\n\t"
      , element1 "Header" empty 
        [ text1 "\r\n\t\t"
        , element1 "DateIssued" empty 
          [ text1 $ dateIssued pmOrder ]
        , text1 "\r\n\t" 
        ]
      , text1 "\r\n\t"
      , element1 "Document" empty (
        [ text1 "\r\n\t\t"
        , element1 "DocumentType" empty 
          [ text1 "order" ]
        , text1 "\r\n\t\t"
        , element1 "DocumentParties" empty (
          partyElement 3 "BuyerParty" (buyerParty pmOrder) ++
          partyElement 3 "DeliveryParty" (deliveryParty pmOrder) ++
          [ text1 "\r\n\t\t" ] )
        , text1 "\r\n\t\t"
        , element1 "DocumentInfo" empty (
          [ text1 "\r\n\t\t\t"
          , element1 "DocumentNum" empty 
            [ text1 $ documentNum pmOrder]
          , text1 "\r\n\t\t\t"
          , element1 "DateInfo" empty 
            [ text1 "\r\n\t\t\t\t"
            , element1 "OrderDate" empty 
              [ text1 $ orderDate pmOrder]
            , text1 "\r\n\t\t\t\t"
            , element1 "DeliveryDateRequested" empty 
              [ text1 $ deliveryDateRequested (pmOrder :: PMOrder)]
            , text1 "\r\n\t\t\t"
            ]
          ] 
          ++ contactElement 3 (createdByContact pmOrder) ++
          [ text1 "\r\n\t\t" ] )
        , text1 "\r\n\t\t"
        , element1 "DocumentItem" empty 
          ( itemEntryElements 3 (itemEntries pmOrder) ++
          [ text1 "\r\n\t\t"
          ] )
        ] 
        ++ maybeAdditionalInfoElement 2 (remarks pmOrder) ++
        [ text1 "\r\n\t"
        ] )
      , text1 "\r\n"
      ]
    ]

newLineTabs :: Int -> String 
newLineTabs n = "\r\n" ++ (replicate n '\t')

partyElement :: Int -> String -> Party -> [Node]
partyElement n partyName party = 
  [ text1 $ newLineTabs n
  , element1 partyName empty (
    [ text1 $ newLineTabs (n+1)
    , element1 "PartyCode" empty 
      [ text1 $ partycode party]
    , text1 $ newLineTabs (n+1)
    , element1 "Name" empty 
      [ text1 $ name party] ]
    ++ maybeSingleElement (n+1) "RegNum" (regNum party)
    ++ maybeSingleElement (n+1) "VATRegNum" (vatRegNum party)
    ++ maybeAddressElement (n+1) (actualAddress party) ++
    [ text1 $ newLineTabs n ] ) 
  ]

maybeSingleElement :: Show a => Int -> String -> Maybe a -> [Node]
maybeSingleElement _ _    Nothing      = []
maybeSingleElement n name (Just value) = 
  [ text1 $ newLineTabs n
  , element1 name empty [ text1 value ]
  ]

maybeAddressElement :: Int -> Maybe Address -> [Node]
maybeAddressElement _ Nothing  = []
maybeAddressElement n (Just addr) = 
  [ text1 $ newLineTabs n
  , element1 "ContactData" empty 
    [ text1 $ newLineTabs (n+1)
    , element1 "ActualAddress" empty 
      [ text1 $ newLineTabs (n+2)
      , element1 "Address1" empty 
        [ text1 $ address1 addr]
      , text1 $ newLineTabs (n+2)
      , element1 "Address2" empty 
        [ text1 $ address2 addr]
      , text1 $ newLineTabs (n+2)
      , element1 "City" empty 
        [ text1 $ city addr]
      , text1 $ newLineTabs (n+2)
      , element1 "PostalCode" empty 
        [ text1 $ postalCode addr]
      , text1 $ newLineTabs (n+2)
      , element1 "County" empty 
        [ text1 $ county addr]
      , text1 $ newLineTabs (n+2)
      , element1 "CountryCode" empty 
        [ text1 $ countryCode addr]
      , text1 $ newLineTabs (n+1) 
      ]
    , text1 $ newLineTabs n 
    ]
  ]

contactElement :: Int -> PMContact -> [Node]
contactElement n contact = 
  [ text1 $ newLineTabs n
  , element1 "CreatedByContact" empty 
    [ text1 $ newLineTabs (n+1)
    , element1 "ContactFirstName" empty
      [ text1 $ contactFirstName contact ]
    , text1 $ newLineTabs (n+1)
    , element1 "PhoneNum" empty 
      [ text1 $ phoneNum contact ]
    , text1 $ newLineTabs (n+1)
    , element1 "EmailAddress" empty
      [ text1 $ emailAddress contact ]
    , text1 $ newLineTabs n 
    ] 
  ]

itemEntryElements :: Int -> [ItemEntry] -> [Node]
itemEntryElements _ []     = []
itemEntryElements n (x:xs) = 
  [ text1 $ newLineTabs n
  , element1 "ItemEntry" empty (
    [ text1 $ newLineTabs (n+1)
    , element1 "LineItemNum" empty
      [ text1 $ lineItemNum x ] 
    ]
    ++ maybeSingleElement (n+1) "GTIN" (gtin x)
    ++ maybeSingleElement (n+1) "SellerItemCode" (sellerItemCode x)
    ++ maybeSingleElement (n+1) "BuyerItemCode" (buyerItemCode x) ++
    [ text1 $ newLineTabs (n+1)
    , element1 "ItemDescription" empty
      [ text1 $ itemDescription x ]
    , text1 $ newLineTabs (n+1)
    , element1 "BaseUnit" empty
      [ text1 $ baseUnit x ]
    , text1 $ newLineTabs (n+1)
    , element1 "AmountOrdered" empty
      [ text1 $ amountOrdered x ]
    ]
    ++ maybeDateElement (n+1) (deliveryDateRequested (x :: ItemEntry))
    ++ maybeSingleElement (n+1) "CustomerRef" (customerRef x) ++
    [ text1 $ newLineTabs n
    ] )
  ] ++ itemEntryElements n xs

maybeDateElement :: Int -> Maybe Day -> [Node]
maybeDateElement _ Nothing  = []
maybeDateElement n (Just d) = 
  [ text1 $ newLineTabs n
  , element1 "DateInfo" empty
    [ text1 $ newLineTabs (n+1)
    , element1 "DeliveryDateRequested" empty
      [ text1 d ]
    , text1 $ newLineTabs n
    ]
  ]

maybeAdditionalInfoElement :: Int -> Maybe String -> [Node]
maybeAdditionalInfoElement _ Nothing  = []
maybeAdditionalInfoElement n (Just r) = 
  [ text1 $ newLineTabs n
  , element1 "AdditionalInfo" empty
    [ text1 $ newLineTabs (n+1)
    , element1 "Extension" (singleton "extensionId" "remarks")
      [ text1 $ newLineTabs (n+2)
      , element1 "InfoContent" empty
        [ text1 r ]
      , text1 $ newLineTabs (n+1)
      ]
    , text1 $ newLineTabs n
    ]
  ]

------
-- FromXml instance 

--class FromXml 
--  fromXml :: Parser a
{-
instance FromXml PMOrder where
  fromXml = (maybe blankPMOrder id) . nodesToPMOrder <$> pChildren 

nodesToPMOrder :: [Node] -> Maybe PMOrder
nodesToPMOrder [] = Just $ PMOrder "etc"
nodesToPMOrder _  = Nothing

extractPairs :: [Node] -> [(Text, Text)]
extractPairs ns = id -- Function that pattern matches
-}
instance FromXml PMOrder where
  fromXml = do 
    pElement "E-Document" do
      dI <- (read . unpack) <$> (pElement "Header" $ pElement "DateIssued" pText) 
      (bP, dP, dN, oD, dDR, cBC, iEs, r) <- pElement "Document" do 
        pElement "DocumentType" do 
          docTypeCheck <- pText
          unless (docTypeCheck == "order") (fail $ "DocumentType = " ++ (show docTypeCheck))
        (bP, dP) <- pElement "DocumentParties" do 
          bP <- pParty "BuyerParty"
          dP <- pParty "DeliveryParty"
          pure (bP, dP)
        (dN, oD, dDR, cBC) <- pElement "DocumentInfo" do 
          dN <- unpack <$> pElement "DocumentNum" pText
          (oD, dDR) <- pElement "DateInfo" do 
            oD  <- read . unpack <$> pElement "OrderDate" pText
            dDR <- read . unpack <$> pElement "DeliveryDateRequested" pText
            pure (oD, dDR)
          cBC <- pContact
          pure (dN, oD, dDR, cBC)
        iEs <- pElement "DocumentItem" pItemEntries
        r <- pRemarks
        pure (bP, dP, dN, oD, dDR, cBC, iEs, r)
      pure $ PMOrder dI bP dP dN oD dDR cBC iEs r
-- I have ideas of a more flexible parser;
-- One that scans through and collects all single-value elements in a key-value store
-- Now the order of the fields doesn't matter
-- Complicated by the fact that we'd have to store the keys as [Text] for all their parents,
-- otherwise names will clash.

pParty :: String -> Parser Party
pParty partyName = do 
  pElement (Strict.pack partyName) do 
    p <- unpack <$> pElement "PartyCode" pText 
    n <- unpack <$> pElement "Name" pText 
    r <- Just . unpack <$> pElement "RegNum" pText <|> pure Nothing
    v <- Just . unpack <$> pElement "VATRegNum" pText <|> pure Nothing
    a <- Just <$> pAddress <|> pure Nothing 
    pure $ Party p n v r a

pAddress :: Parser Address
pAddress = do 
  pElement "ContactData" do 
    pElement "ActualAddress" do 
      a1 <- unpack <$> pElement "Address1" pText
      a2 <- unpack <$> pElement "Address2" pText
      ci <- unpack <$> pElement "City" pText
      pC <- unpack <$> pElement "PostalCode" pText
      co <- unpack <$> pElement "County" pText
      cC <- unpack <$> pElement "CountryCode" pText
      pure $ Address a1 a2 ci pC co cC

pContact :: Parser PMContact
pContact = do 
  pElement "CreatedByContact" do 
    cFN <- unpack <$> pElement "ContactFirstName" pText
    pN  <- unpack <$> pElement "PhoneNum" pText
    eA  <- unpack <$> pElement "EmailAddress" pText
    pure $ PMContact cFN pN eA    

pItemEntries :: Parser [ItemEntry]
pItemEntries = do 
  x <- pElement "ItemEntry" do 
    lIN <- unpack <$> pElement "LineItemNum" pText
    g   <- Just . unpack <$> pElement "GTIN" pText <|> pure Nothing
    sIC <- Just . unpack <$> pElement "SellerItemCode" pText <|> pure Nothing
    bIC <- Just . unpack <$> pElement "BuyerItemCode" pText <|> pure Nothing
    iD  <- unpack <$> pElement "ItemDescription" pText
    bU  <- unpack <$> pElement "BaseUnit" pText
    aO  <- read . unpack <$> pElement "AmountOrdered" pText
    dDR <- Just . read . unpack <$> pElement "DateInfo" (pElement "DeliveryDateRequested" pText) <|> pure Nothing
    cR  <- Just . unpack <$> pElement "CustomerRef" pText <|> pure Nothing
    pure $ ItemEntry lIN g sIC bIC iD bU aO dDR cR
  xs <- pItemEntries <|> pure [] 
  pure $ x : xs

pRemarks :: Parser (Maybe String)
pRemarks = 
  (Just . unpack) <$> (pElement "AdditionalInfo" $ pElement "Extension" $ pElement "InfoContent" pText) <|> (pure Nothing)

------
-- API Type Declaration

type AdapterAPI = 
       "test1" :> ReqBody '[XML] PMOrder :> Put '[XML] PMOrder
  :<|> "test2" :> ReqBody '[XML] PMOrder :> Post '[XML] PMOrder

pmAdapterAPI :: Proxy AdapterAPI
pmAdapterAPI = Proxy

------
-- Handler Functions

pmAdapterServerT :: ServerT AdapterAPI CombinedM
pmAdapterServerT = test1
              :<|> test2
  
  where

    test1 :: PMOrder -> CombinedM PMOrder
    test1 xml = pure xml

    test2 :: PMOrder -> CombinedM PMOrder
    test2 xml = pure xml

pmAdapterServer :: Configs -> Server AdapterAPI
pmAdapterServer = combinedMServer pmAdapterAPI pmAdapterServerT

pmAdapterApp :: Configs -> Application
pmAdapterApp = (serve pmAdapterAPI) . pmAdapterServer


------
-- Query Functions

test1PMA :: PMOrder -> CombinedM PMOrder

test2PMA :: PMOrder -> CombinedM PMOrder

test1PMA :<|> test2PMA = 
  combinedMClients pmAdapterAPI (createCliEnvFromLookup database) 

------
-- Test Code

-- curl -X PUT -d '{"alpha":1,"beta":2}' -H 'Accept: application/json' -H 'Content-type: application/json' 'http://localhost:8082/notify'
-- curl -X POST -d '{true:1,false:0}' -H 'Content-type: application/json' 'http://localhost:8082/testDispatch'