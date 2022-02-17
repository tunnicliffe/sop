{-# LANGUAGE OverloadedStrings, BlockArguments, DuplicateRecordFields, DataKinds #-}

module OrdManServe
  ( ordManApp
  , OrderData
  , timestampOrderData
  , updateItemStatusUnsafe
  ) where

import OrdManQuery (OrdManAPI, ordManAPI)

import Servant
import Data.Aeson (toJSON, fromJSON)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (catchError)
import Control.Monad (when, unless)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Maybe (isJust, fromJust)

import EditJSON (removeField, lookupField, recoverStringFromJSONField)
import SuppliersManager (notifySuppliersSM, relayUpdatesSM)
import Utils ( CombinedM
             , combinedMServer
             , printM
             , fromResult
             , fromResultM
             , fromHeaderM
             , addAuthCheck1, addAuthCheck2, addAuthCheck3
             )
import Types ( Configs (orderManager)
             , ItemStatus (Created)
             , OrderData (..)
             , Blank (blank)
             , OrderID
             , ItemData (..)
             , GTIN
             , OrderUpdates (orderID)
             )
import CouchDBQuery (putDoc, getDoc, headDoc, deleteDoc)

------
-- Internal constants and functions

timestampOrderData :: OrderData -> IO OrderData
timestampOrderData od = do
  t <- getCurrentTime
  let
    xs         = items od
    xsTagged   = HM.map (\i -> i {statusHistory = [(Created, t)]}) xs
  pure od {items = xsTagged}

{-
couchDBErrorConverter :: CouchDBError -> ServerError
couchDBErrorConverter err = err503 { errBody = fromString $ show err } --TODO: Handle each type of error specifically, change codes

throwCouchDBError = throwError . couchDBErrorConverter
--TODO: type signature

missingDocForgive :: ServerError -> CombinedM (Maybe a)
missingDocForgive err = 
  if errHTTPCode err == 404
    then pure Nothing
    else throwError err
-- For when we go to delete a document that does not exist
-- Lets any other kind of error through unchanged

updateItemInOrderData :: ItemData -> OrderData -> OrderData
updateItemInOrderData item order = order { items = item : xs } where
  xs = filter (\i -> gtin i /= gtin item) (items order)
-- Unsafe internal function
-- TODO Could be faster: Don't need to filter entire list, only up to matching ID item
-}

updateItemStatusUnsafe :: GTIN -> (ItemStatus, UTCTime) -> OrderData -> OrderData
updateItemStatusUnsafe itemID update order = order {items = updatedHM} where
  originalHM   = items order
  originalItem = fromJust $ HM.lookup itemID originalHM
  updatedItem  = originalItem { status = fst update, statusHistory = update : (statusHistory originalItem)}
  updatedHM    = HM.insert itemID updatedItem originalHM  

err404ItemNotFound :: OrderID -> GTIN -> ServerError
err404ItemNotFound orderID gtin = 
  err404 {errBody = fromString $ "Item within order not found:\nOrderID = " ++ show orderID ++ "\nGTIN = " ++ show gtin}
------
-- Handler Functions

ordManServerT :: ServerT OrdManAPI CombinedM
ordManServerT = newOrderAuth
           :<|> checkOrderAuth
           :<|> checkItemAuth
           :<|> statusUpdateAuth
           :<|> deleteOrderAuth
  where

    addAuthOM1 = addAuthCheck1 orderManager
    addAuthOM2 = addAuthCheck2 orderManager
    addAuthOM3 = addAuthCheck3 orderManager

    newOrder :: OrderData -> CombinedM NoContent
    newOrder od = do
      order <- liftIO $ timestampOrderData od
      putDoc (_id order) Nothing $ toJSON order 
      supRes <- notifySuppliersSM order
      -- TODO: Code to catch partially successful order submissions
      -- TODO: Do something directly with the supRes
      --when (orderID supRes /= _id order) (throwError err500 {errBody = "ID mismatch between OM and SM."})
      --let
      --  orderUpdated = order {items = items supRes}
      --  orderUpdatedJSON = removeField "_rev" $ toJSON orderUpdated
      --putDoc (_id orderUpdated) orderUpdatedJSON
      pure NoContent
    newOrderAuth = addAuthOM1 newOrder

    checkOrder :: OrderID -> CombinedM OrderData
    checkOrder orderID = do
      doc <- getResponse <$> getDoc orderID
      fromResultM $ fromJSON doc
    checkOrderAuth = addAuthOM1 checkOrder

    checkItem :: OrderID -> GTIN -> CombinedM ItemData
    checkItem orderID gtin = do 
      doc <- getResponse <$> getDoc orderID
      order <- fromResultM $ fromJSON doc
      let maybeItem = HM.lookup gtin $ items order
      when (null maybeItem) (throwError $ err404ItemNotFound orderID gtin)
      pure $ fromJust maybeItem
    checkItemAuth = addAuthOM2 checkItem

    statusUpdate :: OrderID -> GTIN -> ItemStatus -> CombinedM NoContent
    statusUpdate orderID itemID newStatus = do
      docWithHeader <- getDoc orderID
      order <- fromResultM $ fromJSON $ getResponse docWithHeader
      rev <- fromHeaderM (lookupResponseHeader docWithHeader :: ResponseHeader "ETag" String)
      let maybeItem = HM.lookup itemID $ items order
      when (null maybeItem) (throwError $ err404ItemNotFound orderID itemID)
      let 
        item          = fromJust maybeItem
        currentStatus = status item
      unless (newStatus >= currentStatus) 
        (throwError $ err403 {
          errBody = fromString $ "Invalid (backwards) order-status update: " ++ show currentStatus ++ " to " ++ show newStatus})
      time <- liftIO getCurrentTime
      let updatedOrder = updateItemStatusUnsafe itemID (newStatus, time) order
      putDoc orderID (Just rev) (toJSON updatedOrder)
      pure NoContent
    statusUpdateAuth = addAuthOM3 statusUpdate

    deleteOrder :: OrderID -> CombinedM NoContent
    deleteOrder orderID = do 
      ncWithHeader <- headDoc orderID
      rev <- fromHeaderM (lookupResponseHeader ncWithHeader :: ResponseHeader "ETag" String)
      deleteDoc orderID rev
      pure NoContent
    deleteOrderAuth = addAuthOM1 deleteOrder

ordManServer :: Configs -> Server OrdManAPI
ordManServer = combinedMServer ordManAPI ordManServerT

ordManApp :: Configs -> Application
ordManApp = (serve ordManAPI) . ordManServer