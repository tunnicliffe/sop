module Main where

import Control.Concurrent (forkIO)
import System.Directory (createDirectoryIfMissing)

import Types (Configs (..), Supplier (..))
import Utils (runApp, parseConfigs, lookupSupplierAdapter)

import SuppliersManager (supManApp)
import OrdManServe (ordManApp)
import TestSupplier (testSupApp)

main :: IO ()
main = do
  configs <- parseConfigs "configs/configs.yaml"
  createDirectoryIfMissing True "logs"
  forkIO $ runApp (Just "logs/OrderManager.log") configs orderManager ordManApp
  forkIO $ runApp (Just "logs/SuppliersManager.log") configs suppliersManager supManApp
  forkIO $ runApp (Just "logs/TestSupplierAdapter.log") configs (lookupSupplierAdapter TestSupplier) testSupApp
  pure ()