{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as B
import qualified Xmlbf.Xeno as Xeno
import qualified Xmlbf.XmlHtml as H
import Xmlbf
import PMAdapter (PMOrder, blankPMOrder)

import Data.Either (fromRight)

main :: IO ()
main = do 
  rawXmlBS <- B.readFile "xml-parse-test/Sample.xml"
  let xenoXml = Xeno.fromRawXml rawXmlBS
  let hXml = H.fromRawXml rawXmlBS
  putStrLn "Raw XML:"
  print rawXmlBS
  putStrLn "-----"
  putStrLn "Xeno.DOM parse:"
  print xenoXml 
  putStrLn "-----"
  putStrLn "XmlHtml parse:"
  print hXml
  putStrLn "-----"
  let (order :: PMOrder) = fromRight blankPMOrder $ runParser fromXml $ fromRight [] xenoXml
  putStrLn "Xeno parsed into PMOrder:"
  print order
  putStrLn "-----"
  let orderNodes = toXml order 
  putStrLn "Turned back into Xmlbf nodes:"
  print orderNodes
  putStrLn "-----"