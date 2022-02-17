module EditJSON
  ( removeField
  , mergeRecordsLeft
  , lookupField
  , recoverStringFromJSONField
  ) where

import Data.Aeson (Value(Object, String))
import Data.HashMap.Strict (delete, union, lookup)
import Data.Text (pack, unpack)
import Prelude hiding (lookup)

removeField :: String -> Value -> Value
removeField s (Object hm) = Object $ delete (pack s) hm
removeField _ v           = v 

mergeRecordsLeft :: Value -> Value -> Value
mergeRecordsLeft (Object x) (Object y) = Object (union x y)

lookupField :: String -> Value -> Maybe Value
lookupField s (Object hm) = lookup (pack s) hm 

recoverStringFromJSONField :: Value -> String
recoverStringFromJSONField (String t) = unpack t
recoverStringFromJSONField _          = error "recoverStringFromJSONField: JSON field is not of String type"