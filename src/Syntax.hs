module Syntax where

import Data.Map (Map)
import Data.Text (Text)

data Type =
    TypeReference String
  | TupleType [Type]
  | RecordType (Map String Type)
  deriving (Eq, Ord, Show)
