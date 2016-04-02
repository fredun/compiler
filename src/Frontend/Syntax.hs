module Frontend.Syntax where

import Data.Map (Map)
import Data.Text (Text)

data Tuple a = Tuple [a]
  deriving (Eq, Ord, Show)

data Record a = Record (Map String a)
  deriving (Eq, Ord, Show)

data Type =
    TypeReference String
  | TupleType (Tuple Type)
  | RecordType (Record Type)
  deriving (Eq, Ord, Show)

data Binding =
    TypeBinding String Type
  deriving (Eq, Ord, Show)
