module Core.TypeInfer where

import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Generics.Fixplate (Mu(..))

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text

import qualified Core.TypeLevel as TypeLevel
import qualified Core.TermLevel as TermLevel

type Type = TypeLevel.Type

data Constraint =
    EqualityConstraint TypeLevel.Type TypeLevel.Type
  deriving (Eq, Ord, Show)

data TypeResult =
    TypeResult
      { constraints :: [Constraint]
      , assumptions :: Map TypeLevel.Identifier [Type]
      }
  deriving (Eq, Ord, Show)

instance Monoid TypeResult where
  mempty =
    TypeResult
      { constraints = mempty
      , assumptions = mempty
      }
  mappend a b =
    TypeResult
      { constraints = constraints a `mappend` constraints b
      , assumptions = assumptions a `mappend` assumptions b
      }

data TypeState t m =
    TypeState
      { varId :: Int
      , memo :: Map t m
      }

type TypeCheck t = State (TypeState t (Type, TypeResult)) (Type, TypeResult)

freshVarId :: TypeLevel.Identifier -> State (TypeState t m) Type
freshVarId (TypeLevel.Identifier prefix) =
  do
    v <- State.gets varId
    State.modify $ \s -> s { varId = succ v }
    let name = Text.append prefix (Text.pack (show v))
    return
      ( Fix
        ( TypeLevel.Variable
          ( TypeLevel.Identifier name )
        )
      )

memoizedTC :: Ord c => (c -> TypeCheck c) -> c -> TypeCheck c
memoizedTC f c =
  do
    table <- State.gets memo
    case Map.lookup c table of
      Nothing -> memoize
      Just x -> return x
  where
    memoize =
      do
        r <- f c
        State.modify $ \s -> s { memo = Map.insert c r $ memo s }
        return r
