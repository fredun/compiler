module Core.Unique where

import Control.Concurrent.Supply (Supply)
import qualified Control.Concurrent.Supply as Supply
import Control.Monad (forM)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as State

import Data.Generics.Fixplate (Mu(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Syntax.Term (Term)
import qualified Syntax.Term as Term


data UniqueId id = UniqueId Int id
  deriving (Eq, Ord, Show)


newtype UniqueT m a = Unique (StateT Supply m a)
  deriving (Functor, Applicative, Monad)


newSupply :: IO Supply
newSupply =
  Supply.newSupply


runUnique :: UniqueT m a -> Supply -> m (a, Supply)
runUnique (Unique state) =
  State.runStateT state


runUniqueIO :: UniqueT IO a -> IO a
runUniqueIO m = do
  supply <- newSupply
  fst <$> runUnique m supply


unique :: Monad m => id -> UniqueT m (UniqueId id)
unique identifier = Unique $ do
  supply <- State.get
  let (tag, supply') = Supply.freshId supply
  State.put supply'
  return (UniqueId tag identifier)


uniqueOperation :: (Ord id, Monad m) =>
  Map id (UniqueId id) ->
  Term.Operation (Term typeId id) ->
  UniqueT m (Term.Operation (Term typeId (UniqueId id)))
uniqueOperation mapping opn =
  case opn of

    Term.BinaryOperation op left right ->
      Term.BinaryOperation op
        <$> uniqueTerm mapping left
        <*> uniqueTerm mapping right

    Term.UnaryOperation op arg ->
      Term.UnaryOperation op
        <$> uniqueTerm mapping arg


uniqueTerm :: (Ord id, Monad m) =>
  Map id (UniqueId id) ->
  Term typeId id ->
  UniqueT m (Term typeId (UniqueId id))
uniqueTerm mapping (Fix termF) =
  Fix <$> case termF of

    Term.Constant c ->
      pure $ Term.Constant c

    Term.Variable identifier ->
      case Map.lookup identifier mapping of
        Nothing ->
          Term.Variable <$> unique identifier
        Just uniqueIdentifier ->
          pure $ Term.Variable uniqueIdentifier

    Term.Operation opn ->
      Term.Operation
        <$> uniqueOperation mapping opn

    Term.Abstraction args body -> do
      args' <- forM args unique
      let mapping' = Map.unionWith (\_ x -> x) mapping (Map.fromList (zip args args'))
      Term.Abstraction args'
        <$> uniqueTerm mapping' body

    Term.Application body args ->
      Term.Application
        <$> uniqueTerm mapping body
        <*> forM args (uniqueTerm mapping)

    Term.RecordIntroduction recordMapping ->
      Term.RecordIntroduction
        <$> forM recordMapping (uniqueTerm mapping)

    Term.RecordElimination body identifier ->
      Term.RecordElimination
        <$> uniqueTerm mapping body
        <*> pure identifier
