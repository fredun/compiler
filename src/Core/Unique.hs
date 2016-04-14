module Core.Unique where

import Control.Concurrent.Supply (Supply)
import qualified Control.Concurrent.Supply as Supply
import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Generics.Fixplate (Mu(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Syntax.Term (Term)
import qualified Syntax.Term as Term


data UniqueState identifier =
  UniqueState
    { uniqueSupply :: Supply
    , uniqueMapping :: Map identifier Int
    }


newState :: IO (UniqueState id)
newState = do
  supply <- Supply.newSupply
  return (UniqueState supply Map.empty)


splitState :: UniqueState id -> (UniqueState id, UniqueState id)
splitState (UniqueState supply mapping) =
  let (supply', supply'') = Supply.splitSupply supply
  in (UniqueState supply' mapping, UniqueState supply'' mapping)


uniqueIdentifier :: Term.Identifier -> Int -> Term.Identifier
uniqueIdentifier (Term.Identifier s) tag =
  Term.Identifier (s ++ "_" ++ show tag)


freshTag :: State (UniqueState id) Int
freshTag = do
  st <- State.get
  let (tag, supply) = Supply.freshId (uniqueSupply st)
  State.put (st { uniqueSupply = supply })
  return tag


freshIdentifiers :: [Term.Identifier] -> UniqueState Term.Identifier -> ([Term.Identifier], UniqueState Term.Identifier)
freshIdentifiers ids uniqueState =
  flip State.runState uniqueState $
    State.forM ids $ \identifier -> do
      tag <- freshTag
      State.modify $ \st ->
        st { uniqueMapping = Map.insert identifier tag (uniqueMapping st) }
      return (uniqueIdentifier identifier tag)


uniqueTerms :: Traversable t => t Term -> UniqueState Term.Identifier -> t Term
uniqueTerms terms =
  State.evalState $
    State.forM terms $ \term -> do
      state <- State.get
      let (uniqueState', uniqueState'') = splitState state
      State.put uniqueState'
      return $ uniqueTerm term uniqueState''


uniqueOperation :: Term.Operation Term -> UniqueState Term.Identifier -> Term.Operation Term
uniqueOperation opn uniqueState =
  case opn of

    Term.BinaryOperation op left right ->
      let
        (uniqueState', uniqueState'') =
          splitState uniqueState
      in
        Term.BinaryOperation op
          (uniqueTerm left uniqueState')
          (uniqueTerm right uniqueState'')

    Term.UnaryOperation op arg ->
      Term.UnaryOperation op
        (uniqueTerm arg uniqueState)


uniqueTerm :: Term -> UniqueState Term.Identifier -> Term
uniqueTerm (Fix termF) uniqueState =
  Fix $ case termF of

    Term.Constant c ->
      Term.Constant c

    Term.Variable v ->
      case Map.lookup v (uniqueMapping uniqueState) of
        Nothing ->
          Term.Variable v
        Just tag ->
          Term.Variable (uniqueIdentifier v tag)

    Term.Operation opn ->
      Term.Operation (uniqueOperation opn uniqueState)

    Term.Abstraction args body ->
      let
        (args', uniqueState') =
          freshIdentifiers args uniqueState
      in
        Term.Abstraction args'
          (uniqueTerm body uniqueState')

    Term.Application body args ->
      let
        (uniqueState', uniqueState'') =
          splitState uniqueState
      in
        Term.Application
          (uniqueTerm body uniqueState')
          (uniqueTerms args uniqueState'')

    Term.TypeAbstraction args body ->
      Term.TypeAbstraction args (uniqueTerm body uniqueState)

    Term.TypeApplication body args ->
      Term.TypeApplication (uniqueTerm body uniqueState) args

    Term.RecordIntroduction mapping ->
      Term.RecordIntroduction (uniqueTerms mapping uniqueState)

    Term.RecordElimination body identifier ->
      Term.RecordElimination
        (uniqueTerm body uniqueState)
        identifier
