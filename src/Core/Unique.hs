module Core.Unique where

import Control.Concurrent.Supply (Supply)
import qualified Control.Concurrent.Supply as Supply
import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Generics.Fixplate (Mu(..))
import Data.Map (Map)
import qualified Data.Map as Map

import Syntax.Term (Term(..))
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


uniqueTerm :: UniqueState Term.Identifier -> Term -> Term
uniqueTerm uniqueState (Term mu) =
  Term (uniqueMuTermF mu uniqueState)


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


uniqueMuTermFs :: Traversable t => t (Mu Term.TermF) -> UniqueState Term.Identifier -> t (Mu Term.TermF)
uniqueMuTermFs mus =
  State.evalState $
    State.forM mus $ \mu -> do
      state <- State.get
      let (uniqueState', uniqueState'') = splitState state
      State.put uniqueState'
      return $ uniqueMuTermF mu uniqueState''


uniqueOperation :: Term.Operation (Mu Term.TermF) -> UniqueState Term.Identifier -> Term.Operation (Mu Term.TermF)
uniqueOperation opn uniqueState =
  case opn of

    Term.BinaryOperation op left right ->
      let
        (uniqueState', uniqueState'') =
          splitState uniqueState
      in
        Term.BinaryOperation op
          (uniqueMuTermF left uniqueState')
          (uniqueMuTermF right uniqueState'')

    Term.UnaryOperation op arg ->
      Term.UnaryOperation op
        (uniqueMuTermF arg uniqueState)


uniqueMuTermF :: Mu Term.TermF -> UniqueState Term.Identifier -> Mu Term.TermF
uniqueMuTermF (Fix termF) uniqueState =
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
          (uniqueMuTermF body uniqueState')

    Term.Application body args ->
      let
        (uniqueState', uniqueState'') =
          splitState uniqueState
      in
        Term.Application
          (uniqueMuTermF body uniqueState')
          (uniqueMuTermFs args uniqueState'')

    Term.TypeAbstraction args body ->
      Term.TypeAbstraction args (uniqueMuTermF body uniqueState)

    Term.TypeApplication body args ->
      Term.TypeApplication (uniqueMuTermF body uniqueState) args

    Term.RecordIntroduction mapping ->
      Term.RecordIntroduction (uniqueMuTermFs mapping uniqueState)

    Term.RecordElimination body identifier ->
      Term.RecordElimination
        (uniqueMuTermF body uniqueState)
        identifier
