module Core.Unique where

import Control.Concurrent.Supply (Supply)
import qualified Control.Concurrent.Supply as Supply
import Control.Monad.State (State)
import qualified Control.Monad.State as State

import Data.Generics.Fixplate (Mu(..))
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Core.TermLevel as TermLevel


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


uniqueTerm :: UniqueState TermLevel.Identifier -> TermLevel.Term -> TermLevel.Term
uniqueTerm uniqueState (TermLevel.Term mu) =
  TermLevel.Term (uniqueMuTermF mu uniqueState)


uniqueIdentifier :: TermLevel.Identifier -> Int -> TermLevel.Identifier
uniqueIdentifier (TermLevel.Identifier s) tag =
  TermLevel.Identifier (s ++ "_" ++ show tag)


freshTag :: State (UniqueState id) Int
freshTag = do
  st <- State.get
  let (tag, supply) = Supply.freshId (uniqueSupply st)
  State.put (st { uniqueSupply = supply })
  return tag


freshIdentifiers :: [TermLevel.Identifier] -> UniqueState TermLevel.Identifier -> ([TermLevel.Identifier], UniqueState TermLevel.Identifier)
freshIdentifiers ids uniqueState =
  flip State.runState uniqueState $
    State.forM ids $ \identifier -> do
      tag <- freshTag
      State.modify $ \st ->
        st { uniqueMapping = Map.insert identifier tag (uniqueMapping st) }
      return (uniqueIdentifier identifier tag)


uniqueMuTermFs :: Traversable t => t (Mu TermLevel.TermF) -> UniqueState TermLevel.Identifier -> t (Mu TermLevel.TermF)
uniqueMuTermFs mus =
  State.evalState $
    State.forM mus $ \mu -> do
      state <- State.get
      let (uniqueState', uniqueState'') = splitState state
      State.put uniqueState'
      return $ uniqueMuTermF mu uniqueState''


uniqueOperation :: TermLevel.Operation (Mu TermLevel.TermF) -> UniqueState TermLevel.Identifier -> TermLevel.Operation (Mu TermLevel.TermF)
uniqueOperation opn uniqueState =
  case opn of

    TermLevel.BinaryOperation op left right ->
      let
        (uniqueState', uniqueState'') =
          splitState uniqueState
      in
        TermLevel.BinaryOperation op
          (uniqueMuTermF left uniqueState')
          (uniqueMuTermF right uniqueState'')

    TermLevel.UnaryOperation op arg ->
      TermLevel.UnaryOperation op
        (uniqueMuTermF arg uniqueState)


uniqueMuTermF :: Mu TermLevel.TermF -> UniqueState TermLevel.Identifier -> Mu TermLevel.TermF
uniqueMuTermF (Fix termF) uniqueState =
  Fix $ case termF of

    TermLevel.Constant c ->
      TermLevel.Constant c

    TermLevel.Variable v ->
      case Map.lookup v (uniqueMapping uniqueState) of
        Nothing ->
          TermLevel.Variable v
        Just tag ->
          TermLevel.Variable (uniqueIdentifier v tag)

    TermLevel.Operation opn ->
      TermLevel.Operation (uniqueOperation opn uniqueState)

    TermLevel.Abstraction args body ->
      let
        (args', uniqueState') =
          freshIdentifiers args uniqueState
      in
        TermLevel.Abstraction args'
          (uniqueMuTermF body uniqueState')

    TermLevel.Application body args ->
      let
        (uniqueState', uniqueState'') =
          splitState uniqueState
      in
        TermLevel.Application
          (uniqueMuTermF body uniqueState')
          (uniqueMuTermFs args uniqueState'')

    TermLevel.TypeAbstraction args body ->
      TermLevel.TypeAbstraction args (uniqueMuTermF body uniqueState)

    TermLevel.TypeApplication body args ->
      TermLevel.TypeApplication (uniqueMuTermF body uniqueState) args

    TermLevel.RecordIntroduction mapping ->
      TermLevel.RecordIntroduction (uniqueMuTermFs mapping uniqueState)

    TermLevel.RecordElimination body identifier ->
      TermLevel.RecordElimination
        (uniqueMuTermF body uniqueState)
        identifier
