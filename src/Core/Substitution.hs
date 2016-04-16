module Core.Substitution where

import Data.Generics.Fixplate (Mu(..))
import qualified Data.Generics.Fixplate as Fix

import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Syntax.Type (TypeF)
import qualified Syntax.Type as Type
import Syntax.Term (TermF)
import qualified Syntax.Term as Term

import Core.Annotation (Annotation, Annotated)
import qualified Core.Annotation as Annotation


type Substitution f id = Map id (Annotated f id)


substituteAnn :: Ord id => Substitution f id -> Annotation id -> Annotation id
substituteAnn subs ann =
  let
    unusedSubIds =
      Set.difference (Map.keysSet subs) (Annotation.freeVars ann)
    usedSubs =
      Set.fold Map.delete subs unusedSubIds
    lessFreeVars =
      Set.difference (Annotation.freeVars ann) (Map.keysSet usedSubs)
    moreFreeVars =
      Set.union lessFreeVars (Set.unions (map (Annotation.freeVars . Fix.attribute) (Map.elems usedSubs)))
  in
    ann { Annotation.freeVars = moreFreeVars }


substituteType :: Ord id => Substitution TypeF id -> Annotated TypeF id -> Annotated TypeF id
substituteType subs (Fix (Fix.Ann ann typeF)) =
  let
    newAnn = substituteAnn subs ann
  in
    case typeF of

      Type.Constant c ->
        Fix (Fix.Ann newAnn (Type.Constant c))

      Type.Variable var ->
        case Map.lookup var subs of
          Just sub ->
            substituteType subs sub
          Nothing ->
            Fix (Fix.Ann newAnn (Type.Variable var))

      Type.Abstraction arg kind body ->
        let
          bodySubs = Map.delete arg subs
        in
          Fix (Fix.Ann newAnn (Type.Abstraction arg kind (substituteType bodySubs body)))

      Type.Application body arg ->
        Fix (Fix.Ann newAnn (Type.Application (substituteType subs body) (substituteType subs arg)))


substituteTerm :: Ord id => Substitution (TermF typeId) id -> Annotated (TermF typeId) id -> Annotated (TermF typeId) id
substituteTerm subs (Fix (Fix.Ann ann termF)) =
  let
    newAnn = substituteAnn subs ann
  in
    case termF of

      Term.Constant constant ->
        Fix (Fix.Ann newAnn (Term.Constant constant))

      Term.Variable var ->
        case Map.lookup var subs of
          Just sub ->
            substituteTerm subs sub
          Nothing ->
            Fix (Fix.Ann newAnn (Term.Variable var))

      Term.Operation opn ->
        Fix (Fix.Ann newAnn (Term.Operation (fmap (substituteTerm subs) opn)))

      Term.Abstraction args body ->
        let
          bodySubs = foldr Map.delete subs args
        in
          Fix (Fix.Ann newAnn (Term.Abstraction args (substituteTerm bodySubs body)))

      Term.Application body args ->
        let
          newBody = substituteTerm subs body
          newArgs = map (substituteTerm subs) args
        in
          Fix (Fix.Ann newAnn (Term.Application newBody newArgs))

      Term.TypeAbstraction args body ->
        Fix (Fix.Ann newAnn (Term.TypeAbstraction args (substituteTerm subs body)))

      Term.TypeApplication body args ->
        Fix (Fix.Ann newAnn (Term.TypeApplication (substituteTerm subs body) args))

      Term.RecordIntroduction mapping ->
        Fix (Fix.Ann newAnn (Term.RecordIntroduction (fmap (substituteTerm subs) mapping)))

      Term.RecordElimination body key ->
        Fix (Fix.Ann newAnn (Term.RecordElimination (substituteTerm subs body) key))
