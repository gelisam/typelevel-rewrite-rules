{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module TypeLevel.Rewrite.Internal.TypeRule where

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins (getOccString)
import GHC.Core.Predicate (mkPrimEqPred)
import GHC.Plugins (TyVar, Type, mkTyVarTy)
#else
import Name (getOccString)
import Predicate (mkPrimEqPred)
import Type (TyVar, Type, mkTyVarTy)
#endif

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Term (Term(..))

import TypeLevel.Rewrite.Internal.TypeNode
import TypeLevel.Rewrite.Internal.TypeTemplate


type TypeRule = Rule TypeNode TyVar

toTypeRule_maybe
  :: Type
  -> Maybe TypeRule
toTypeRule_maybe (toTypeTemplate_maybe -> Just (Fun (TyCon (getOccString -> "~")) [_type, lhs_, rhs_]))
  = Just (Rule lhs_ rhs_)
toTypeRule_maybe _
  = Nothing

fromTyVar
  :: TyVar
  -> Type
fromTyVar
  = mkTyVarTy

fromTerm
  :: (f -> [Type] -> Type)
  -> (v -> Type)
  -> Term f v
  -> Type
fromTerm fromF fromV = \case
  Var v
    -> fromV v
  Fun f args
    -> fromF f (fmap (fromTerm fromF fromV) args)

fromTypeRule
  :: TypeRule
  -> Type
fromTypeRule (Rule lhs rhs)
  = mkPrimEqPred (fromTerm fromTypeNode fromTyVar lhs)
                 (fromTerm fromTypeNode fromTyVar rhs)
