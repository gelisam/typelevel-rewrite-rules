{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module TypeLevel.Rewrite.Internal.TypeRule where

import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Traversable
import qualified Data.List as List
import qualified Data.Map as Map

-- GHC API
import Name (getOccString)
import Type (TyVar, Type)

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Substitution (gApply)
import Data.Rewriting.Term (Term(..))
import qualified Data.Rewriting.Substitution.Type as Substitution

import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeNode
import TypeLevel.Rewrite.Internal.TypeSubst
import TypeLevel.Rewrite.Internal.TypeTemplate
import TypeLevel.Rewrite.Internal.TypeTerm


type TypeRule = Rule TypeNode TyVar

toTypeRule_maybe
  :: Type
  -> Maybe TypeRule
toTypeRule_maybe (toTypeTemplate_maybe -> Just (Fun (TyCon (getOccString -> "~")) [_type, lhs_, rhs_]))
  = Just (Rule lhs_ rhs_)
toTypeRule_maybe _
  = Nothing
