{-# LANGUAGE ViewPatterns #-}
module TypeLevel.Rewrite.TypeRule where

-- GHC API
import Name (getOccString)
import TyCon (TyCon)
import Type (TyVar, Type)

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Rules (Reduct(result), fullRewrite)
import Data.Rewriting.Term (Term(Fun))

import TypeLevel.Rewrite.TypeTemplate
import TypeLevel.Rewrite.TypeTerm


type TypeRule = Rule TyCon TyVar

toTypeRule_maybe
  :: Type
  -> Maybe TypeRule
toTypeRule_maybe (toTypeTemplate_maybe -> Just (Fun (getOccString -> "~") [_type, lhs_, rhs_]))
  = Just (Rule lhs_ rhs_)
toTypeRule_maybe _
  = Nothing

applyRules
  :: [TypeRule]
  -> TypeTerm
  -> TypeTerm
applyRules rules typeTerm
  = case fullRewrite rules typeTerm of
      []
        -> typeTerm
      reducts
        -> applyRules rules . result . last $ reducts
