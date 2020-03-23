{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.Internal.TypeSubst where

-- term-rewriting API
import Data.Rewriting.Term (Term(Fun, Var))

import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeTerm


type TypeSubst = [(TypeEq, TypeTerm)]

applyTypeSubst
  :: TypeSubst
  -> TypeTerm
  -> TypeTerm
applyTypeSubst []
  = id
applyTypeSubst subst
  = go (length subst * 100)
  where
    go :: Int -> TypeTerm -> TypeTerm
    go 0 _
      = error "the substitution forms a cycle"
    go fuel typeTerm
      = let typeTerm' = applyTypeSubst1 typeTerm
        in if typeTerm' == typeTerm
           then typeTerm
           else go (fuel - 1) typeTerm'

    applyTypeSubst1 :: TypeTerm -> TypeTerm
    applyTypeSubst1 = \case
      Var var
        -> case lookup var subst of
             Just replacement
               -> replacement
             Nothing
               -> Var var
      Fun tyCon args
        -> Fun tyCon (fmap applyTypeSubst1 args)
