{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.TypeTerm where

-- GHC API
import TyCon (TyCon)
import Type (Type, mkTyConApp, splitTyConApp_maybe)

-- term-rewriting API
import Data.Rewriting.Term (Term(Fun, Var))

import TypeLevel.Rewrite.TypeEq


type TypeTerm = Term TyCon TypeEq

toTypeTerm
  :: Type -> TypeTerm
toTypeTerm (splitTyConApp_maybe -> Just (tyCon, args))
  = Fun tyCon (fmap toTypeTerm args)
toTypeTerm tp
  = Var (TypeEq tp)

toType
  :: TypeTerm -> Type
toType = \case
  Var x
    -> unTypeEq x
  Fun tyCon args
    -> mkTyConApp tyCon (fmap toType args)
