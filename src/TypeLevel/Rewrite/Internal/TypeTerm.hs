{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.Internal.TypeTerm where

-- GHC API
import Type (Type, mkTyConApp)

-- term-rewriting API
import Data.Rewriting.Term (Term(Fun, Var))

import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeNode


type TypeTerm = Term TypeNode TypeEq

toTypeTerm
  :: Type -> TypeTerm
toTypeTerm (toTypeNodeApp_maybe -> Just (tyNode, args))
  = Fun tyNode (fmap toTypeTerm args)
toTypeTerm tp
  = Var (TypeEq tp)

fromTypeTerm
  :: TypeTerm -> Type
fromTypeTerm = \case
  Var x
    -> unTypeEq x
  Fun (TyCon tyCon) args
    -> mkTyConApp tyCon (fmap fromTypeTerm args)
  Fun (TyLit (TypeEq tyLit)) []
    -> tyLit
  Fun (TyLit _) _
    -> error "impossible: TyLit doesn't have any arguments"
