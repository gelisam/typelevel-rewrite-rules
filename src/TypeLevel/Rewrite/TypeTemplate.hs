{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.TypeTemplate where

-- GHC API
import TyCon (TyCon)
import Type (TyVar, Type, getTyVar_maybe, splitTyConApp_maybe)

-- term-rewriting API
import Data.Rewriting.Term (Term(Fun, Var))


type TypeTemplate = Term TyCon TyVar

toTypeTemplate_maybe
  :: Type
  -> Maybe TypeTemplate
toTypeTemplate_maybe (getTyVar_maybe -> Just tyVar)
  = Just . Var $ tyVar
toTypeTemplate_maybe (splitTyConApp_maybe -> Just (tyCon, args))
  = Fun tyCon <$> traverse toTypeTemplate_maybe args
toTypeTemplate_maybe _
  = Nothing
