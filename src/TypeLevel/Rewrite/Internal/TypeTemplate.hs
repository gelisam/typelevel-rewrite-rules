{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.Internal.TypeTemplate where

-- GHC API
import GHC.Plugins (TyVar, Type, getTyVar_maybe)

-- term-rewriting API
import Data.Rewriting.Term (Term(Fun, Var))

import TypeLevel.Rewrite.Internal.TypeNode


type TypeTemplate = Term TypeNode TyVar

toTypeTemplate_maybe
  :: Type
  -> Maybe TypeTemplate
toTypeTemplate_maybe (getTyVar_maybe -> Just tyVar)
  = Just . Var $ tyVar
toTypeTemplate_maybe (toTypeNodeApp_maybe -> Just (tyNode, args))
  = Fun tyNode <$> traverse toTypeTemplate_maybe args
toTypeTemplate_maybe _
  = Nothing
