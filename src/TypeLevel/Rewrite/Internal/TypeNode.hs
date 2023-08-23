{-# LANGUAGE ViewPatterns #-}
module TypeLevel.Rewrite.Internal.TypeNode where

-- GHC API
#if MIN_VERSION_ghc(9,0,0)
import GHC (TyCon)
import GHC.Plugins (Type, isNumLitTy, isStrLitTy, mkTyConApp, splitTyConApp_maybe)
#else
import TyCon (TyCon)
import Type (Type, isNumLitTy, isStrLitTy, mkTyConApp, splitTyConApp_maybe)
#endif

import TypeLevel.Rewrite.Internal.TypeEq


-- A 'Type' is a tree whose internal nodes are 'TypeNode's and whose leaves are
-- type variables.
data TypeNode
  = TyCon TyCon
  | TyLit TypeEq
  deriving Eq

toTypeNodeApp_maybe
  :: Type
  -> Maybe (TypeNode, [Type])
toTypeNodeApp_maybe (splitTyConApp_maybe -> Just (tyCon, args))
  = pure (TyCon tyCon, args)
toTypeNodeApp_maybe tyLit@(isNumLitTy -> Just _)
  = pure (TyLit (TypeEq tyLit), [])
toTypeNodeApp_maybe tyLit@(isStrLitTy -> Just _)
  = pure (TyLit (TypeEq tyLit), [])
toTypeNodeApp_maybe _
  = Nothing

fromTypeNode
  :: TypeNode
  -> [Type]
  -> Type
fromTypeNode (TyCon tyCon) args = mkTyConApp tyCon args
fromTypeNode (TyLit (TypeEq tyLit)) _ = tyLit
