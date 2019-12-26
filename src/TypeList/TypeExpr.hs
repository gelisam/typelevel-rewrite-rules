{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeList.TypeExpr where

import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty((:|)))

-- GHC API
import TyCon (TyCon)
import Type (Type, eqType, mkTyConApp, mkTyConTy, splitTyConApp_maybe)

import TypeList.Tree


-- 'Type' does not have an 'Eq' instance
eqTypeList
  :: [Type]
  -> [Type]
  -> Bool
eqTypeList [] []
  = True
eqTypeList (t1 : ts1) (t2 : ts2)
  | eqType t1 t2
  = eqTypeList ts1 ts2
eqTypeList _ _
  = False


asArgumentsTo
  :: NonEmpty TyCon
  -> Type
  -> Maybe [Type]
asArgumentsTo (toList -> expectedTyCons) tp = do
  (actualTyCon, args) <- splitTyConApp_maybe tp
  let (actualArgs, remainingArgs) = splitAt (length expectedTyCons - 1) args
  let actualTypes   = mkTyConTy actualTyCon : actualArgs
  let expectedTypes = fmap mkTyConTy expectedTyCons
  guard (eqTypeList actualTypes expectedTypes)
  pure remainingArgs

toArgumentsTo
  :: NonEmpty TyCon
  -> [Type] -> Type
toArgumentsTo (tyCon :| tyCons) tps
  = mkTyConApp tyCon (fmap mkTyConTy tyCons ++ tps)


asBinaryApplication
  :: NonEmpty TyCon
  -> Type
  -> Maybe (Type, Type)
asBinaryApplication tyCons tp = do
  args <- asArgumentsTo tyCons tp
  case args of
    [arg1, arg2] -> pure (arg1, arg2)
    _ -> Nothing

toBinaryApplication
  :: NonEmpty TyCon
  -> Type -> Type -> Type
toBinaryApplication tyCons lhs rhs
  = toArgumentsTo tyCons [lhs, rhs]


asTypeTree
  :: NonEmpty TyCon
  -> Type -> Tree Type
asTypeTree tyCons tp
  = case asBinaryApplication tyCons tp of
      Just (lhs, rhs)
        -> Branch (asTypeTree tyCons lhs)
                  (asTypeTree tyCons rhs)
      Nothing
        -> Leaf tp

toTypeTree
  :: NonEmpty TyCon
  -> Tree Type -> Type
toTypeTree tyCons = \case
  Nil
    -> error "toTypeTree Nil: not yet supported"
  Leaf x
    -> x
  Branch lhs rhs
    -> toBinaryApplication tyCons (toTypeTree tyCons lhs)
                                  (toTypeTree tyCons rhs)
