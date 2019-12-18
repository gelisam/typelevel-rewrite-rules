{-# LANGUAGE ViewPatterns #-}
module TypeList.Normalize (plugin) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.OnError
import Data.Foldable
import GHC.TcPluginM.Extra (lookupModule, lookupName)

-- GHC API
import Module (mkModuleName)
import OccName (mkTcOcc)
import Plugins (Plugin(tcPlugin), defaultPlugin)
import TcPluginM (TcPluginM, tcLookupTyCon)
import TcRnTypes
import TyCon (TyCon)
import Type (EqRel(NomEq), PredTree(EqPred), Type, classifyPredType, eqType, mkTyConTy, splitTyConApp_maybe)
import qualified FastString

-- printf-debugging:
import TcPluginM (tcPluginIO)
import Outputable
--tcPluginIO $ print ("foo", showSDocUnsafe $ ppr foo)


plugin
  :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just $ TcPlugin
    { tcPluginInit  = (,)
                  <$> lookupTyCon "typelist-normalize" "TypeList.Append" "++"
                  <*> lookupTyCon "ghc-prim" "GHC.Types" "Type"
    , tcPluginSolve = solve
    , tcPluginStop  = \_ -> pure ()
    }
  }

lookupTyCon
  :: String  -- ^ package name
  -> String  -- ^ module name
  -> String  -- ^ type constructor/family name
  -> TcPluginM TyCon
lookupTyCon packageName moduleName tyConName = do
  -- TODO: the calling program might not have 'packageName' in their
  -- dependencies; better print a helpful error message instead of letting GHC
  -- panic!
  module_ <- lookupModule (mkModuleName moduleName)
                          (FastString.fsLit packageName)
  tcNm <- lookupName module_ (mkTcOcc tyConName)
  tyCon <- tcLookupTyCon tcNm
  pure tyCon

asEqualityConstraint
  :: Ct
  -> Maybe (Type, Type)
asEqualityConstraint constraint = do
  let predTree
        = classifyPredType
        $ ctEvPred
        $ ctEvidence
        $ constraint
  case predTree of
    EqPred NomEq lhs rhs
      -> pure (lhs, rhs)
    _ -> Nothing

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
  :: [TyCon]
  -> Type
  -> Maybe [Type]
asArgumentsTo expectedTyCons tp = do
  (actualTyCon, args) <- splitTyConApp_maybe tp
  let (actualArgs, remainingArgs) = splitAt (length expectedTyCons - 1) args
  let actualTypes   = mkTyConTy actualTyCon : actualArgs
  let expectedTypes = fmap mkTyConTy expectedTyCons
  guard (eqTypeList actualTypes expectedTypes)
  pure remainingArgs

asBinaryApplication
  :: [TyCon]
  -> Type
  -> Maybe (Type, Type)
asBinaryApplication tyCons tp = do
  args <- asArgumentsTo tyCons tp
  case args of
    [arg1, arg2] -> pure (arg1, arg2)
    _ -> Nothing

asLeftAssociativeApplication
  :: [TyCon]
  -> Type
  -> Maybe (Type, Type, Type)
asLeftAssociativeApplication tyCons args123 = do
  (args12, arg3) <- asBinaryApplication tyCons args123
  (arg1, arg2) <- asBinaryApplication tyCons args12
  pure (arg1, arg2, arg3)

solve
  :: (TyCon, TyCon)  -- ^ (type family (++), kind *)
  -> [Ct]            -- ^ Given constraints
  -> [Ct]            -- ^ Derived constraints
  -> [Ct]            -- ^ Wanted constraints
  -> TcPluginM TcPluginResult
solve _ _ _ [] = do
  pure $ TcPluginOk [] []
solve (appendTyCon, starTyCon) _ _ wantedConstraints
  = onErrorDefault (TcPluginOk [] []) $ do
      for_ wantedConstraints $ \constraint -> do
        -- lhs ~ rhs
        (lhs, rhs) <- onNothingThrowError ()
                    $ asEqualityConstraint constraint
        case lhs of
          (asLeftAssociativeApplication appendTyCons -> Just (arg1, arg2, arg3)) -> do
            lift $ tcPluginIO $ print ("arg1", showSDocUnsafe $ ppr arg1)
            lift $ tcPluginIO $ print ("arg2", showSDocUnsafe $ ppr arg2)
            lift $ tcPluginIO $ print ("arg3", showSDocUnsafe $ ppr arg3)
            pure ()
          _ -> do
            throwE ()
      pure $ TcPluginOk [] []
  where
    -- Since '++' is kind-polymorphic, its first argument is '*'
    appendTyCons :: [TyCon]
    appendTyCons = [appendTyCon, starTyCon]
