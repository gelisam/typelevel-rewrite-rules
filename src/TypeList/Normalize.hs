{-# LANGUAGE ViewPatterns #-}
module TypeList.Normalize (plugin) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)

-- GHC API
import Module (mkModuleName)
import OccName (mkTcOcc)
import Plugins (Plugin(pluginRecompile, tcPlugin), defaultPlugin, purePlugin)
import TcEvidence (EvTerm)
import TcPluginM (TcPluginM, newCoercionHole, tcLookupTyCon)
import TcRnTypes
import TcType (TcPredType)
import TyCon (TyCon)
import Type
  ( EqRel(NomEq), PredTree(EqPred), Type, classifyPredType, eqType, mkPrimEqPred, mkTyConApp
  , mkTyConTy, splitTyConApp_maybe
  )
import qualified FastString

-- printf-debugging:
--import TcPluginM (tcPluginIO)
--import Outputable
----tcPluginIO $ print ("foo", showSDocUnsafe $ ppr foo)


data ReplaceCt = ReplaceCt
  { evidenceOfCorrectness  :: EvTerm
  , replacedConstraint     :: Ct
  , replacementConstraints :: [Ct]
  }

combineReplaceCts
  :: [ReplaceCt]
  -> TcPluginResult
combineReplaceCts replaceCts
  = TcPluginOk (fmap solvedConstraint replaceCts)
               (foldMap replacementConstraints replaceCts)
  where
    solvedConstraint :: ReplaceCt -> (EvTerm, Ct)
    solvedConstraint = (,) <$> evidenceOfCorrectness <*> replacedConstraint


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
  , pluginRecompile = purePlugin
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
asEqualityConstraint ct = do
  let predTree
        = classifyPredType
        $ ctEvPred
        $ ctEvidence
        $ ct
  case predTree of
    EqPred NomEq lhs rhs
      -> pure (lhs, rhs)
    _ -> Nothing

toEqualityConstraint
  :: Type -> Type -> CtLoc -> TcPluginM Ct
toEqualityConstraint lhs rhs loc = do
  let tcPredType :: TcPredType
      tcPredType = mkPrimEqPred lhs rhs

  hole <- newCoercionHole tcPredType

  pure $ mkNonCanonical
       $ CtWanted tcPredType (HoleDest hole) WDeriv loc

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

asLeftAssociativeApplication
  :: NonEmpty TyCon
  -> Type
  -> Maybe (Type, Type, Type)
asLeftAssociativeApplication tyCons args123 = do
  (args12, arg3) <- asBinaryApplication tyCons args123
  (arg1, arg2) <- asBinaryApplication tyCons args12
  pure (arg1, arg2, arg3)

toRightAssociativeApplication
  :: NonEmpty TyCon
  -> Type -> Type -> Type -> Type
toRightAssociativeApplication tyCons arg1 arg2 arg3
  = arg1 `op` (arg2 `op` arg3)
  where
    op :: Type -> Type -> Type
    op = toBinaryApplication tyCons


rewrite
  :: Ct
  -> (Type -> Maybe Type)
  -> WriterT [ReplaceCt] TcPluginM ()
rewrite ct f = do
  for_ (asEqualityConstraint ct) $ \(lhs, rhs) -> do
    -- lhs ~ rhs => ...
    case f lhs of
      Just lhs' -> do
        ct' <- lift $ toEqualityConstraint lhs' rhs (ctLoc ct)

        let replaceCt :: ReplaceCt
            replaceCt = ReplaceCt
              { evidenceOfCorrectness  = evByFiat "TypeList.Normalize" lhs' rhs
              , replacedConstraint     = ct
              , replacementConstraints = [ct']
              }
        tell [replaceCt]
      Nothing -> do
        case f rhs of
          Just rhs' -> do
            ct' <- lift $ toEqualityConstraint lhs rhs' (ctLoc ct)

            let replaceCt :: ReplaceCt
                replaceCt = ReplaceCt
                  { evidenceOfCorrectness  = evByFiat "TypeList.Normalize" lhs rhs'
                  , replacedConstraint     = ct
                  , replacementConstraints = [ct']
                  }
            tell [replaceCt]
          Nothing -> do
            pure ()

solve
  :: (TyCon, TyCon)  -- ^ (type family (++), kind *)
  -> [Ct]            -- ^ Given constraints
  -> [Ct]            -- ^ Derived constraints
  -> [Ct]            -- ^ Wanted constraints
  -> TcPluginM TcPluginResult
solve _ _ _ [] = do
  pure $ TcPluginOk [] []
solve (appendTyCon, starTyCon) _ _ cts = do
  replaceCts <- execWriterT $ do
    for_ cts $ \ct -> do
      -- ct => ...
      rewrite ct $ \typeExpr -> do
        -- ... (arg1 ++ arg2) ++ arg3 ...
        (arg1, arg2, arg3) <- asLeftAssociativeApplication appendTyCons typeExpr

        -- ... arg1 ++ (arg2 ++ arg3) ...
        Just $ toRightAssociativeApplication appendTyCons arg1 arg2 arg3
  pure $ combineReplaceCts replaceCts
  where
    -- Since '++' is kind-polymorphic, its first argument is '*'
    appendTyCons :: NonEmpty TyCon
    appendTyCons = appendTyCon :| [starTyCon]
