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
import Type (EqRel(NomEq), PredTree(EqPred), Type, classifyPredType, eqType, mkPrimEqPred)
import qualified FastString

import TypeList.Tree
import TypeList.TypeExpr

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
      for_ (asEqualityConstraint ct) $ \(lhs, rhs) -> do
        -- lhs ~ rhs => ...

        let lhsTree = asTypeTree appendTyCons lhs
        let rhsTree = asTypeTree appendTyCons rhs
        let canonicalize = toRightAssociativeTree
        let isAlreadyCanonical t = isSingletonTree t
                                || isRightAssociativeTree t
        unless (isAlreadyCanonical lhsTree && isAlreadyCanonical rhsTree) $ do
          let lhsList = toList lhsTree
          let rhsList = toList rhsTree

          let commonPrefix = takeWhile (uncurry eqType)
                           $ zip lhsList rhsList
          let commonSuffix = reverse
                           $ takeWhile (uncurry eqType)
                           $ zip (reverse lhsList)
                                 (reverse rhsList)

          let simplify = drop (length commonPrefix)
                       . reverse
                       . drop (length commonSuffix)
                       . reverse
          let simplifiedLhsList = simplify lhsList
          let simplifiedRhsList = simplify rhsList

          if null simplifiedLhsList && null simplifiedRhsList
          then do
            let replaceCt :: ReplaceCt
                replaceCt = ReplaceCt
                  { evidenceOfCorrectness  = evByFiat "TypeList.Normalize" lhs rhs
                  , replacedConstraint     = ct
                  , replacementConstraints = []
                  }
            tell [replaceCt]
          else do
            -- lhs' ~ rhs' => ...
            let lhs' = toTypeTree appendTyCons (canonicalize simplifiedLhsList)
            let rhs' = toTypeTree appendTyCons (canonicalize simplifiedRhsList)

            ct' <- lift $ toEqualityConstraint lhs' rhs' (ctLoc ct)

            let replaceCt :: ReplaceCt
                replaceCt = ReplaceCt
                  { evidenceOfCorrectness  = evByFiat "TypeList.Normalize" lhs rhs
                  , replacedConstraint     = ct
                  , replacementConstraints = [ct']
                  }
            tell [replaceCt]
  pure $ combineReplaceCts replaceCts
  where
    -- Since '++' is kind-polymorphic, its first argument is '*'
    appendTyCons :: NonEmpty TyCon
    appendTyCons = appendTyCon :| [starTyCon]
