{-# LANGUAGE RecordWildCards #-}
module TypeLevel.Rewrite (plugin) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.TcPluginM.Extra (evByFiat)

-- GHC API
import DataCon
import Plugins (Plugin(pluginRecompile, tcPlugin), CommandLineOption, defaultPlugin, purePlugin)
import TcEvidence (EvTerm)
import TcPluginM (TcPluginM, newCoercionHole)
import TcRnTypes
import TcType (TcPredType)
import TyCon (TyCon)
import Type (EqRel(NomEq), PredTree(EqPred), Type, classifyPredType, mkPrimEqPred)

import TypeLevel.Lookup
import TypeLevel.Tree
import TypeLevel.TypeExpr

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


data RelevantTyCons = RelevantTyCons
  { nilTyCon    :: TyCon
  , appendTyCon :: TyCon
  , starTyCon   :: TyCon
  }

lookupRelevantTyCons
  :: [CommandLineOption]
  -> TcPluginM RelevantTyCons
lookupRelevantTyCons []
    = RelevantTyCons
  <$> (promoteDataCon <$> lookupDataCon "GHC.Types" "[]")
  <*> lookupTyCon "TypeLevel.Append" "++"
  <*> lookupTyCon "GHC.Types" "Type"
lookupRelevantTyCons [nilFQN, appendFQN]
    = RelevantTyCons
  <$> lookupFQN nilFQN
  <*> lookupFQN appendFQN
  <*> lookupTyCon "GHC.Types" "Type"
lookupRelevantTyCons _
    = error "usage: {-# OPTIONS_GHC -fplugin TypeLevel.Rewrite -fplugin-opt='GHC.Types.[] -fplugin-opt=TypeLevel.Append.++ #-}"


plugin
  :: Plugin
plugin = defaultPlugin
  { tcPlugin = \args -> Just $ TcPlugin
    { tcPluginInit  = lookupRelevantTyCons args
    , tcPluginSolve = solve
    , tcPluginStop  = \_ -> pure ()
    }
  , pluginRecompile = purePlugin
  }


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
  :: RelevantTyCons
  -> [Ct]  -- ^ Given constraints
  -> [Ct]  -- ^ Derived constraints
  -> [Ct]  -- ^ Wanted constraints
  -> TcPluginM TcPluginResult
solve _ _ _ [] = do
  pure $ TcPluginOk [] []
solve (RelevantTyCons {..}) _ _ cts = do
  replaceCts <- execWriterT $ do
    for_ cts $ \ct -> do
      -- ct => ...
      for_ (asEqualityConstraint ct) $ \(lhs, rhs) -> do
        -- lhs ~ rhs => ...

        let lhsTree = asTypeTree nilTyCons appendTyCons lhs
        let rhsTree = asTypeTree nilTyCons appendTyCons rhs
        let isCanonicalTree t = isSingletonTree t
                             || isRightAssociativeTree t
        let canonicalize = toRightAssociativeTree . toList
        unless (isCanonicalTree lhsTree && isCanonicalTree rhsTree) $ do
          -- lhs' ~ rhs' => ...
          let lhs' = toTypeTree nilTyCons appendTyCons (canonicalize lhsTree)
          let rhs' = toTypeTree nilTyCons appendTyCons (canonicalize rhsTree)

          ct' <- lift $ toEqualityConstraint lhs' rhs' (ctLoc ct)

          let replaceCt :: ReplaceCt
              replaceCt = ReplaceCt
                { evidenceOfCorrectness  = evByFiat "TypeLevel.Normalize" lhs' rhs'
                , replacedConstraint     = ct
                , replacementConstraints = [ct']
                }
          tell [replaceCt]
  pure $ combineReplaceCts replaceCts
  where
    -- Since @'[]@ is kind-polymorphic, its first argument is '*'
    nilTyCons :: NonEmpty TyCon
    nilTyCons = nilTyCon :| [starTyCon]

    -- Since '++' is kind-polymorphic, its first argument is '*'
    appendTyCons :: NonEmpty TyCon
    appendTyCons = appendTyCon :| [starTyCon]
