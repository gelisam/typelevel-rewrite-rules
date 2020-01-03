{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module TypeLevel.Rewrite (plugin) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import GHC.TcPluginM.Extra (evByFiat)

-- GHC API
import Plugins (Plugin(pluginRecompile, tcPlugin), CommandLineOption, defaultPlugin, purePlugin)
import TcEvidence (EvTerm)
import TcPluginM (TcPluginM, newCoercionHole)
import TcRnTypes
import TcType (TcPredType)
import TyCon (TyCon)
import Type (EqRel(NomEq), PredTree(EqPred), Type, classifyPredType, mkPrimEqPred)

-- term-rewriting API
import Data.Rewriting.Rule (Rule)

import TypeLevel.Rewrite.Lookup
import TypeLevel.Rewrite.TypeRule
import TypeLevel.Rewrite.TypeTerm

-- printf-debugging:
--import TcPluginM (tcPluginIO)
--import TypeLevel.Rewrite.PrettyPrint
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
  }

lookupRelevantTyCons
  :: [CommandLineOption]
  -> TcPluginM RelevantTyCons
lookupRelevantTyCons ["(a)SamePackage.Append.++('GHC.Types.[])=a", "((a)SamePackage.Append.++(b))SamePackage.Append.++(c)=(a)SamePackage.Append.++((b)SamePackage.Append.++(c))"]
    = RelevantTyCons
  <$> lookupFQN "'GHC.Types.[]"
  <*> lookupFQN "SamePackage.Append.++"
lookupRelevantTyCons ["(a)Data.Vinyl.TypeLevel.++('GHC.Types.[])=a", "((a)Data.Vinyl.TypeLevel.++(b))Data.Vinyl.TypeLevel.++(c)=(a)Data.Vinyl.TypeLevel.++((b)Data.Vinyl.TypeLevel.++(c))"]
    = RelevantTyCons
  <$> lookupFQN "'GHC.Types.[]"
  <*> lookupFQN "Data.Vinyl.TypeLevel.++"
lookupRelevantTyCons ["(a)TypeLevel.Append.++('GHC.Types.[])=a", "((a)TypeLevel.Append.++(b))TypeLevel.Append.++(c)=(a)TypeLevel.Append.++((b)TypeLevel.Append.++(c))"]
    = RelevantTyCons
  <$> lookupFQN "'GHC.Types.[]"
  <*> lookupFQN "TypeLevel.Append.++"
lookupRelevantTyCons commandLineOptions
    = error $ "usage: {-# OPTIONS_GHC -fplugin TypeLevel.Rewrite\n"
           ++ "                       -fplugin-opt=TypeLevel.Rewrite:'GHC.Types.[]\n"
           ++ "                       -fplugin-opt=TypeLevel.Rewrite:TypeLevel.Append.++ #-}\n"
           ++ "expected: " ++ show ["'GHC.Types.[]", "TypeLevel.Append.++"] ++ "\n"
           ++ "got: " ++ show commandLineOptions

monoidRules
  :: RelevantTyCons
  -> [Rule TyCon String]
monoidRules (RelevantTyCons {..})
  = [ rightIdentityRule nilTyCon appendTyCon
    , rightAssociativityRule appendTyCon
    ]


plugin
  :: Plugin
plugin = defaultPlugin
  { tcPlugin = \args -> Just $ TcPlugin
    { tcPluginInit  = monoidRules <$> lookupRelevantTyCons args
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
  :: [Rule TyCon String]
  -> [Ct]  -- ^ Given constraints
  -> [Ct]  -- ^ Derived constraints
  -> [Ct]  -- ^ Wanted constraints
  -> TcPluginM TcPluginResult
solve _ _ _ [] = do
  pure $ TcPluginOk [] []
solve rules _ _ cts = do
  replaceCts <- execWriterT $ do
    for_ cts $ \ct -> do
      -- ct => ...
      for_ (asEqualityConstraint ct) $ \(lhs, rhs) -> do
        -- lhs ~ rhs => ...

        let lhsTypeTerm = toTypeTerm lhs
        let rhsTypeTerm = toTypeTerm rhs
        let lhsTypeTerm' = applyRules rules lhsTypeTerm
        let rhsTypeTerm' = applyRules rules rhsTypeTerm

        unless (lhsTypeTerm' == lhsTypeTerm && rhsTypeTerm' == rhsTypeTerm) $ do
          -- lhs' ~ rhs' => ...
          let lhs' = toType lhsTypeTerm'
          let rhs' = toType rhsTypeTerm'

          ct' <- lift $ toEqualityConstraint lhs' rhs' (ctLoc ct)

          let replaceCt :: ReplaceCt
              replaceCt = ReplaceCt
                { evidenceOfCorrectness  = evByFiat "TypeLevel.Rewrite" lhs' rhs'
                , replacedConstraint     = ct
                , replacementConstraints = [ct']
                }
          tell [replaceCt]
  pure $ combineReplaceCts replaceCts
