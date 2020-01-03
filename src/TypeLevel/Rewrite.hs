{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module TypeLevel.Rewrite (plugin) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Traversable
import GHC.TcPluginM.Extra (evByFiat)

-- GHC API
import Plugins (Plugin(pluginRecompile, tcPlugin), CommandLineOption, defaultPlugin, purePlugin)
import TcEvidence (EvTerm)
import TcPluginM (TcPluginM, newCoercionHole)
import TcRnTypes
import TcType (TcPredType)
import TyCon (TyCon, isTypeSynonymTyCon, synTyConDefn_maybe)
import Type (EqRel(NomEq), PredTree(EqPred), Type, classifyPredType, mkPrimEqPred)

-- term-rewriting API
import Data.Rewriting.Rule (Rule)

import TypeLevel.Rewrite.Lookup
import TypeLevel.Rewrite.PrettyPrint
import TypeLevel.Rewrite.TypeRule
import TypeLevel.Rewrite.TypeTemplate
import TypeLevel.Rewrite.TypeTerm

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


usage
  :: String  -- ^ expected
  -> String  -- ^ actual
  -> TcPluginM a
usage expected actual
  = error $ "usage:\n"
         ++ "  {-# OPTIONS_GHC -fplugin TypeLevel.Rewrite\n"
         ++ "                  -fplugin-opt=TypeLevel.Rewrite:TypeLevel.Append.RightIdentity\n"
         ++ "                  -fplugin-opt=TypeLevel.Rewrite:TypeLevel.Append.RightAssociative #-}\n"
         ++ "Where 'TypeLevel.Append' is a module containing a type synonym named 'RightIdentity':\n"
         ++ "  type RightIdentity as = (as ++ '[]) ~ as\n"
         ++ "Type expressions which match the left of the '~' will get rewritten to the type\n"
         ++ "expression on the right of the '~'. Be careful not to introduce cycles!\n"
         ++ "\n"
         ++ "expected:\n"
         ++ "  " ++ expected ++ "\n"
         ++ "got:\n"
         ++ "  " ++ actual

lookupTypeRules
  :: [CommandLineOption]
  -> TcPluginM [TypeRule]
lookupTypeRules [] = do
  usage (show ["TypeLevel.Append.RightIdentity", "TypeLevel.Append.RightAssociative"])
        "[]"
lookupTypeRules fullyQualifiedTypeSynonyms = do
  -- ["TypeLevel.Append.RightIdentity", "TypeLevel.Append.RightAssociative"]
  for fullyQualifiedTypeSynonyms $ \fullyQualifiedTypeSynonym -> do
    -- "TypeLevel.Append.RightIdentity"
    case splitLastDot fullyQualifiedTypeSynonym of
      Nothing -> do
        usage (show "TypeLevel.Append.RightIdentity")
              (show fullyQualifiedTypeSynonym)
      Just (moduleNameStr, tyConNameStr) -> do
        -- ("TypeLevel.Append", "RightIdentity")
        tyCon <- lookupTyCon moduleNameStr tyConNameStr  -- FIXME: if tyConNameStr is not found in
                                                         -- the module, the error message is poor
        case synTyConDefn_maybe tyCon of
          Nothing -> do
            usage ("type " ++ pprTyCon tyCon ++ " ... = ...")
                  (pprTyCon tyCon ++ " is not a type synonym")
          Just (_tyVars, definition) -> do
            -- ([TyVar "as"], Type "(as ++ '[]) ~ as")
            case toTypeRule_maybe definition of
              Nothing -> do
                usage "... ~ ..."
                      (pprType definition)
              Just typeRule -> do
                -- Rule (TypeTree "(as ++ '[])")
                --      (TypeTree "as")
                pure typeRule


plugin
  :: Plugin
plugin = defaultPlugin
  { tcPlugin = \args -> Just $ TcPlugin
    { tcPluginInit  = lookupTypeRules args
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
  :: [TypeRule]
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
          let lhs' = fromTypeTerm lhsTypeTerm'
          let rhs' = fromTypeTerm rhsTypeTerm'

          ct' <- lift $ toEqualityConstraint lhs' rhs' (ctLoc ct)

          let replaceCt :: ReplaceCt
              replaceCt = ReplaceCt
                { evidenceOfCorrectness  = evByFiat "TypeLevel.Rewrite" lhs' rhs'
                , replacedConstraint     = ct
                , replacementConstraints = [ct']
                }
          tell [replaceCt]
  pure $ combineReplaceCts replaceCts
