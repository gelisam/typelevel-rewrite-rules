{-# LANGUAGE CPP, LambdaCase, OverloadedStrings, RecordWildCards, ViewPatterns #-}
module TypeLevel.Rewrite (plugin) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Traversable

-- GHC API
#if MIN_VERSION_ghc(9,6,0)
import GHC.Core.Coercion (Role(Representational), mkUnivCo)
import GHC.Tc.Types.Constraint (CtEvidence(ctev_loc), Ct, ctEvExpr, ctLoc, mkNonCanonical)
import GHC.Plugins (PredType, SDoc, fsep, ppr)
import GHC.Tc.Utils.TcType (eqType)
import GHC.Plugins (Plugin(pluginRecompile, tcPlugin), CommandLineOption, defaultPlugin, purePlugin)
import GHC.Tc.Types.Evidence (EvExpr, EvTerm, evCast)
import GHC.Tc.Plugin (newWanted)
import GHC.Core.TyCo.Rep (UnivCoProvenance(PluginProv))
import GHC.Plugins (synTyConDefn_maybe)
import GHC.Tc.Types (TcPluginSolveResult(..), TcPluginM, ErrCtxt, pushErrCtxtSameOrigin, TcPlugin(..), TcPluginSolver)
import GHC.Types.Unique.FM ( emptyUFM )
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Core.Coercion (Role(Representational), mkUnivCo)
import GHC.Tc.Types.Constraint (CtEvidence(ctev_loc), Ct, ctEvExpr, ctLoc, mkNonCanonical)
import GHC.Plugins (PredType, SDoc, eqType, fsep, ppr)
import GHC.Plugins (Plugin(pluginRecompile, tcPlugin), CommandLineOption, defaultPlugin, purePlugin)
import GHC.Tc.Types.Evidence (EvExpr, EvTerm, evCast)
import GHC.Tc.Plugin (newWanted)
import GHC.Core.TyCo.Rep (UnivCoProvenance(PluginProv))
import GHC.Plugins (synTyConDefn_maybe)
import GHC.Tc.Types (TcPluginResult(..), TcPluginM, ErrCtxt, pushErrCtxtSameOrigin, TcPlugin(..))
#else
import Coercion (Role(Representational), mkUnivCo)
import Constraint (CtEvidence(ctev_loc), Ct, ctEvExpr, ctLoc, mkNonCanonical)
import GhcPlugins (PredType, SDoc, eqType, fsep, ppr)
import Plugins (Plugin(pluginRecompile, tcPlugin), CommandLineOption, defaultPlugin, purePlugin)
import TcEvidence (EvExpr, EvTerm, evCast)
import TcPluginM (newWanted)
import TcRnTypes
import TyCoRep (UnivCoProvenance(PluginProv))
import TyCon (synTyConDefn_maybe)
#endif

import TypeLevel.Rewrite.Internal.ApplyRules
import TypeLevel.Rewrite.Internal.DecomposedConstraint
import TypeLevel.Rewrite.Internal.Lookup
import TypeLevel.Rewrite.Internal.PrettyPrint
import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeRule
import TypeLevel.Rewrite.Internal.TypeTerm

-- printf-debugging:
--import TcPluginM (tcPluginIO)
--import Outputable
----tcPluginIO $ print ("foo", showSDocUnsafe $ ppr foo)


data ReplaceCt = ReplaceCt
  { evidenceOfCorrectness  :: EvTerm
  , replacedConstraint     :: Ct
  , replacementConstraints :: [Ct]
  }


-- See https://gitlab.haskell.org/ghc/ghc/-/commit/9d4ba36f1de7ced62e2c0c6a911411144e9a3b27
-- Change TcPluginResult to TcPluginSolveResult.
combineReplaceCts
  :: [ReplaceCt]
  -> TcPluginSolveResult
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
  usage (show [ "TypeLevel.Append.RightIdentity" :: String
              , "TypeLevel.Append.RightAssociative"
              ])
        "[]"
lookupTypeRules fullyQualifiedTypeSynonyms = do
  -- ["TypeLevel.Append.RightIdentity", "TypeLevel.Append.RightAssociative"]
  for fullyQualifiedTypeSynonyms $ \fullyQualifiedTypeSynonym -> do
    -- "TypeLevel.Append.RightIdentity"
    case splitLastDot fullyQualifiedTypeSynonym of
      Nothing -> do
        usage (show ("TypeLevel.Append.RightIdentity" :: String))
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
    , tcPluginRewrite = \_ -> emptyUFM
    , tcPluginStop  = \_ -> pure ()
    }
  , pluginRecompile = purePlugin
  }


mkErrCtx
  :: SDoc
  -> ErrCtxt
mkErrCtx errDoc = (True, \env -> pure (env, errDoc))

newRuleInducedWanted
  :: Ct
  -> TypeRule
  -> PredType
  -> TcPluginM CtEvidence
newRuleInducedWanted oldCt rule newPredType = do
  let loc = ctLoc oldCt

  -- include the rewrite rule in the error message, if any
  let errMsg = fsep [ "From the typelevel rewrite rule:"
                    , ppr (fromTypeRule rule)
                    ]
  let loc' = pushErrCtxtSameOrigin (mkErrCtx errMsg) loc

  wanted <- newWanted loc' newPredType

  -- ctLoc only copies the "arising from function X" part but not the location
  -- etc., so we need to copy the rest of it manually
  pure $ wanted { ctev_loc = loc' }

solve
  :: [TypeRule]
  -> TcPluginSolver
solve rules = \_ givens wanteds' ->
  case wanteds' of
    [] -> pure (TcPluginOk [] [])
    wanteds -> do
      typeSubst <- execWriterT $ do
        for_ givens $ \given -> do
          for_ (asEqualityConstraint given) $ \(lhs, rhs) -> do
            -- lhs ~ rhs
            -- where lhs is typically an expression and rhs is typically a variable
            let var = TypeEq rhs
            let val = toTypeTerm lhs
            tell [(var, val)]
    
      replaceCts <- execWriterT $ do
        for_ wanteds $ \wanted -> do
          -- wanted => ...
          for_ (asDecomposedConstraint wanted) $ \types -> do
            -- C a b c => ...
    
            -- C a b c
            let typeTerms = fmap toTypeTerm types
            let predType = fromDecomposeConstraint types
    
            for_ (applyRules typeSubst rules typeTerms) $ \(rule, typeTerms') -> do
              -- C a' b' c'
              let types' = fmap fromTypeTerm typeTerms'
              let predType' = fromDecomposeConstraint types'
    
              unless (eqType predType' predType) $ do
                -- co :: C a' b' c'  ~R  C a b c
                let co = mkUnivCo
                           (PluginProv "TypeLevel.Rewrite")
                           Representational
                           predType'
                           predType
                evWanted' <- lift $ newRuleInducedWanted wanted rule predType'
                let wanted' = mkNonCanonical evWanted'
                let futureDict :: EvExpr
                    futureDict = ctEvExpr evWanted'
                let replaceCt :: ReplaceCt
                    replaceCt = ReplaceCt
                      { evidenceOfCorrectness  = evCast futureDict co
                      , replacedConstraint     = wanted
                      , replacementConstraints = [wanted']
                      }
                tell [replaceCt]
      pure $ combineReplaceCts replaceCts
