{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module TypeLevel.Rewrite (plugin) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Foldable
import Data.Traversable
import GHC.TcPluginM.Extra (evByFiat)

-- GHC API
import Coercion (Role(Representational), mkUnivCo)
import Constraint
  ( CtEvidence(CtWanted), ShadowInfo(WDeriv), TcEvDest(HoleDest), Ct, CtLoc, ctEvExpr, ctEvPred
  , ctEvidence, ctLoc, mkNonCanonical
  )
import GHC (Class, Type)
import Plugins (Plugin(pluginRecompile, tcPlugin), CommandLineOption, defaultPlugin, purePlugin)
import Predicate (EqRel(NomEq), Pred(ClassPred, EqPred), classifyPredType, mkClassPred, mkPrimEqPred)
import TcEvidence (EvExpr, EvTerm, evCast)
import TcPluginM (newCoercionHole, newWanted)
import TcRnTypes
import TcType (TcPredType)
import TyCoRep (UnivCoProvenance(PluginProv))
import TyCon (synTyConDefn_maybe)

import TypeLevel.Rewrite.Internal.Lookup
import TypeLevel.Rewrite.Internal.PrettyPrint
import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeRule
import TypeLevel.Rewrite.Internal.TypeSubst
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

asInstanceConstraint
  :: Ct
  -> Maybe (Class, [Type])
asInstanceConstraint ct = do
  let predTree
        = classifyPredType
        $ ctEvPred
        $ ctEvidence
        $ ct
  case predTree of
    ClassPred typeclass args
      -> pure (typeclass, args)
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
solve rules givens _ wanteds = do
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
      for_ (asEqualityConstraint wanted) $ \(lhs, rhs) -> do
        -- lhs ~ rhs => ...

        let lhsTypeTerm = applyTypeSubst typeSubst $ toTypeTerm lhs
        let rhsTypeTerm = applyTypeSubst typeSubst $ toTypeTerm rhs
        let lhsTypeTerm' = applyRules rules lhsTypeTerm
        let rhsTypeTerm' = applyRules rules rhsTypeTerm

        unless (lhsTypeTerm' == lhsTypeTerm && rhsTypeTerm' == rhsTypeTerm) $ do
          -- lhs' ~ rhs' => ...
          let lhs' = fromTypeTerm lhsTypeTerm'
          let rhs' = fromTypeTerm rhsTypeTerm'

          wanted' <- lift $ toEqualityConstraint lhs' rhs' (ctLoc wanted)

          let replaceCt :: ReplaceCt
              replaceCt = ReplaceCt
                { evidenceOfCorrectness  = evByFiat "TypeLevel.Rewrite" lhs rhs
                , replacedConstraint     = wanted
                , replacementConstraints = [wanted']
                }
          tell [replaceCt]

      for_ (asInstanceConstraint wanted) $ \(typeclass, args) -> do
        -- C a ... => ...

        let argTypeTerms = fmap (applyTypeSubst typeSubst . toTypeTerm) args
        let argTypeTerms' = fmap (applyRules rules) argTypeTerms

        unless (argTypeTerms' == argTypeTerms) $ do
          -- C' a' ... => ...
          let args' = fmap fromTypeTerm argTypeTerms'

          -- co :: C a' ... ~ C a ...
          let co = mkUnivCo
                     (PluginProv "TypeLevel.Rewrite")
                     Representational
                     (mkClassPred typeclass args')
                     (mkClassPred typeclass args)
          evWanted' <- lift $ newWanted (ctLoc wanted)
                                        (mkClassPred typeclass args')
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
