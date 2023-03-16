{-# LANGUAGE CPP, DeriveFunctor, DeriveFoldable, DeriveTraversable, LambdaCase, RecordWildCards, ViewPatterns #-}
module TypeLevel.Rewrite.Internal.DecomposedConstraint where

import Control.Applicative

-- GHC API
import GHC (Class, Type)
#if MIN_VERSION_ghc(9,0,0)
import GHC.Tc.Types.Constraint (Ct, ctEvPred, ctEvidence)
import GHC.Core.Predicate (EqRel(NomEq), Pred(ClassPred, EqPred), classifyPredType, mkClassPred, mkPrimEqPred)
#else
import Constraint (Ct, ctEvPred, ctEvidence)
import Predicate (EqRel(NomEq), Pred(ClassPred, EqPred), classifyPredType, mkClassPred, mkPrimEqPred)
#endif


data DecomposedConstraint a
  = EqualityConstraint a a        -- lhs ~ rhs
  | InstanceConstraint Class [a]  -- C a b c
  deriving (Functor, Foldable, Traversable)

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

asDecomposedConstraint
  :: Ct
  -> Maybe (DecomposedConstraint Type)
asDecomposedConstraint ct
    = (uncurry EqualityConstraint <$> asEqualityConstraint ct)
  <|> (uncurry InstanceConstraint <$> asInstanceConstraint ct)

fromDecomposeConstraint
  :: DecomposedConstraint Type
  -> Type
fromDecomposeConstraint = \case
  EqualityConstraint t1 t2
    -> mkPrimEqPred t1 t2
  InstanceConstraint cls args
    -> mkClassPred cls args
