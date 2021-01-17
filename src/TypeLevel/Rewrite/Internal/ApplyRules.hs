{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module TypeLevel.Rewrite.Internal.ApplyRules where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Foldable (asum, for_)
import Data.Map (Map)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Traversable
import qualified Data.Map as Map

-- GHC API
import Type (TyVar)

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Substitution (gApply)
import Data.Rewriting.Term (Term(..))
import qualified Data.Rewriting.Substitution.Type as Substitution

import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeNode
import TypeLevel.Rewrite.Internal.TypeRule
import TypeLevel.Rewrite.Internal.TypeSubst
import TypeLevel.Rewrite.Internal.TypeTerm


type Subst = Map TyVar (Term TypeNode TypeEq)

applyRules
  :: Traversable t
  => TypeSubst
  -> [TypeRule]
  -> t TypeTerm
  -> Maybe (TypeRule,t TypeTerm)
applyRules typeSubst rules inputs
  = annotatedTraverseFirst (multiRewrite typeSubst rules) inputs

multiRewrite
  :: TypeSubst
  -> [TypeRule]
  -> TypeTerm
  -> Maybe (TypeRule, TypeTerm)
multiRewrite typeSubst rules input
  = asum
    [ (rule,) <$> singleRewrite typeSubst rule input
    | rule <- rules
    ]

-- >>> singleRewrite (F x (F x y) ~ F x y) [F a (F a b)]
-- Just [F a b]
singleRewrite
  :: TypeSubst
  -> TypeRule
  -> TypeTerm
  -> Maybe TypeTerm
singleRewrite typeSubst rule input@(Fun inputF inputXS)
    = topLevelRewrite typeSubst rule input
  <|> (Fun inputF <$> traverseFirst (singleRewrite typeSubst rule) inputXS)
singleRewrite typeSubst rule input
  = topLevelRewrite typeSubst rule input


-- >>> topLevelRewrite (F x (F x y) ~ F x y) (F a (F a b))
-- Just (F a b)
topLevelRewrite
  :: TypeSubst
  -> TypeRule
  -> TypeTerm
  -> Maybe TypeTerm
topLevelRewrite typeSubst (Rule pattern0 pattern') input0 = do
  subst <- execStateT (go pattern0 input0) Map.empty
  gApply (Substitution.fromMap subst) pattern'
  where
    go
      :: Term TypeNode TyVar
      -> TypeTerm
      -> StateT Subst Maybe ()
    go (Var var) input = do
      subst <- get
      case Map.lookup var subst of
        Nothing -> do
          modify (Map.insert var input)
        Just term -> do
          guard (input == term)
    go (Fun patternF patternXS)
       (Fun inputF inputXS)
       = do
      guard (patternF == inputF)
      guard (length patternXS == length inputXS)
      for_ (zip patternXS inputXS) $ \(pattern, input) -> do
        go pattern input
    go pattern (Var var) = do
      let possibleReplacements = fmap snd
                               . filter ((== var) . fst)
                               $ typeSubst
      asum $ fmap (go pattern) possibleReplacements

-- >>> traverseFirst (\x -> if even x then Just (10 + x) else Nothing) [1,3,5]
-- Nothing
-- >>> traverseFirst (\x -> if even x then Just (10 + x) else Nothing) [1,2,4]
-- Just [1,12,4]
traverseFirst
  :: Traversable t
  => (a -> Maybe a)
  -> t a
  -> Maybe (t a)
traverseFirst f = listToMaybe . traverseAll f

annotatedTraverseFirst
  :: Traversable t
  => (a -> Maybe (annotation, a))
  -> t a
  -> Maybe (annotation, t a)
annotatedTraverseFirst f = listToMaybe . annotatedTraverseAll f

-- >>> traverseAll (\x -> if even x then Just (10 + x) else Nothing) [1,3,5]
-- []
-- >>> traverseAll (\x -> if even x then Just (10 + x) else Nothing) [1,2,4]
-- [[1,12,4], [1,2,14]]
traverseAll
  :: Traversable t
  => (a -> Maybe a)
  -> t a
  -> [t a]
traverseAll f
  = fmap snd
  . annotatedTraverseAll (fmap ((),) . f)

annotatedTraverseAll
  :: Traversable t
  => (a -> Maybe (annotation, a))
  -> t a
  -> [(annotation, t a)]
annotatedTraverseAll f ta = flip evalStateT Nothing $ do
  ta' <- for ta $ \a -> do
    get >>= \case
      Just _ -> do
        -- already picked one
        pure a
      Nothing -> do
        pickIt <- lift [True,False]
        if pickIt
          then do
            (annotation, a) <- lift $ maybeToList $ f a
            put (Just annotation)
            pure a
          else do
            pure a
  maybeAnnotation <- get
  annotation <- lift $ maybeToList maybeAnnotation
  pure (annotation, ta')
