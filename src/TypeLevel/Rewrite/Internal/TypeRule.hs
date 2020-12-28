{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module TypeLevel.Rewrite.Internal.TypeRule where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Foldable (asum, for_)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Traversable
import qualified Data.Map as Map

-- GHC API
import Name (getOccString)
import Type (TyVar, Type)

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Substitution (gApply)
import Data.Rewriting.Term (Term(..))
import qualified Data.Rewriting.Substitution.Type as Substitution

import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeNode
import TypeLevel.Rewrite.Internal.TypeTemplate
import TypeLevel.Rewrite.Internal.TypeTerm


type TypeRule = Rule TypeNode TyVar

toTypeRule_maybe
  :: Type
  -> Maybe TypeRule
toTypeRule_maybe (toTypeTemplate_maybe -> Just (Fun (TyCon (getOccString -> "~")) [_type, lhs_, rhs_]))
  = Just (Rule lhs_ rhs_)
toTypeRule_maybe _
  = Nothing

applyRules
  :: [TypeRule]
  -> TypeTerm
  -> TypeTerm
applyRules []
  = id
applyRules rules
  = go (length rules * 100)
  where
    go :: Int -> TypeTerm -> TypeTerm
    go 0 _
      = error "the rewrite rules form a cycle"
    go fuel typeTerm
      = case multiRewrite rules typeTerm of
          Nothing
            -> typeTerm
          Just typeTerm'
            -> go (fuel - 1) typeTerm'

type Subst = Map TyVar (Term TypeNode TypeEq)

multiRewrite
  :: [TypeRule]
  -> TypeTerm
  -> Maybe TypeTerm
multiRewrite rules input
  = asum
    [ singleRewrite rule input
    | rule <- rules
    ]

-- >>> singleRewrite (F x (F x y) ~ F x y) [F a (F a b)]
-- Just [F a b]
singleRewrite
  :: TypeRule
  -> TypeTerm
  -> Maybe TypeTerm
singleRewrite rule input@(Fun inputF inputXS)
    = topLevelRewrite rule input
  <|> zipRewrite inputF inputXS (fmap (singleRewrite rule) inputXS)
singleRewrite rule input
  = topLevelRewrite rule input


-- >>> topLevelRewrite (F x (F x y) ~ F x y) (F a (F a b))
-- Just (F a b)
topLevelRewrite
  :: TypeRule
  -> TypeTerm
  -> Maybe TypeTerm
topLevelRewrite (Rule pattern0 pattern') input0 = do
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
    go _ _ = do
      lift Nothing

-- >>> zipRewrite F [x,y,z] [Nothing,Nothing,Nothing]
-- Nothing
-- >>> zipRewrite F [x,y,z] [Just x',Nothing,Just z']
-- Just [x',y,z']
zipRewrite
  :: TypeNode
  -> [TypeTerm]
  -> [Maybe TypeTerm]
  -> Maybe TypeTerm
zipRewrite f inputXS intermediateXS = do
  guard (any isJust intermediateXS)
  outputXS <- for (zip inputXS intermediateXS) $ \(input, intermediate) -> do
    intermediate <|> pure input
  pure $ Fun f outputXS
