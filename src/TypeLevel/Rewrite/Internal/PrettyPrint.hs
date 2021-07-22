{-# LANGUAGE LambdaCase, RecordWildCards #-}
module TypeLevel.Rewrite.Internal.PrettyPrint where

import Data.List (intercalate)

-- GHC API
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import GHC.Plugins (TyCon)
import GHC.Plugins (TyVar, Type)

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Rules (Reduct(..))
import Data.Rewriting.Term (Term(Fun, Var))

import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeNode
import TypeLevel.Rewrite.Internal.TypeRule
import TypeLevel.Rewrite.Internal.TypeSubst
import TypeLevel.Rewrite.Internal.TypeTemplate
import TypeLevel.Rewrite.Internal.TypeTerm


pprMaybe
  :: (a -> String)
  -> Maybe a
  -> String
pprMaybe pprA = \case
  Nothing
    -> "Nothing"
  Just a
    -> "Just ("
    ++ pprA a
    ++ ")"

pprPair
  :: (a -> String)
  -> (b -> String)
  -> (a, b)
  -> String
pprPair pprA pprB (a, b)
  = "("
 ++ pprA a
 ++ ", "
 ++ pprB b
 ++ ")"

pprList
  :: (a -> String)
  -> [a]
  -> String
pprList pprA
  = ("[" ++)
  . (++ "]")
  . intercalate ", "
  . fmap pprA


pprTyCon
  :: TyCon -> String
pprTyCon
  = showSDocUnsafe . ppr

pprType
  :: Type -> String
pprType
  = showSDocUnsafe . ppr

pprTyVar
  :: TyVar -> String
pprTyVar
  = showSDocUnsafe . ppr


pprTypeEq
  :: TypeEq -> String
pprTypeEq
  = pprType . unTypeEq


pprTerm
  :: (f -> String)
  -> (v -> String)
  -> Term f v
  -> String
pprTerm pprF pprV = \case
  Var v
    -> pprV v
  Fun f args
    -> pprF f
    ++ " "
    ++ pprList (pprTerm pprF pprV) args

pprRule
  :: (f -> String)
  -> (v -> String)
  -> Rule f v
  -> String
pprRule pprF pprV (Rule {..})
  = "Rule "
 ++ "{ lhs = " ++ pprTerm pprF pprV lhs
 ++ ", rhs = " ++ pprTerm pprF pprV rhs
 ++ "}"

pprReduct
  :: (f -> String)
  -> (v -> String)
  -> (v' -> String)
  -> Reduct f v v'
  -> String
pprReduct pprF pprV pprV' (Reduct {..})
  = "Reduct "
 ++ "{ result = " ++ pprTerm pprF pprV result
 ++ ", pos = " ++ show pos
 ++ ", rule = " ++ pprRule pprF pprV' rule
 ++ ", subst = undefined"
 ++ "}"


pprTypeNode
  :: TypeNode -> String
pprTypeNode = \case
  TyCon tyCon
    -> "TyCon ("
    ++ pprTyCon tyCon
    ++ ")"
  TyLit tyLit
    -> "TyLit ("
    ++ pprTypeEq tyLit
    ++ ")"

pprTypeSubst
  :: TypeSubst -> String
pprTypeSubst
  = pprList $ pprPair pprTypeEq pprTypeTerm

pprTypeTemplate
  :: TypeTemplate -> String
pprTypeTemplate
  = pprTerm pprTypeNode pprTyVar

pprTypeTerm
  :: TypeTerm -> String
pprTypeTerm
  = pprTerm pprTypeNode pprTypeEq

pprTypeRule
  :: TypeRule -> String
pprTypeRule
  = pprRule pprTypeNode pprTyVar

pprTypeReduct
  :: Reduct TyCon TypeEq TyVar -> String
pprTypeReduct
  = pprReduct pprTyCon pprTypeEq pprTyVar
