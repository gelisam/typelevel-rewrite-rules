{-# LANGUAGE LambdaCase, RecordWildCards #-}
module TypeLevel.Rewrite.PrettyPrint where

import Data.List (intercalate)

-- GHC API
import Outputable (ppr, showSDocUnsafe)
import TyCon (TyCon)
import Type (Type)

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Rules (Reduct(..))
import Data.Rewriting.Term (Term(Fun, Var))

import TypeLevel.Rewrite.TypeEq
import TypeLevel.Rewrite.TypeRule
import TypeLevel.Rewrite.TypeTerm


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


pprTypeTerm
  :: TypeTerm -> String
pprTypeTerm
  = pprTerm pprTyCon pprTypeEq

pprTypeRule
  :: TypeRule -> String
pprTypeRule
  = pprRule pprTyCon show

pprTypeReduct
  :: Reduct TyCon TypeEq String -> String
pprTypeReduct
  = pprReduct pprTyCon pprTypeEq show
