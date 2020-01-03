module TypeLevel.Rewrite.TypeRule where

-- GHC API
import TyCon (TyCon)

-- term-rewriting API
import Data.Rewriting.Rule (Rule(..))
import Data.Rewriting.Rules (Reduct(result), fullRewrite)
import Data.Rewriting.Term (Term(Fun, Var))

import TypeLevel.Rewrite.TypeTerm


type TypeRule = Rule TyCon String

rightIdentityRule
  :: TyCon
  -> TyCon
  -> TypeRule
rightIdentityRule nilTyCon appendTyCon
  = Rule lhs_ rhs_
  where
    nil :: Term TyCon String
    nil = Fun nilTyCon [Var "k"]

    (%) :: Term TyCon String -> Term TyCon String -> Term TyCon String
    x % y = Fun appendTyCon [Var "k", x, y]

    lhs_ :: Term TyCon String
    lhs_ = Var "x" % nil

    rhs_ :: Term TyCon String
    rhs_ = Var "x"

rightAssociativityRule
  :: TyCon
  -> TypeRule
rightAssociativityRule appendTyCon
  = Rule lhs_ rhs_
  where
    (%) :: Term TyCon String -> Term TyCon String -> Term TyCon String
    x % y = Fun appendTyCon [Var "k", x, y]

    lhs_ :: Term TyCon String
    lhs_ = (Var "x" % Var "y") % Var "z"

    rhs_ :: Term TyCon String
    rhs_ = Var "x" % (Var "y" % Var "z")


applyRules
  :: [TypeRule]
  -> TypeTerm
  -> TypeTerm
applyRules rules typeTerm
  = case fullRewrite rules typeTerm of
      []
        -> typeTerm
      reducts
        -> applyRules rules . result . last $ reducts
