module TypeLevel.Rewrite.Internal.Term where

import Data.Rewriting.Term (Term(Fun))


-- |
-- an expression like @(as ++ '[]) ++ bs@ would be represented as
-- @Fun appendTyCon [Var "as", Fun nilTyCon []@
-- or rather
-- @Fun appendTyCon [Fun starTyCon [], Var "as", Fun nilTyCon [Fun starTyCon []]@
-- because those polymorphic TyCons need to be specialized to the '*' kind

atomTerm
  :: f -> Term f v
atomTerm f
  = Fun f []
