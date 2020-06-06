{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies, TypeOperators #-}
module GHC.TypeLits.RewriteRules where

import GHC.TypeLits


type NatRule n
  = (1 + n) ~ (n + 1)

type SymbolRule s1 s2
  = ((s1 `AppendSymbol` "foo") `AppendSymbol` s2)
  ~ (s1 `AppendSymbol` ("foo" `AppendSymbol` s2))
