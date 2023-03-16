{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:GHC.TypeLits.RewriteRules.NatRule
                -fplugin-opt=TypeLevel.Rewrite:GHC.TypeLits.RewriteRules.SymbolRule #-}
module GHC.TypeLits.Test where

import GHC.TypeLits
import GHC.TypeLits.RewriteRules ()


ex1 :: ((1 + n) ~ (n + 1) => r)
    -> proxy n
    -> r
ex1 r _ = r

ex2 :: ( ((s1 `AppendSymbol` "foo") `AppendSymbol` s2)
       ~ (s1 `AppendSymbol` ("foo" `AppendSymbol` s2))
      => r
       )
    -> proxy s1
    -> proxy s2
    -> r
ex2 r _ _ = r
