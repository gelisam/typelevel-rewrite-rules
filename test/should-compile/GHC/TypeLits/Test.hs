{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:GHC.TypeLits.RewriteRules.Rule #-}
module GHC.TypeLits.Test where

import GHC.TypeLits
import GHC.TypeLits.RewriteRules


ex1a :: ((1 + n) ~ (n + 1) => r)
     -> proxy n
     -> r
ex1a r _ = r
