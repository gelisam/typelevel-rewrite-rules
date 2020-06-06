{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies, TypeOperators #-}
module GHC.TypeLits.RewriteRules where

import GHC.TypeLits


type Rule n
  = (1 + n) ~ (n + 1)
