{-# OPTIONS_GHC -fconstraint-solver-iterations=10
                -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:UnwrapIdentity.Rule.UnwrapIdentity #-}
module UnwrapIdentity.Test where

import UnwrapIdentity.Rule
import Data.Functor.Identity


example :: Identity Int -> Int
example x = x
