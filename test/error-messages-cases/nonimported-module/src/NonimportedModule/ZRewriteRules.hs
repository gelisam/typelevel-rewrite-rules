{-# LANGUAGE ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
-- the 'Z' causes ghc to load the module after 'NonimportedModule.Test'
module NonimportedModule.ZRewriteRules where

import TypeLevel.Append


type RightIdentity as
  = (as ++ '[]) ~ as
type RightAssociative as bs cs
  = ((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs))
