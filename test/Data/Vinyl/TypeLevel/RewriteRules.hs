{-# LANGUAGE ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
module Data.Vinyl.TypeLevel.RewriteRules where

import Data.Vinyl.TypeLevel


type RightIdentity as
  = (as ++ '[]) ~ as
type RightAssociative as bs cs
  = ((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs))
