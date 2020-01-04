{-# LANGUAGE ConstraintKinds, DataKinds, KindSignatures, TypeFamilies, TypeOperators #-}
module MonoKinds.Append where

import GHC.TypeLits


-- unlike "SamePackage.Append", this version of '(++)' only works with
-- type-level lists of strings
type family (++) (as :: [Symbol]) (bs :: [Symbol]) where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type RightIdentity as
  = (as ++ '[]) ~ as
type RightAssociative as bs cs
  = ((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs))
