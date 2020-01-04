{-# LANGUAGE ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
module SamePackage.Append where

type family (++) as bs where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type RightIdentity as
  = (as ++ '[]) ~ as
type RightAssociative as bs cs
  = ((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs))
