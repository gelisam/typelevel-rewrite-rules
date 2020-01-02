{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
module SamePackage.Append where

type family (++) as bs where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
