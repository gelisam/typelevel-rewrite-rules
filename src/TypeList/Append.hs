{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
module TypeList.Append where

type family (++) as bs where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
