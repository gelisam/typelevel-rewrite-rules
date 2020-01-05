{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
module NoPlugin.Test where


type family (++) as bs where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)


ex2e :: proxy as
     -> proxy bs
     -> proxy cs
     -> proxy (as ++ (bs ++ cs))
     -> proxy ((as ++ bs) ++ cs)
ex2e _ _ _ r = r
