{-# LANGUAGE ConstraintKinds, DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:SameModule.Test.LeftIdentity #-}
module SameModule.Test where


type family (++) as bs where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type LeftIdentity as
  = (as ++ '[]) ~ as
