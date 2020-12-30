{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module InstanceConstraints.Laws where

type family F a b
type family G a b

type FLaw a b = F a (F a b) ~ F a b
