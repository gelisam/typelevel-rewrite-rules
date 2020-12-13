{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module InstanceConstraints.Laws where

type family F a b

type FLaw a b = F (F a b) b ~ F a b
