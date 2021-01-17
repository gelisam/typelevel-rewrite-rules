{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module ErrorLocation.Laws where

type family F a b

type FLaw x y = F x (F x y) ~ F x y
