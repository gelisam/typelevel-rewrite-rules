{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module ErrorLocation.Laws where

type family F a b

type FLaw a b = F a (F a b) ~ F a b
