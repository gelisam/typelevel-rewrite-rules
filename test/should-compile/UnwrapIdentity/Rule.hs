{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module UnwrapIdentity.Rule where

import Data.Functor.Identity

type UnwrapIdentity a
  = Identity a ~ a
