{-# LANGUAGE ConstraintKinds, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
module MalformedTypeSynonym.RewriteRules where

import TypeLevel.Append


type Malformed as
  = as ++ '[]
