{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:InstanceConstraints.Laws.FLaw #-}
module InstanceConstraints.Test where

import InstanceConstraints.Laws


class Foo a where
  foo :: ()

f :: forall a b. Foo (F a b) => ()
f = foo @(F a (F a b))
