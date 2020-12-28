{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:InstanceConstraints.Laws.FLaw #-}
module InstanceConstraints.Test where

import InstanceConstraints.Laws


class Foo a where
  foo :: ()

f1 :: forall a b. Foo (F a b) => ()
f1 = foo @(F a (F a b))

f2 :: forall a b. Foo [F a b] => ()
f2 = foo @[F a (F a b)]
