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

f3 :: forall a b x y
    . ( F a b ~ y
      , y ~ x
      , Foo (F a b)
      )
   => ()
f3 = foo @(F a x)

f4 :: forall a b x y
    . ( F a b ~ y
      , y ~ x
      , x ~ y
      , Foo (F a b)
      )
   => ()
f4 = foo @(F a x)

f5 :: forall a b x y
    . ( F a b ~ x
      , y ~ x
      , x ~ y
      , Foo (F a b)
      )
   => ()
f5 = foo @(F a x)

f6 :: forall a b x y
    . ( b ~ G x y
      , y ~ x
      , x ~ y
      , Foo (F a b)
      )
   => ()
f6 = foo @(F a (G x y))
