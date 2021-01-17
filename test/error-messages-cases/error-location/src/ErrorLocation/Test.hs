{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:ErrorLocation.Laws.FLaw #-}
module ErrorLocation.Test where

import ErrorLocation.Laws


class Foo a where
  foo :: ()

f :: forall a b. ()
f = foo @(F a (F a b))
