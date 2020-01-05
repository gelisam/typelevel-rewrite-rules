{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:NonimportedModule.ZRewriteRules.RightIdentity
                -fplugin-opt=TypeLevel.Rewrite:NonimportedModule.ZRewriteRules.RightAssociative #-}
module NonimportedModule.Test where

import TypeLevel.Append


ex2e :: proxy as
     -> proxy bs
     -> proxy cs
     -> proxy (as ++ (bs ++ cs))
     -> proxy ((as ++ bs) ++ cs)
ex2e _ _ _ r = r
