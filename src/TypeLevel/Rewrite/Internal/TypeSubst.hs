module TypeLevel.Rewrite.Internal.TypeSubst where

import TypeLevel.Rewrite.Internal.TypeEq
import TypeLevel.Rewrite.Internal.TypeTerm


type TypeSubst = [(TypeEq, TypeTerm)]
