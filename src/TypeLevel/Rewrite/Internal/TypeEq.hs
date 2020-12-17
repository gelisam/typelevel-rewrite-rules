module TypeLevel.Rewrite.Internal.TypeEq where

import Data.Function

import GhcPlugins (Type, eqType)


-- | A newtype around 'Type' which has an 'Eq' instance.
newtype TypeEq = TypeEq
  { unTypeEq :: Type
  }

instance Eq TypeEq where
  (==) = eqType `on` unTypeEq
