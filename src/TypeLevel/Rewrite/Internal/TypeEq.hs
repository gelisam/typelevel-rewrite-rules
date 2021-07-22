module TypeLevel.Rewrite.Internal.TypeEq where

import Data.Function

import GHC.Plugins (Type, eqType)


-- | A newtype around 'Type' which has an 'Eq' instance.
newtype TypeEq = TypeEq
  { unTypeEq :: Type
  }

instance Eq TypeEq where
  (==) = eqType `on` unTypeEq
