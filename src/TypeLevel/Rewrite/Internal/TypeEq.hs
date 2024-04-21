{-# LANGUAGE CPP #-}
module TypeLevel.Rewrite.Internal.TypeEq where

import Data.Function

#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins (Type, eqType)
#else
import GhcPlugins (Type, eqType)
#endif


-- | A newtype around 'Type' which has an 'Eq' instance.
newtype TypeEq = TypeEq
  { unTypeEq :: Type
  }

instance Eq TypeEq where
  (==) = eqType `on` unTypeEq
