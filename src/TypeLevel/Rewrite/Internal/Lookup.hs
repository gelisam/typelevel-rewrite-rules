{-# LANGUAGE CPP, LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.Internal.Lookup where

import Control.Arrow ((***), first)
import Data.Tuple (swap)

import GHC.TcPluginM.Extra (lookupModule, lookupName)

-- GHC API
import GHC (DataCon, TyCon, dataConTyCon)
#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins (mkModuleName, mkTcOcc, mkDataOcc)
import GHC.Tc.Plugin (TcPluginM, tcLookupTyCon, tcLookupDataCon)
import GHC.Data.FastString (fsLit)
#else
import Module (mkModuleName)
import OccName (mkTcOcc, mkDataOcc)
import TcPluginM (TcPluginM, tcLookupTyCon, tcLookupDataCon)
import FastString (fsLit)
#endif

lookupTyCon
  :: String  -- ^ module name
  -> String  -- ^ type constructor/family name
  -> TcPluginM TyCon
lookupTyCon moduleNameStr tyConNameStr = do
  module_ <- lookupModule (mkModuleName moduleNameStr) (fsLit "")
  tyConName <- lookupName module_ (mkTcOcc tyConNameStr)
  tcLookupTyCon tyConName

lookupDataCon
  :: String  -- ^ module name
  -> String  -- ^ data constructor name
  -> TcPluginM DataCon
lookupDataCon moduleNameStr dataConNameStr = do
  module_ <- lookupModule (mkModuleName moduleNameStr) (fsLit "")
  dataConName <- lookupName module_ (mkDataOcc dataConNameStr)
  tcLookupDataCon dataConName


splitFirstDot
  :: String -> Maybe (String, String)
splitFirstDot ('.' : rhs)
  = Just ("", rhs)
splitFirstDot (x : xs)
  = first (x:) <$> splitFirstDot xs
splitFirstDot _
  = Nothing

splitLastDot
  :: String -> Maybe (String, String)
splitLastDot
  = fmap swap
  . fmap (reverse *** reverse)
  . splitFirstDot
  . reverse

-- lookup a Fully-Qualified Name, such as "'GHC.Types.[]" or "TypeLevel.Append.++"
lookupFQN
  :: String
  -> TcPluginM TyCon
lookupFQN ('\'' : (splitLastDot -> Just (moduleNameStr, dataConNameStr)))
  = dataConTyCon <$> lookupDataCon moduleNameStr dataConNameStr
lookupFQN (splitLastDot -> Just (moduleNameStr, tyConNameStr))
  = lookupTyCon moduleNameStr tyConNameStr
lookupFQN fqn
  = error $ "expected " ++ show "ModuleName.TypeName"
         ++ ", got " ++ show fqn
