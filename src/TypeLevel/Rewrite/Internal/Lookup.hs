{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.Internal.Lookup where

import Control.Arrow ((***), first)
import Data.Tuple (swap)

-- GHC API
import GHC (DataCon, TyCon, dataConTyCon)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Iface.Load (cannotFindModule)
import GHC.Tc.Plugin (getTopEnv)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Finder (cannotFindModule)
#endif
#if MIN_VERSION_ghc(9,0,0)
import GHC (Module, ModuleName, mkModuleName)
import GHC.Plugins (mkDataOcc, mkTcOcc)
import GHC.Utils.Panic (panicDoc)
import GHC.Tc.Plugin
  ( FindResult(Found), TcPluginM, findImportedModule, lookupOrig, tcLookupDataCon, tcLookupTyCon
  , unsafeTcPluginTcM
  )
import GHC.Tc.Solver.Monad (getDynFlags)
#else
import Finder (cannotFindModule)
import Module (Module, ModuleName, mkModuleName)
import OccName (mkDataOcc, mkTcOcc)
import Panic (panicDoc)
import TcPluginM
import TcSMonad (getDynFlags)
#endif

#if MIN_VERSION_ghc(9,6,0)
import GHC.Types.PkgQual (PkgQual(..))
#endif

lookupModule
  :: String  -- ^ module name
  -> TcPluginM Module
lookupModule moduleNameStr = do
  let moduleName :: ModuleName
      moduleName = mkModuleName moduleNameStr
#if MIN_VERSION_ghc(9,6,0)
  findImportedModule moduleName NoPkgQual >>= \case
#else
  findImportedModule moduleName Nothing >>= \case
#endif
    Found _ module_ -> do
      pure module_
    findResult -> do
#if MIN_VERSION_ghc(9,2,0)
      hscEnv <- getTopEnv
      panicDoc ("TypeLevel.Lookup.lookupModule " ++ show moduleNameStr)
             $ cannotFindModule hscEnv moduleName findResult
#else
      dynFlags <- unsafeTcPluginTcM getDynFlags
      panicDoc ("TypeLevel.Lookup.lookupModule " ++ show moduleNameStr)
             $ cannotFindModule dynFlags moduleName findResult
#endif

-- 'TcPluginM.lookupM' unfortunately fails with a very unhelpful error message
-- when we look up a name which doesn't exist:
--
--   Can't find interface-file declaration for type constructor or class ModuleName.TypeName
--   Probable cause: bug in .hi-boot file, or inconsistent .hi file
--   Use -ddump-if-trace to get an idea of which file caused the error
--
-- But the true cause isn't a corrupted file, it's simply that the requested
-- name is not in the given module. I don't know how to fix the error message
-- (I can't use 'try' nor 'tryM' because we're in the wrong monad)

lookupTyCon
  :: String  -- ^ module name
  -> String  -- ^ type constructor/family name
  -> TcPluginM TyCon
lookupTyCon moduleNameStr tyConNameStr = do
  module_ <- lookupModule moduleNameStr
  tyConName <- lookupOrig module_ (mkTcOcc tyConNameStr)
  tyCon <- tcLookupTyCon tyConName
  pure tyCon

lookupDataCon
  :: String  -- ^ module name
  -> String  -- ^ data constructor name
  -> TcPluginM DataCon
lookupDataCon moduleNameStr dataConNameStr = do
  module_ <- lookupModule moduleNameStr
  dataConName <- lookupOrig module_ (mkDataOcc dataConNameStr)
  dataCon <- tcLookupDataCon dataConName
  pure dataCon


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
