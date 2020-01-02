{-# LANGUAGE LambdaCase, ViewPatterns #-}
module TypeLevel.Rewrite.Lookup where

import Control.Arrow ((***), first)
import Data.Tuple (swap)
import qualified GHC.TcPluginM.Extra as TcPluginM

-- GHC API
import DataCon (DataCon, promoteDataCon)
import DynFlags (getDynFlags)
import Finder (cannotFindModule)
import Module (Module, ModuleName, mkModuleName)
import OccName (mkDataOcc, mkTcOcc)
import Panic (panicDoc)
import TcPluginM
  ( FindResult(Found), TcPluginM, findImportedModule, tcLookupDataCon, tcLookupTyCon
  , unsafeTcPluginTcM
  )
import TyCon (TyCon)


lookupModule
  :: String  -- ^ module name
  -> TcPluginM Module
lookupModule moduleNameStr = do
  let moduleName :: ModuleName
      moduleName = mkModuleName moduleNameStr
  findImportedModule moduleName Nothing >>= \case
    Found _ module_ -> do
      pure module_
    findResult -> do
      dynFlags <- unsafeTcPluginTcM getDynFlags
      panicDoc ("TypeLevel.Lookup.lookupModule " ++ show moduleNameStr)
             $ cannotFindModule dynFlags moduleName findResult

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
  tyConName <- TcPluginM.lookupName module_ (mkTcOcc tyConNameStr)
  tyCon <- tcLookupTyCon tyConName
  pure tyCon

lookupDataCon
  :: String  -- ^ module name
  -> String  -- ^ data constructor name
  -> TcPluginM DataCon
lookupDataCon moduleNameStr dataConNameStr = do
  module_ <- lookupModule moduleNameStr
  dataConName <- TcPluginM.lookupName module_ (mkDataOcc dataConNameStr)
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
  = promoteDataCon <$> lookupDataCon moduleNameStr dataConNameStr
lookupFQN (splitLastDot -> Just (moduleNameStr, tyConNameStr))
  = lookupTyCon moduleNameStr tyConNameStr
lookupFQN fqn
  = error $ "expected " ++ show "ModuleName.TypeName"
         ++ ", got " ++ show fqn
