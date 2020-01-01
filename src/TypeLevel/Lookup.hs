{-# LANGUAGE LambdaCase #-}
module TypeLevel.Lookup where

import qualified GHC.TcPluginM.Extra as TcPluginM

-- GHC API
import DataCon (DataCon)
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
