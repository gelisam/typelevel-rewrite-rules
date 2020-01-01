module TypeLevel.Lookup where

import GHC.TcPluginM.Extra (lookupModule, lookupName)

-- GHC API
import Module (mkModuleName)
import OccName (mkDataOcc, mkTcOcc)
import TcPluginM (TcPluginM, tcLookupDataCon, tcLookupTyCon)
import DataCon (DataCon)
import TyCon (TyCon)
import qualified FastString


lookupTyCon
  :: String  -- ^ package name
  -> String  -- ^ module name
  -> String  -- ^ type constructor/family name
  -> TcPluginM TyCon
lookupTyCon packageName moduleName tyConName = do
  -- TODO: the calling program might not have 'packageName' in their
  -- dependencies; better print a helpful error message instead of letting GHC
  -- panic!
  module_ <- lookupModule (mkModuleName moduleName)
                          (FastString.fsLit packageName)
  tyConName_ <- lookupName module_ (mkTcOcc tyConName)
  tyCon <- tcLookupTyCon tyConName_
  pure tyCon

lookupDataCon
  :: String  -- ^ package name
  -> String  -- ^ module name
  -> String  -- ^ data constructor name
  -> TcPluginM DataCon
lookupDataCon packageName moduleName dataConName = do
  -- TODO: (bis)
  module_ <- lookupModule (mkModuleName moduleName)
                          (FastString.fsLit packageName)
  dataConName_ <- lookupName module_ (mkDataOcc dataConName)
  dataCon <- tcLookupDataCon dataConName_
  pure dataCon
