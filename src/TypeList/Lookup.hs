module TypeList.Lookup where

import GHC.TcPluginM.Extra (lookupModule, lookupName)

-- GHC API
import Module (mkModuleName)
import OccName (mkTcOcc)
import TcPluginM (TcPluginM, tcLookupTyCon)
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
  tcNm <- lookupName module_ (mkTcOcc tyConName)
  tyCon <- tcLookupTyCon tcNm
  pure tyCon
