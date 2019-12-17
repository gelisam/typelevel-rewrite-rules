module TypeList.Normalize (plugin) where

import GHC.TcPluginM.Extra (lookupModule, lookupName)
import Module (mkModuleName)
import OccName (mkTcOcc)
import Plugins (Plugin(..), defaultPlugin)
import TcPluginM (TcPluginM, tcLookupTyCon)
import TcRnTypes (TcPlugin(..), TcPluginResult(..), Ct)
import TyCon (TyCon)
import qualified FastString

-- printf-debugging:
--   import TcPluginM (tcPluginIO)
--   import Outputable
--   tcPluginIO $ print ("foo", showSDocUnsafe $ ppr foo)


plugin
  :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just $ TcPlugin
    { tcPluginInit  = lookupTyCon
                        "typelist-normalize"
                        "TypeList.Append"
                        "++"
    , tcPluginSolve = solve
    , tcPluginStop  = \_ -> pure ()
    }
  }

lookupTyCon
  :: String  -- ^ package name
  -> String  -- ^ module name
  -> String  -- ^ type constructor/family name
  -> TcPluginM TyCon
lookupTyCon packageName moduleName tyConName = do
  module_ <- lookupModule (mkModuleName moduleName)
                          (FastString.fsLit packageName)
  tcNm <- lookupName module_ (mkTcOcc tyConName)
  tyCon <- tcLookupTyCon tcNm
  pure tyCon

solve
  :: TyCon  -- ^ (++)
  -> [Ct]   -- ^ Given constraints
  -> [Ct]   -- ^ Derived constraints
  -> [Ct]   -- ^ Wanted constraints
  -> TcPluginM TcPluginResult
solve _ _ _ _ = do
  pure $ TcPluginOk [] []
