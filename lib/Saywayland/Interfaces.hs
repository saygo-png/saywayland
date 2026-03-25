{-# LANGUAGE TemplateHaskell #-}
module Saywayland.Interfaces where

import Relude
import Protocol
--import Saywayland.Protocol
import Saywayland.Types
import Language.Haskell.TH (Type(ConT))

import Data.Map qualified as Map
import Data.Binary.Put (runPut)

--data ClientWL_display = ClientWL_display {}
--data ServerWL_display = ServerWL_display {}

-- Interfaces {{{
type role WL_display phantom
data WL_display perspective = WL_display {}
type role WL_registry phantom
data WL_registry perspective = WL_registry {}
-- }}}

data Perspective = Client | Server
type ClientServer a = Either (a Client) (a Server)
data Interface = WLDisplay (ClientServer WL_display) | WLRegistry (ClientServer WL_registry)


type Wayland = WaylandM Interface

$(loadProtocols (ConT ''Wayland) False "protocols")

-- WL_display {{{
wlDisplayID = 1
instance Interface_wl_display (WL_display Client) where
  wl_display_sync callbackId self = {-SEND sync request-} undefined
  wl_display_get_registry registryID self = ask >>= \case
    ServerEnv _ -> undefined 
    ClientEnv env -> do
      modifyIORef env.objects (Map.insert registryID (WLRegistry $ Left WL_registry))
      {- TODO:
      let messageBody = runPut $ put registryID
      sendMessage wlDisplayID 1 messageBody
      liftIO . strReq ("wl_display", wlDisplayID, "get_registry") $ "wl_registry=" <> show registryID
      return $ coerce registryID
      -}
  wl_display_error objectId code message self = {-HANDLE error event-} undefined
  wl_display_delete_id id = {-HANDLE delete_id event-} undefined
 
instance Interface_wl_display (WL_display Server) where
  wl_display_sync callbackId self = {-HANDLE sync request-} undefined
  wl_display_get_registry registryId self = {-HANDLE get_registry request-} undefined
  wl_display_error objectId code message self = {-SEND error event-} undefined
  wl_display_delete_id id = {-SEND delete_id event-} undefined
-- }}}

-- WL_registry {{{
instance Interface_wl_registry (WL_registry Client) where
 
instance Interface_wl_registry (WL_registry Server) where
-- }}}

-- vim: foldmethod=marker
