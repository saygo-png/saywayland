{-# LANGUAGE TemplateHaskell, TypeFamilies, FunctionalDependencies, UndecidableInstances #-}
module Saywayland.Protocols.WlrLayerShell where
import Relude
import Protocol
import Saywayland.Types

import Control.Lens
import Data.Binary.Put (runPut)

import Data.Map qualified as Map
import Data.Char (toUpper)

$(loadProtocolFile False "protocols/wlr-layer-shell-unstable-v1.xml")

data Zwlr_layer_shell_v1 = Zwlr_layer_shell_v1 {wlid :: Word32}
makeFieldsId ''Zwlr_layer_shell_v1
data Zwlr_layer_surface_v1 = Zwlr_layer_surface_v1 {wlid :: Word32}
makeFieldsId ''Zwlr_layer_surface_v1

instance DefaultIO Zwlr_layer_shell_v1 where
  defM = pure $ Zwlr_layer_shell_v1 0
instance DefaultIO Zwlr_layer_surface_v1 where
  defM = pure $ Zwlr_layer_surface_v1 0

$(generateTables False (\(x1:xs) -> toUpper x1:xs) "protocols/wlr-layer-shell-unstable-v1.xml")

-- zwlr_layer_shell_v1 {{{
instance Interface' Zwlr_layer_shell_v1 Client where
  type Event Zwlr_layer_shell_v1 = Event_zwlr_layer_shell_v1
  type Request Zwlr_layer_shell_v1 = Request_zwlr_layer_shell_v1
  runEvent shell _ = undefined
  runRequest shell request@Request_zwlr_layer_shell_v1_get_layer_surface{id = layerSurfaceId, surface = surfaceId, output = outputId, layer, namespace} = do
    ClientEnv env <- ask
    sendMessage shell.wlid (getOpcode request) $ runPut $ putEvent nodata request
    let sender = ("zwlr_layer_shell_v1", shell.wlid, "get_layer_surface")
    liftIO
      . strReq sender
      $ mconcat
      [ "newID="
      , show layerSurfaceId
      , " wl_surface="
      , show surfaceId
      , " output="
      , show outputId
      , " layer="
      , show layer
      , " namespace="
      , show namespace
      ]
    modifyIORef env.objects $ Map.insert layerSurfaceId $ Interface Zwlr_layer_surface_v1 {wlid=layerSurfaceId}
  runRequest shell Request_zwlr_layer_shell_v1_destroy = undefined
-- }}}
-- zwlr_layer_surface_v1 {{{
instance Interface' Zwlr_layer_surface_v1 Client where
  type Event Zwlr_layer_surface_v1 = Event_zwlr_layer_surface_v1
  type Request Zwlr_layer_surface_v1 = Request_zwlr_layer_surface_v1
  runEvent ls Event_zwlr_layer_surface_v1_closed = undefined
  runEvent ls Event_zwlr_layer_surface_v1_configure{} = pure ()
  runRequest ls request@Request_zwlr_layer_surface_v1_ack_configure{serial} = do
    sendMessage ls.wlid (getOpcode request) $ runPut $ putEvent nodata request
    let sender = ("zwlr_layer_surface_v1", ls.wlid, "ack_configure")
    liftIO . strReq sender $ "serial=" <> show serial
  runRequest ls request@Request_zwlr_layer_surface_v1_set_anchor{anchor} = do
    sendMessage ls.wlid (getOpcode request) $ runPut $ putEvent nodata request
    let sender = ("zwlr_layer_surface_v1", ls.wlid, "set_anchor")
    liftIO . strReq sender $ "anchor=" <> show anchor
  runRequest ls request@Request_zwlr_layer_surface_v1_set_exclusive_zone{zone} = do
    sendMessage ls.wlid (getOpcode request) $ runPut $ putEvent nodata request
    let sender = ("zwlr_layer_surface_v1", ls.wlid, "set_exclusive_zone")
    liftIO . strReq sender $ "zone=" <> show zone
  runRequest ls request@Request_zwlr_layer_surface_v1_set_size{width, height} = do
    sendMessage ls.wlid (getOpcode request) $ runPut $ putEvent nodata request
    let sender = ("zwlr_layer_surface_v1", ls.wlid, "set_size")
    liftIO . strReq sender $ mconcat ["width=", show width, " height=", show height]
  runRequest ls Request_zwlr_layer_surface_v1_destroy{} = undefined
  runRequest ls Request_zwlr_layer_surface_v1_get_popup{} = undefined
  runRequest ls Request_zwlr_layer_surface_v1_set_keyboard_interactivity{} = undefined
  runRequest ls Request_zwlr_layer_surface_v1_set_layer{} = undefined
  runRequest ls Request_zwlr_layer_surface_v1_set_margin{} = undefined
-- }}}
-- vim: foldmethod=marker
