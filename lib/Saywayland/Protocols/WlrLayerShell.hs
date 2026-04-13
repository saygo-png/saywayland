{-# LANGUAGE TemplateHaskell, TypeFamilies, FunctionalDependencies, UndecidableInstances #-}
module Saywayland.Protocols.WlrLayerShell where
import Relude
import Protocol
import Saywayland.Types

import Language.Haskell.TH
import Control.Lens
import Data.Binary.Put (runPut)

import Data.Map qualified as Map

$(loadProtocolFile (ConT ''WaylandM) False "protocols/wlr-layer-shell-unstable-v1.xml")

data Zwlr_layer_shell_v1 = Zwlr_layer_shell_v1 {wlid :: Word32}
makeFieldsId ''Zwlr_layer_shell_v1
data Zwlr_layer_surface_v1 = Zwlr_layer_surface_v1 {wlid :: Word32}
makeFieldsId ''Zwlr_layer_surface_v1


-- zwlr_layer_shell_v1 {{{
instance (
    Protocol_wlr_layer_shell_unstable_v1 i
  , HasWlid (Type_zwlr_layer_surface_v1 i) Word32
  ) => Interface_zwlr_layer_shell_v1 Zwlr_layer_shell_v1 i Client where
  -- requests {{{
  zwlr_layer_shell_v1_get_layer_surface layerSurfaceId surfaceId outputId layer namespace self =
    do
    ClientEnv env <- ask
    let body = runPut $ zwlr_layer_shell_v1_get_layer_surfaceBuilder nodata layerSurfaceId surfaceId outputId layer namespace
    sendMessage self.wlid zwlr_layer_shell_v1_get_layer_surfaceOpcode body

    let sender = ("zwlr_layer_shell_v1", self.wlid, "get_layer_surface")
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
    
    layersurface' <- liftIO $ get_zwlr_layer_surface_v1 (Proxy @i)
    let layersurface = layersurface' & wlid .~ layerSurfaceId
    modifyIORef env.objects $ Map.insert layerSurfaceId $ pack_zwlr_layer_surface_v1 layersurface
  zwlr_layer_shell_v1_destroy = undefined
  -- }}}
-- }}}
-- zwlr_layer_surface_v1 {{{
instance Interface_zwlr_layer_surface_v1 Zwlr_layer_surface_v1 i Client where
  -- requests {{{
  zwlr_layer_surface_v1_set_size width height self = do
    let body = runPut $ zwlr_layer_surface_v1_set_sizeBuilder nodata width height
    sendMessage self.wlid zwlr_layer_surface_v1_set_sizeOpcode body

    let sender = ("zwlr_layer_surface_v1", self.wlid, "set_size")
    liftIO . strReq sender $ mconcat ["width=", show width, " height=", show height]
  zwlr_layer_surface_v1_set_anchor anchor self = do
    let body = runPut $ zwlr_layer_surface_v1_set_anchorBuilder nodata anchor
    sendMessage self.wlid zwlr_layer_surface_v1_set_anchorOpcode body
    let sender = ("zwlr_layer_surface_v1", self.wlid, "set_anchor")
    liftIO . strReq sender $ "anchor=" <> show anchor
  zwlr_layer_surface_v1_set_exclusive_zone zone self = do
    let body = runPut $ zwlr_layer_surface_v1_set_exclusive_zoneBuilder nodata zone
    sendMessage self.wlid zwlr_layer_surface_v1_set_exclusive_zoneOpcode body
    let sender = ("zwlr_layer_surface_v1", self.wlid, "set_exclusive_zone")
    liftIO . strReq sender $ "zone=" <> show zone
  zwlr_layer_surface_v1_set_margin = undefined
  zwlr_layer_surface_v1_set_keyboard_interactivity = undefined
  zwlr_layer_surface_v1_get_popup = undefined
  zwlr_layer_surface_v1_ack_configure serial self = do
    let body = runPut $ zwlr_layer_surface_v1_ack_configureBuilder nodata serial
    sendMessage self.wlid zwlr_layer_surface_v1_ack_configureOpcode body
    let sender = ("zwlr_layer_surface_v1", self.wlid, "ack_configure")
    liftIO . strReq sender $ "serial=" <> show serial
  zwlr_layer_surface_v1_destroy = undefined
  zwlr_layer_surface_v1_set_layer = undefined
  -- }}}
  -- events {{{
  zwlr_layer_surface_v1_configure = undefined
  zwlr_layer_surface_v1_closed = undefined
  -- }}}
-- }}}


-- vim: foldmethod=marker
