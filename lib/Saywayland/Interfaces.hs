{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
-- | This module combines all implemented protocols into a single Interface datatype.
module Saywayland.Interfaces where

import Relude
import Saywayland.Protocols.Wayland
import Protocol
import Data.List (singleton)

data Interface = WLDisplay WL_display | WLRegistry WL_registry | WLCallback WL_callback | WLShmPool WL_shm_pool | WLShm WL_shm | WLSurface WL_surface | WLCompositor WL_compositor

instance InterfaceSet Interface where
  interfaceByStringName = \case
    "wl_display"  -> defM <&> WLDisplay
    "wl_registry" -> defM <&> WLRegistry
    "wl_callback" -> defM <&> WLCallback
    "wl_shm_pool" -> defM <&> WLShmPool
    "wl_shm"      -> defM <&> WLShm
    "wl_surface"  -> defM <&> WLSurface
    _ -> undefined

-- Protocol instance {{{
pure . singleton $ genProtocol ''Protocol_wayland ''Interface [
    ("wl_display", ''WL_display, 'WLDisplay)
  , ("wl_registry", ''WL_registry, 'WLRegistry)
  , ("wl_callback", ''WL_callback, 'WLCallback)
  , ("wl_compositor", ''WL_compositor, 'WLCompositor)
  , ("wl_shm_pool", ''WL_shm_pool, 'WLShmPool)
  , ("wl_shm", ''WL_shm, 'WLShm)
  , ("wl_surface", ''WL_surface, 'WLSurface)
  ]
-- }}}
