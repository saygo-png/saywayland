{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Main (main) where


import Config
import Control.Concurrent (forkIO)
import Control.Exception
import Data.ByteString.Lazy hiding (singleton)
import Data.Map qualified as Map
import Data.Bimap qualified as BM
import Data.List (singleton)
import Network.Socket hiding (openSocket)
import Relude hiding (ByteString, get, isPrefixOf, put)
import Saywayland
import Saywayland.Types
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem

import Saywayland.Protocols.Wayland
import Saywayland.Protocols.WlrLayerShell
import Saywayland.WaylandSocket
import Protocol
import Data.Maybe (fromJust)

data MyInterface = WLDisplay WL_display | WLRegistry WL_registry | WLCallback WL_callback | WLShmPool WL_shm_pool | WLShm WL_shm | WLSurface WL_surface | WLCompositor WL_compositor | WLRegion WL_region | WLBuffer WL_buffer
                  | ZwlrLayerShellV1 Zwlr_layer_shell_v1 | ZwlrLayerSurfaceV1 Zwlr_layer_surface_v1

instance InterfaceSet MyInterface where
  interfaceByStringName _ = \case
    "wl_display"  -> defM <&> WLDisplay
    "wl_registry" -> defM <&> WLRegistry
    "wl_callback" -> defM <&> WLCallback
    "wl_shm_pool" -> defM <&> WLShmPool
    "wl_shm"      -> defM <&> WLShm
    "wl_surface"  -> defM <&> WLSurface
    _ -> undefined

-- Protocol instance {{{
pure . singleton $ genProtocol ''Protocol_wayland ''MyInterface [
    ("wl_display", ''WL_display, 'WLDisplay)
  , ("wl_buffer", ''WL_buffer, 'WLBuffer)
  , ("wl_registry", ''WL_registry, 'WLRegistry)
  , ("wl_region", ''WL_region, 'WLRegion)
  , ("wl_callback", ''WL_callback, 'WLCallback)
  , ("wl_compositor", ''WL_compositor, 'WLCompositor)
  , ("wl_shm_pool", ''WL_shm_pool, 'WLShmPool)
  , ("wl_shm", ''WL_shm, 'WLShm)
  , ("wl_surface", ''WL_surface, 'WLSurface)
  ]
pure . singleton $ genProtocol ''Protocol_wlr_layer_shell_unstable_v1 ''MyInterface [
    ("zwlr_layer_shell_v1", ''Zwlr_layer_shell_v1, 'ZwlrLayerShellV1)
  , ("zwlr_layer_surface_v1", ''Zwlr_layer_surface_v1, 'ZwlrLayerSurfaceV1)
  ]
-- }}}


main :: IO ()
main = do
  runReaderT program =<< waylandSetup
  where
    waylandSetup = do

      openSocket >>= \case
        Just path -> do
          sock <- socket AF_UNIX Stream defaultProtocol
          connect sock $ SockAddrUnix path
          counter <- newIORef $ coerce wlDisplayID + 1
          globals <- newIORef mempty
          objects <- newIORef BM.empty
          --handlers <- newIORef mempty
          pure $ ClientEnv $ ClientEnvironment sock counter globals objects --handlers
        Nothing -> error "couldn't find a $WAYLAND_DISPLAY, nor any open socket."

program :: WaylandM MyInterface Client ()
program = do
  ClientEnv env <- ask
  serial :: TMVar Word32 <- newEmptyTMVarIO
  running :: MVar () <- newEmptyMVar

  let display = WL_display 1
  
  registryID <- newObjectId
  wl_display_get_registry registryID display
  (Just (WLRegistry registry)) <- Map.lookup registryID <$> readIORef env.objects
  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())

  putStrLn "Binding to required interfaces..."
  wlShmID <- fromJust <$> bindToInterface registry "wl_shm"
  WLShm wl_shm <- fromJust <$> getInterface wlShmID
  wlCompositorID <- fromJust <$> bindToInterface registry "wl_compositor"
  WLCompositor compositor <- fromJust <$> getInterface wlCompositorID
  zwlrLayerShellV1ID <- fromJust <$> bindToInterface registry "zwlr_layer_shell_v1"
  ZwlrLayerShellV1 zwlrLayerShellV1 <- fromJust <$> getInterface zwlrLayerShellV1ID


  {-TODO: port this
  onEvent $ \case
    (EvZwlrLayerSurfaceV1_configure _ body) ->
      atomically $ putTMVar serial body.serial
    _ -> pure ()
  -}

  wlSurfaceID <- newObjectId
  wl_compositor_create_surface wlSurfaceID compositor
  WLSurface surface <- fromJust <$> getInterface wlSurfaceID
  layerSurfaceID <- newObjectId
  zwlr_layer_shell_v1_get_layer_surface layerSurfaceID wlSurfaceID {-output-}0 {-enums might've been a bad idea, this is unreadable-}(enum_zwlr_layer_shell_v1_layer Enum_zwlr_layer_shell_v1_layerbackground) "wallpaper" zwlrLayerShellV1
  ZwlrLayerSurfaceV1 zwlrLayerSurfaceV1 <- fromJust <$> getInterface layerSurfaceID
  zwlr_layer_surface_v1_set_size (fromIntegral bufferWidth) (fromIntegral bufferHeight) zwlrLayerSurfaceV1
  zwlr_layer_surface_v1_set_exclusive_zone (-1) zwlrLayerSurfaceV1
  wl_surface_commit surface
  (`zwlr_layer_surface_v1_ack_configure` zwlrLayerSurfaceV1) =<< atomically (takeTMVar serial)

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      removeSharedMemoryObject _ = shmUnlink poolName
      useSharedMemoryObject fileDescriptor =
        flip runReaderT (ClientEnv env) $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          let poolSize = fromIntegral frameSize
          liftIO . setFdSize fileDescriptor $ fromIntegral poolSize
          wlShmPoolID <- newObjectId
          wl_shm_create_pool wlShmPoolID fileDescriptor poolSize wl_shm
          WLShmPool wl_shm_pool <- fromJust <$> getInterface wlShmPoolID
          wlBufferID <- newObjectId
          wl_shm_pool_create_buffer wlBufferID 0 bufferWidth bufferHeight colorChannels (enum_wl_shm_format colorFormat) wl_shm_pool

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle image
          wl_surface_attach wlBufferID 0 0 surface
          wl_surface_commit surface

          -- Wait for exit
          takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject removeSharedMemoryObject useSharedMemoryObject
