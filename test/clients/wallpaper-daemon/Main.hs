{- HLINT ignore "Use camelCase" -}
-- {-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Main (main) where

import Config
import Control.Concurrent (forkIO)
import Control.Exception
import Data.Bimap qualified as BM
import Data.ByteString.Lazy hiding (singleton)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Network.Socket hiding (openSocket)
import Relude hiding (ByteString, get, isPrefixOf, put)
import Saywayland.Protocols.Wayland
import Saywayland.Protocols.WlrLayerShell
import Saywayland.Types
import Saywayland.WaylandSocket
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem

interfaceTable = waylandInterfaceTable <> wlr_layer_shell_unstable_v1InterfaceTable

versionTable = waylandVersionTable <> wlr_layer_shell_unstable_v1VersionTable

main :: IO ()
main = do
  runReaderT program =<< waylandSetup
  where
    waylandSetup = do
      let display = Interface $ WL_display wlDisplayID
      getSocketPath openSocket >>= \case
        Just path -> do
          putStrLn $ "using socket path: " <> show path
          sock <- socket AF_UNIX Stream defaultProtocol
          connect sock $ SockAddrUnix path
          counter <- newIORef $ coerce wlDisplayID
          objects <- newIORef $ Map.fromList [(wlDisplayID, display)]
          globals <- newIORef BM.empty
          handlers <- newIORef mempty
          interfaceTable' <- newIORef $ Map.fromList interfaceTable
          versionTable' <- newIORef $ Map.fromList versionTable
          pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers
        Nothing -> error "couldn't find a $WAYLAND_DISPLAY, nor any open socket."

program :: Wayland Client ()
program = do
  ClientEnv env <- ask
  serial :: TMVar Word32 <- newEmptyTMVarIO
  running :: MVar () <- newEmptyMVar

  display <- fromJust <$> getInterface' (Proxy @WL_display) 1
  registryId <- newObjectId
  runRequest display $ Request_wl_display_get_registry registryId
  (Just registry') <- Map.lookup registryId <$> readIORef env.objects
  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop Client env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())
  let registry = fromJust $ proxyInterface (Proxy @WL_registry) registry'
  putStrLn "Binding to required interfaces..."
  wlShmId <- fromJust <$> bindToInterface registry "wl_shm"
  wl_shm <- fromJust <$> getInterface' (Proxy @WL_shm) wlShmId
  wlCompositorId <- fromJust <$> bindToInterface registry "wl_compositor"
  compositor <- fromJust <$> getInterface' (Proxy @WL_compositor) wlCompositorId
  zwlrLayerShellV1Id <- fromJust <$> bindToInterface registry "zwlr_layer_shell_v1"
  zwlrLayerShellV1 <- fromJust <$> getInterface' (Proxy @Zwlr_layer_shell_v1) zwlrLayerShellV1Id

  modifyIORef env.eventHandlers $ (:) $ EventHandler $ \case
    (Event_zwlr_layer_surface_v1_configure serial' _width _height) -> do
      atomically $ putTMVar serial serial'
    _ -> pure ()

  wlSurfaceId <- newObjectId
  runRequest compositor $ Request_wl_compositor_create_surface wlSurfaceId
  surface' <- fromJust <$> getInterface wlSurfaceId
  let surface = fromJust $ proxyInterface (Proxy @WL_surface) surface'
  layerSurfaceId <- newObjectId
  runRequest zwlrLayerShellV1 $ Request_zwlr_layer_shell_v1_get_layer_surface{id = layerSurfaceId, surface = wlSurfaceId, output = 0, layer = enum_zwlr_layer_shell_v1_layer Enum_zwlr_layer_shell_v1_layerbackground, namespace = "wallpaper"}
  zwlrLayerSurfaceV1 <- fromJust <$> getInterface' (Proxy @Zwlr_layer_surface_v1) layerSurfaceId
  runRequest zwlrLayerSurfaceV1 $ Request_zwlr_layer_surface_v1_set_size{width = fromIntegral bufferWidth, height = fromIntegral bufferHeight}
  runRequest zwlrLayerSurfaceV1 $ Request_zwlr_layer_surface_v1_set_exclusive_zone{zone = -1}

  runRequest surface Request_wl_surface_commit
  atomically (takeTMVar serial) >>= runRequest zwlrLayerSurfaceV1 . Request_zwlr_layer_surface_v1_ack_configure

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      removeSharedMemoryObject _ = shmUnlink poolName
      useSharedMemoryObject fileDescriptor =
        flip runReaderT (ClientEnv env) $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          let poolSize = fromIntegral frameSize
          liftIO . setFdSize fileDescriptor $ fromIntegral poolSize
          wlShmPoolId <- newObjectId
          runRequest wl_shm $ Request_wl_shm_create_pool{id = wlShmPoolId, fd = fileDescriptor, size = poolSize}
          wl_shm_pool <- fromJust <$> getInterface' (Proxy @WL_shm_pool) wlShmPoolId
          wlBufferId <- newObjectId
          runRequest wl_shm_pool $ Request_wl_shm_pool_create_buffer{id = wlBufferId, offset = 0, width = bufferWidth, height = bufferHeight, stride = bufferWidth * colorChannels, format = enum_wl_shm_format colorFormat}

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle image
          runRequest surface Request_wl_surface_attach{buffer = wlBufferId, x = 0, y = 0}
          runRequest surface Request_wl_surface_commit

          -- Wait for exit
          takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject removeSharedMemoryObject useSharedMemoryObject
