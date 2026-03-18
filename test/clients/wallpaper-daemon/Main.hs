{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Config
import Control.Concurrent (forkIO)
import Control.Exception
import Data.ByteString.Lazy
import Network.Socket
import Relude hiding (ByteString, get, isPrefixOf, put)
import Saywayland
import Saywayland.Types
import System.Posix (ownerReadMode, ownerWriteMode, setFdSize, unionFileModes)
import System.Posix.IO
import System.Posix.SharedMem

main :: IO ()
main = do
  runReaderT program =<< waylandSetup
  where
    waylandSetup = do
      sock <- connectToWlSocket
      counter <- newIORef $ coerce wlDisplayID + 1
      globals <- newIORef mempty
      objects <- newIORef mempty
      handlers <- newIORef mempty
      pure $ ClientEnv sock counter globals objects handlers

program :: Wayland ()
program = do
  env <- ask
  serial :: TMVar WlUint <- newEmptyTMVarIO
  running :: MVar () <- newEmptyMVar

  registryID <- wlDisplay_getRegistry

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT eventLoop env)
      (close env.socket >> putMVar running ())

  putStrLn "Binding to required interfaces..."
  wlShmID <- bindToInterface registryID env.globals "wl_shm" WlShm
  wlCompositorID <- bindToInterface registryID env.globals "wl_compositor" WlCompositor
  zwlrLayerShellV1ID <- bindToInterface registryID env.globals "zwlr_layer_shell_v1" ZwlrLayerShellV1

  onEvent $ \case
    (EvZwlrLayerSurfaceV1_configure _ body) ->
      atomically $ putTMVar serial body.serial
    _ -> pure ()

  wlSurfaceID <- wlCompositor_createSurface wlCompositorID
  layerSurfaceID <- zwlrLayerShellV1_getLayerSurface zwlrLayerShellV1ID wlSurfaceID 0 "wallpaper"
  zwlrLayerSurfaceV1_setSize layerSurfaceID (fromIntegral bufferWidth) (fromIntegral bufferHeight)
  zwlrLayerSurfaceV1_setExclusiveZone layerSurfaceID (-1)
  wlSurface_commit wlSurfaceID
  zwlrLayerSurfaceV1_ackConfigure layerSurfaceID =<< atomically (takeTMVar serial)

  let makeSharedMemoryObject = shmOpen poolName (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      removeSharedMemoryObject _ = shmUnlink poolName
      useSharedMemoryObject fileDescriptor =
        flip runReaderT env $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          let poolSize = frameSize
          liftIO . setFdSize fileDescriptor $ fromIntegral poolSize
          wlShmPoolID <- wlShm_createPool wlShmID fileDescriptor poolSize
          buffer <- wlShmPool_createBuffer wlShmPoolID 0 bufferWidth bufferHeight colorChannels colorFormat

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle image
          wlSurface_attach wlSurfaceID buffer.id
          wlSurface_commit wlSurfaceID

          -- Wait for exit
          takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject removeSharedMemoryObject useSharedMemoryObject
