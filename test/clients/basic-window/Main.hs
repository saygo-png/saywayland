module Main (main) where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, finally)
import Data.Bimap qualified as BM
import Data.ByteString (hPut, pack)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import GHC.IO.Handle
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket)
import Relude hiding (hFlush)
import Saywayland
import System.Posix (ShmOpenFlags (ShmOpenFlags), fdToHandle, ownerReadMode, ownerWriteMode, setFdSize, shmOpen, shmUnlink, unionFileModes)
import GHC.IO.Handle
import Control.Concurrent.STM (newTQueue)

interfaceTable :: InterfaceClientTable
interfaceTable = waylandInterfaceClientTable <> xdg_shellInterfaceClientTable

versionTable :: VersionTable
versionTable = waylandVersionTable <> xdg_shellVersionTable

main :: IO ()
main = do
  runReaderT program =<< waylandSetup
  where
    waylandSetup = do
      let display :: Interface Client = Interface $ WL_display wlDisplayID
      getSocketPath openSocket >>= \case
        Just path -> do
          putStrLn $ "using socket path: " <> show path
          sock <- socket AF_UNIX Stream defaultProtocol
          connect sock $ SockAddrUnix path
          counter <- newIORef wlDisplayID
          objects <- newIORef $ Map.fromList [(wlDisplayID, display)]
          globals <- newIORef BM.empty
          handlers <- newIORef mempty
          interfaceTable' <- newIORef $ Map.fromList interfaceTable
          versionTable' <- newIORef $ Map.fromList versionTable
          q <- atomically newTQueue
          pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers q
        Nothing -> error "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."

program :: Wayland Client ()
program = do
  ClientEnv env <- ask
  serial :: TMVar Word32 <- newEmptyTMVarIO
  running :: MVar () <- newEmptyMVar

  display <- fromJust <$> getInterface' @WL_display 1
  registryId <- newObjectId
  runRequest display $ Request_wl_display_get_registry registryId
  registry <- fromJust <$> getInterface' @WL_registry registryId

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())

  putStrLn "Binding to required interfaces..."
  wlShmId <- fromJust <$> bindToInterface registry "wl_shm"
  wl_shm <- fromJust <$> getInterface' @WL_shm wlShmId

  wlCompositorId <- fromJust <$> bindToInterface registry "wl_compositor"
  wl_compositor <- fromJust <$> getInterface' @WL_compositor wlCompositorId

  wlSurfaceId <- newObjectId
  runRequest wl_compositor $ Request_wl_compositor_create_surface wlSurfaceId
  surface <- fromJust <$> getInterface' @WL_surface wlSurfaceId

  xdgWmBaseId <- fromJust <$> bindToInterface registry "xdg_wm_base"
  xdg_wm_base <- fromJust <$> getInterface' @XDG_wm_base xdgWmBaseId

  xdgSurfaceId <- newObjectId
  runRequest xdg_wm_base $ Request_xdg_wm_base_get_xdg_surface xdgSurfaceId wlSurfaceId
  xdg_surface <- fromJust <$> getInterface' @XDG_surface xdgSurfaceId

  xdgToplevelId <- newObjectId
  runRequest xdg_surface $ Request_xdg_surface_get_toplevel xdgToplevelId-}

  bufferWidth <- liftIO $ newIORef 512
  bufferHeight <- liftIO $ newIORef 512
  let colorChannels = 4
  let
    makeSharedMemoryObject = shmOpen "wl_shm_pool" (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
    useSharedMemoryObject fileDescriptor =
      flip runReaderT (ClientEnv env) $ do
        bw <- liftIO $ readIORef bufferWidth
        bh <- liftIO $ readIORef bufferHeight
        let frameSize = bw * bh * colorChannels
        liftIO . setFdSize fileDescriptor $ fromIntegral frameSize
        wlShmPoolId <- newObjectId
        runRequest wl_shm $ Request_wl_shm_create_pool{id = wlShmPoolId, fd = fileDescriptor, size = frameSize}
        wl_shm_pool <- fromJust <$> getInterface' @WL_shm_pool wlShmPoolId
        wlBufferId <- newObjectId
        runRequest wl_shm_pool
          $ Request_wl_shm_pool_create_buffer
            { id = wlBufferId
            , offset = 0
            , width = bw
            , height = bh
            , stride = bw * colorChannels
            , format = Enum_wl_shm_format_argb8888
            }
        buffer <- fromJust <$> getInterface' @WL_buffer wlBufferId
        fileHandle <- liftIO $ fdToHandle fileDescriptor

        liftIO $ hPut fileHandle $ image bw bh
        liftIO $ hFlush fileHandle
        runRequest surface Request_wl_surface_attach{buffer = wlBufferId, x = 0, y = 0}
        runRequest surface Request_wl_surface_commit
        lastbuffer' <- newIORef buffer
        modifyIORef env.eventHandlers $ (:) $ EventHandler $ \case
          (Event_xdg_toplevel_configure width height _) -> when (width > 0 && height > 0) $ do
            bw <- liftIO $ readIORef bufferWidth
            bh <- liftIO $ readIORef bufferHeight
            let frameSize = bw * bh * colorChannels
            let newSize = width * height * colorChannels
            when (newSize > frameSize) $ do
              liftIO $ setFdSize fileDescriptor $ fromIntegral newSize
              runRequest wl_shm_pool Request_wl_shm_pool_resize{size = newSize}
            liftIO $ hSeek fileHandle AbsoluteSeek 0
            liftIO $ hPut fileHandle $ image width height
            liftIO $ hFlush fileHandle
            newBufferId <- newObjectId
            print newBufferId
            runRequest wl_shm_pool
              $ Request_wl_shm_pool_create_buffer
                { id = newBufferId
                , offset = 0
                , width = width
                , height = height
                , stride = width * colorChannels
                , format = Enum_wl_shm_format_argb8888
                }
            newbuffer <- fromJust <$> getInterface' @WL_buffer newBufferId
            runRequest surface Request_wl_surface_attach{buffer = newBufferId, x = 0, y = 0}
            runRequest surface Request_wl_surface_commit
            lastbuffer <- readIORef lastbuffer'
            runRequest lastbuffer Request_wl_buffer_destroy
            writeIORef lastbuffer' newbuffer
            liftIO $ writeIORef bufferWidth width
            liftIO $ writeIORef bufferHeight height
          -- runRequest buffer Request_wl_buffer_destroy
          _ -> pure ()
        -- Wait for exit
        takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject (const $ shmUnlink "wl_shm_pool") useSharedMemoryObject

-- | Rainbow image :D
image :: Int -> Int -> ByteString
image bufferWidth bufferHeight =
  generateBGRA8 $ \x y ->
    let tx = fi x / fi @Int (fi bufferWidth - 1) :: Double
        ty = fi y / fi @Int (fi bufferHeight - 1) :: Double
        b = round $ tx * 255 -- left -> right
        g = round $ ty * 255 -- top -> bottom
        r = round $ (1 - tx) * 255 -- right -> left
        a = round $ (1 - ty) * 255 -- bottom -> top
     in (b, g, r, a)
  where
    fi :: forall a b. (Integral a, Num b) => a -> b
    fi = fromIntegral
    generateBGRA8 :: (Int -> Int -> (Word8, Word8, Word8, Word8)) -> ByteString
    generateBGRA8 pixelFn =
      pack
        [ byte
        | y <- [0 .. fi bufferHeight - 1]
        , x <- [0 .. fi bufferWidth - 1]
        , let (b, g, r, a) = pixelFn x y
        , byte <- [b, g, r, a]
        ]
