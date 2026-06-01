module Main (main) where
import Saywayland
import Relude
import Network.Socket (SockAddr(SockAddrUnix), socket, Family (AF_UNIX), SocketType (Stream), defaultProtocol, connect, close)
import Data.Map qualified as Map
import Data.Bimap qualified as BM
import Data.Maybe (fromJust)
import Control.Concurrent (forkIO)
import Control.Exception (finally, bracket)
import System.Posix (setFdSize, fdToHandle, shmUnlink, unionFileModes, ownerWriteMode, ownerReadMode, ShmOpenFlags (ShmOpenFlags), shmOpen)
import Data.ByteString (pack, hPut)

interfaceTable :: InterfaceClientTable
interfaceTable = waylandInterfaceClientTable

versionTable :: VersionTable
versionTable = waylandVersionTable


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
          pure $ ClientEnv $ ClientEnvironment sock counter objects globals interfaceTable' versionTable' handlers
        Nothing -> error "couldn't find `$WAYLAND_DISPLAY`, nor any open socket."


program :: Wayland Client ()
program = do
  ClientEnv env <- ask
  serial :: TMVar Word32 <- newEmptyTMVarIO
  running :: MVar () <- newEmptyMVar

  display <- fromJust <$> getInterface' (Proxy @WL_display) 1
  registryId <- newObjectId
  runRequest display $ Request_wl_display_get_registry registryId
  registry <- fromJust <$> getInterface' (Proxy @WL_registry) registryId

  liftIO
    . void
    . forkIO
    $ finally
      (putStrLn "\n--- Starting event loop ---" >> runReaderT (clientLoop env.socket) (ClientEnv env))
      (close env.socket >> putMVar running ())

  putStrLn "Binding to required interfaces..."
  wlShmId <- fromJust <$> bindToInterface registry "wl_shm"
  wl_shm <- fromJust <$> getInterface' (Proxy @WL_shm) wlShmId

  wlCompositorId <- fromJust <$> bindToInterface registry "wl_compositor"
  wl_compositor <- fromJust <$> getInterface' (Proxy @WL_compositor) wlCompositorId
  

  wlSurfaceId <- newObjectId
  runRequest wl_compositor $ Request_wl_compositor_create_surface wlSurfaceId
  surface <- fromJust <$> getInterface' (Proxy @WL_surface) wlSurfaceId

  let 
      makeSharedMemoryObject = shmOpen "wl_shm_pool" (ShmOpenFlags True True False True) (Relude.foldl' unionFileModes ownerWriteMode [ownerReadMode])
      useSharedMemoryObject fileDescriptor =
        flip runReaderT (ClientEnv env) $ do
          let frameSize = bufferWidth * bufferHeight * colorChannels
          liftIO . setFdSize fileDescriptor $ fromIntegral frameSize
          wlShmPoolId <- newObjectId
          runRequest wl_shm $ Request_wl_shm_create_pool{id = wlShmPoolId, fd = fileDescriptor, size = frameSize}
          wl_shm_pool <- fromJust <$> getInterface' (Proxy @WL_shm_pool) wlShmPoolId
          wlBufferId <- newObjectId
          runRequest wl_shm_pool
            $ Request_wl_shm_pool_create_buffer
              { id = wlBufferId
              , offset = 0
              , width = bufferWidth
              , height = bufferHeight
              , stride = bufferWidth * colorChannels
              , format = Enum_wl_shm_format_argb8888
              }

          fileHandle <- liftIO $ fdToHandle fileDescriptor

          liftIO $ hPut fileHandle image
          runRequest surface Request_wl_surface_attach{buffer = wlBufferId, x = 0, y = 0}
          runRequest surface Request_wl_surface_commit

          -- Wait for exit
          takeMVar running

  liftIO . void $ bracket makeSharedMemoryObject (const $ shmUnlink "wl_shm_pool") useSharedMemoryObject


  undefined
-- | Rainbow image :D
image :: ByteString
image =
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
bufferWidth = 512
bufferHeight = 512
colorChannels = 4
