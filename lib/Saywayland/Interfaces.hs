{-# LANGUAGE TemplateHaskell #-}
module Saywayland.Interfaces where
-- this module imlements some interfaces using classes defined by the `Protocol` module.

import Relude hiding (get,put)
import Protocol hiding (ObjectID)
import Saywayland.Types
import Language.Haskell.TH (Type(ConT))

import Data.ByteString.Lazy qualified as BSL
import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Binary.Put (runPut, putLazyByteString)
import Data.Binary (Binary (get, put))
import Network.Socket.ByteString.Lazy (sendAll)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)
import System.Posix (Fd)
import Network.Socket.ByteString (sendManyWithFds)

-- ObjectID Type {{{
type role ObjectID phantom

-- | Phantom type representing an ID of an object.
newtype ObjectID (a :: Interface) = ObjectID {id :: WlUint}
  deriving newtype (Num, Show)

instance Binary (ObjectID a) where
  get = ObjectID <$> get
  put (ObjectID x) = put x
-- }}}

-- Interfaces {{{
data WL_display = WL_display {}
data WL_registry = WL_registry {}
data WL_callback = WL_callback {id :: Word32}
data WL_compositor = WL_compositor {}
data WL_shm_pool = WL_shm_pool {id:: Word32, fd :: Fd, size :: Int}
data WL_shm = WL_shm {id :: Word32, formats :: IORef [Int]}
data WL_surface = WL_surface {id :: Word32}

data Interface = WLDisplay WL_display | WLRegistry WL_registry | WLCallback WL_callback | WLShmPool WL_shm_pool | WLShm WL_shm | WLSurface WL_surface
-- }}}

-- The Wayland Monad {{{
type Wayland p = WaylandM Interface p
-- }}}

-- TemplateHaskell Definitions {{{
$(loadProtocols (ConT ''Wayland) False "protocols")
--}}}

-- Utils {{{
newObjectId :: Wayland 'Client Word32
newObjectId = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.counter (+ 1)
    liftIO $ readIORef env.counter

newObject :: Word32 -> Interface -> Wayland 'Client Interface
newObject intId int = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.insert intId int)
    pure int

{- | Convenience function for sending a Wayland message.
See 'mkMessage'.
-}
sendMessageWithFds :: [Fd] -> Word32 -> Word16 -> BSL.ByteString -> Wayland 'Client ()
sendMessageWithFds fds objectID opCode messageBody = do
  socket <- asks (\(ClientEnv env) -> env.socket)
  liftIO $ sendManyWithFds socket [BS.toStrict $ mkMessage objectID opCode messageBody] fds
sendMessage :: Word32 -> Word16 -> BSL.ByteString -> Wayland 'Client ()
sendMessage objectID opCode messageBody = do
  wlSocket <- asks (\(ClientEnv env) -> env.socket)
  liftIO . sendAll wlSocket $ mkMessage objectID opCode messageBody

{- | Convenience function for formatting a Wayland message.
It takes an objectID, operation code and a message body.
The header is generated based on this, the size is derived automatically.
-}
mkMessage :: Word32 -> Word16 -> BSL.ByteString -> BSL.ByteString
mkMessage objectID opCode messageBody =
  runPut $ do
    put $ Header (fromIntegral objectID) opCode (headerSize + fromIntegral (BSL.length messageBody))
    putLazyByteString messageBody

{- | Convenience function for formatting events.
Events are colored in magenta following the wayland.app colorscheme.
-}
strReq :: (Text, ObjectID a, Text) -> Text -> IO ()
strReq (object, objectID, method) text = do
  colorize <- getColorize
  putTextLn . colorize Vivid Magenta $ mconcat ["        -> ", object, "@", show objectID, ".", method, ": ", text]
  where
    getColorize :: (IsString s, Semigroup s) => IO (ColorIntensity -> Color -> s -> s)
    getColorize = do
      ansiSupport <- hNowSupportsANSI stdout
      pure
        $ if ansiSupport
          then \ci c t -> fromString (setSGRCode [SetColor Foreground ci c]) <> t <> fromString (setSGRCode [Reset])
          else const $ const id

-- }}}


-- Interface Implementations

-- WL_display {{{

instance Interface_wl_display WL_display Client where
  wl_display_sync callbackId self = do
    ClientEnv _env <- ask
    (WLCallback _callback) <- newObject callbackId (WLCallback WL_callback {id = callbackId})
    pure ()
  wl_display_get_registry registryID self = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.insert registryID (WLRegistry WL_registry {}))

    let body = runPut $ wl_display_get_registryBuilder (AdditionalParserData []) registryID
    let opcode = wl_display_get_registryOpcode
    sendMessage wlDisplayID opcode body
    liftIO . strReq ("wl_display", fromIntegral wlDisplayID, "get_registry") $ "wl_registry=" <> show registryID
  wl_display_error objectId code message _self = do
    liftIO $ print $ "Unhandled error from `" <> show objectId <> "`: [" <> show code <> "] " <> message
  wl_display_delete_id objectId _self = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.delete objectId)


instance Interface_wl_display WL_display Server where
  wl_display_sync callbackId self = {-HANDLE sync request-} undefined
  wl_display_get_registry registryId self = {-HANDLE get_registry request-} undefined
  wl_display_error objectId code message self = {-SEND error event-} undefined
  wl_display_delete_id id = {-SEND delete_id event-} undefined
-- }}}

-- WL_registry {{{
instance Interface_wl_registry WL_registry Client where
  wl_registry_bind name interfaceName interfaceVersion newID self = do
    --TODO: append `interfaceFromName name` to client interfaces
    let body = runPut $ wl_registry_bindBuilder (AdditionalParserData []) name interfaceName interfaceVersion newID
    sendMessage newID wl_registry_bindOpcode body
    liftIO . strReq ("wl_registry", fromIntegral newID, "registry_bind") $ "name: " <> show name <> " newID: " <> show interfaceName <> ", " <> show interfaceVersion <> ", " <> show newID
  wl_registry_global name interfaceName interfaceVersion self = undefined
  wl_registry_global_remove name = undefined
instance Interface_wl_registry WL_registry Server where
-- }}}

-- WL_callback {{{
instance Interface_wl_callback WL_callback Client where
  wl_callback_done dat self = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.delete self.id)
-- }}}

-- WL_shm_pool {{{
instance Interface_wl_shm_pool WL_shm_pool Client where
  wl_shm_pool_create_buffer bufID offset width height stride format self = undefined
  wl_shm_pool_destroy self = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete self.id
    let body = runPut $ wl_shm_pool_destroyBuilder (AdditionalParserData [])
    sendMessage self.id wl_shm_pool_destroyOpcode body
    liftIO $ strReq ("wl_shm_pool", fromIntegral self.id, "wl_shm_pool_destroy") ""
  wl_shm_pool_resize size self = undefined
-- }}}

-- WL_shm {{{
instance Interface_wl_shm WL_shm Client where
  wl_shm_create_pool poolId fd size self = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.insert poolId $ WLShmPool WL_shm_pool
      { id = poolId
      , fd = fd
      , size = size
      }
    let body = runPut $ wl_shm_create_poolBuilder (AdditionalParserData [fd]) poolId fd size
    sendMessageWithFds [fd] self.id wl_shm_create_poolOpcode body
    liftIO $ strReq ("wl_shm", fromIntegral self.id, "wl_shm_create_pool") $ "poolId: " <> show poolId <> " fd: " <> show fd <> " size: " <> show size
  wl_shm_release self = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete self.id
    let body = runPut $ wl_shm_releaseBuilder (AdditionalParserData [])
    sendMessage self.id wl_shm_releaseOpcode body
    liftIO $ strReq ("wl_shm", fromIntegral self.id, "wl_shm_release") ""
  wl_shm_format format self = modifyIORef self.formats (fromIntegral format:)
-- }}}

-- vim: foldmethod=marker
