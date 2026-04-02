{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances, DefaultSignatures, FunctionalDependencies #-}
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
import Control.Lens (makeFieldsId)
import Data.List (singleton)
-- ObjectID Type {{{
{-
type role ObjectID phantom

-- | Phantom type representing an ID of an object.
newtype ObjectID (a :: Interface) = ObjectID {id :: WlUint}
  deriving newtype (Num, Show)

instance Binary (ObjectID a) where
  get = ObjectID <$> get
  put (ObjectID x) = put x
-}
-- }}}

-- Interfaces {{{
data WL_display = WL_display {wlid :: Word32}
data WL_registry = WL_registry {wlid :: Word32}
data WL_callback = WL_callback {wlid :: Word32}
data WL_compositor = WL_compositor {wlid :: Word32}
data WL_shm_pool = WL_shm_pool {wlid :: Word32, fd :: Fd, size :: IORef Int}
data WL_shm = WL_shm {wlid :: Word32, formats :: IORef [Int]}
data WL_surface = WL_surface {wlid :: Word32}

makeFieldsId ''WL_display
makeFieldsId ''WL_registry
makeFieldsId ''WL_callback
makeFieldsId ''WL_compositor
makeFieldsId ''WL_shm_pool
makeFieldsId ''WL_shm
makeFieldsId ''WL_surface

data Interface = WLDisplay WL_display | WLRegistry WL_registry | WLCallback WL_callback | WLShmPool WL_shm_pool | WLShm WL_shm | WLSurface WL_surface | WLCompositor WL_compositor

class DefaultIO a where
  defM :: IO a

instance DefaultIO WL_display where
  defM = pure WL_display {wlid=wlDisplayID}
instance DefaultIO WL_registry where
  defM = pure WL_registry {wlid=0}
instance DefaultIO WL_callback where
  defM = pure WL_callback {wlid=0}
instance DefaultIO WL_compositor where
  defM = pure WL_compositor {wlid=0}
instance DefaultIO WL_shm_pool where
  defM = newIORef 0 <&> WL_shm_pool 0 0 
instance DefaultIO WL_shm where
  defM = newIORef [] <&> WL_shm 0
instance DefaultIO WL_surface where
  defM = pure WL_surface {wlid=0}

instance InterfaceSet Interface where
  interfaceByStringName = \case
    "wl_display"  -> defM <&> WLDisplay
    "wl_registry" -> defM <&> WLRegistry
    "wl_callback" -> defM <&> WLCallback
    "wl_shm_pool" -> defM <&> WLShmPool
    "wl_shm"      -> defM <&> WLShm
    "wl_surface"  -> defM <&> WLSurface
    _ -> undefined
-- }}}

-- The Wayland Monad {{{
type Wayland p = WaylandM Interface p
-- }}}

-- TemplateHaskell Definitions {{{
$(loadProtocols (ConT ''WaylandM) False "protocols")
--}}}

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
-- Utils {{{
newObjectId :: WaylandM i 'Client Word32
newObjectId = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.counter (+ 1)
    liftIO $ readIORef env.counter

newObject :: Word32 -> i -> WaylandM i 'Client i
newObject intId int = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.insert intId int)
    pure int

{- | Convenience function for sending a Wayland message.
See 'mkMessage'.
-}
sendMessageWithFds :: [Fd] -> Word32 -> Word16 -> BSL.ByteString -> WaylandM i 'Client ()
sendMessageWithFds fds objectID opCode messageBody = do
  socket <- asks (\(ClientEnv env) -> env.socket)
  liftIO $ sendManyWithFds socket [BS.toStrict $ mkMessage objectID opCode messageBody] fds
sendMessage :: Word32 -> Word16 -> BSL.ByteString -> WaylandM i 'Client ()
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
strReq :: (Text, Word32, Text) -> Text -> IO ()
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
instance
        (Protocol_wayland i
        , HasWlid (Type_wl_callback i) Word32
        ) => Interface_wl_display WL_display i Client where
  wl_display_sync callbackId _self = do
    ClientEnv _env <- ask
    callback <- liftIO $ get_wl_callback (Proxy @i)
    _int <- newObject callbackId $ pack_wl_callback callback
    pure ()
  wl_display_get_registry registryID _self = do
    ClientEnv env <- ask
    registry <- liftIO $ get_wl_registry (Proxy @i)
    modifyIORef env.objects (Map.insert registryID $ pack_wl_registry registry)

    let body = runPut $ wl_display_get_registryBuilder (AdditionalParserData []) registryID
    let opcode = wl_display_get_registryOpcode
    sendMessage wlDisplayID opcode body
    liftIO . strReq ("wl_display", wlDisplayID, "get_registry") $ "wl_registry=" <> show registryID
  wl_display_error objectId code message _self = do
    liftIO $ print $ "Unhandled error from `" <> show objectId <> "`: [" <> show code <> "] " <> message
  wl_display_delete_id objectId _self = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.delete objectId)

instance Interface_wl_display WL_display i Server where
  wl_display_sync callbackId self = {-HANDLE sync request-} undefined
  wl_display_get_registry registryId self = {-HANDLE get_registry request-} undefined
  wl_display_error objectId code message self = {-SEND error event-} undefined
  wl_display_delete_id id = {-SEND delete_id event-} undefined
-- }}}

-- WL_registry {{{
instance InterfaceSet i => Interface_wl_registry WL_registry i Client where
  wl_registry_bind name interfaceName interfaceVersion newID _self = do
    --TODO: append `interfaceFromName name` to client interfaces
    let body = runPut $ wl_registry_bindBuilder (AdditionalParserData []) name interfaceName interfaceVersion newID
    sendMessage newID wl_registry_bindOpcode body
    liftIO . strReq ("wl_registry", newID, "registry_bind") $ "name: " <> show name <> " newID: " <> show interfaceName <> ", " <> show interfaceVersion <> ", " <> show newID
  wl_registry_global name interfaceName _interfaceVersion _self = do
    -- likely this requires a version check, like this maybe:
    -- liftIO $ unless (wl_registryVersion == fromIntegral _interfaceVersion) (putStrLn $ "warning: WL_registry version mismatch: [compositor " <> show _interfaceVersion <> "], client [" <> show wl_registryVersion <> "]")
    int <- liftIO $ interfaceByStringName interfaceName
    ClientEnv env <- ask
    modifyIORef env.globals $ Map.insert name int
  wl_registry_global_remove name _self = do
    ClientEnv env <- ask
    modifyIORef env.globals $ Map.delete name

instance Interface_wl_registry WL_registry i Server where
-- }}}

-- WL_callback {{{
instance Interface_wl_callback WL_callback i Client where
  wl_callback_done dat self = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.delete self.wlid)
-- }}}

-- WL_shm_pool {{{
instance Interface_wl_shm_pool WL_shm_pool i Client where
  wl_shm_pool_create_buffer bufID offset width height stride format self = undefined
  wl_shm_pool_destroy self = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete self.wlid
    let body = runPut $ wl_shm_pool_destroyBuilder (AdditionalParserData [])
    sendMessage self.wlid wl_shm_pool_destroyOpcode body
    liftIO $ strReq ("wl_shm_pool", self.wlid, "wl_shm_pool_destroy") ""
  wl_shm_pool_resize new_size self = do
    writeIORef self.size new_size
-- }}}

-- WL_shm {{{
instance (Protocol_wayland i
        , HasWlid (Type_wl_shm_pool i) Word32
        , HasFd (Type_wl_shm_pool i) Fd
        , HasSize (Type_wl_shm_pool i) Int
        ) => Interface_wl_shm WL_shm i Client where
  wl_shm_create_pool poolId fd size self = do
    ClientEnv env <- ask
    pool <- liftIO $ get_wl_shm_pool (Proxy @i)
    modifyIORef env.objects $ Map.insert poolId $ pack_wl_shm_pool pool
      {-{ id = poolId
      , fd = fd
      , size = size
      }-}
    let body = runPut $ wl_shm_create_poolBuilder (AdditionalParserData [fd]) poolId fd size
    sendMessageWithFds [fd] self.wlid wl_shm_create_poolOpcode body
    liftIO $ strReq ("wl_shm", self.wlid, "wl_shm_create_pool") $ "poolId: " <> show poolId <> " fd: " <> show fd <> " size: " <> show size
  wl_shm_release self = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete self.wlid
    let body = runPut $ wl_shm_releaseBuilder (AdditionalParserData [])
    sendMessage self.wlid wl_shm_releaseOpcode body
    liftIO $ strReq ("wl_shm", self.wlid, "wl_shm_release") ""
  wl_shm_format format self = modifyIORef self.formats (fromIntegral format:)
-- }}}

-- vim: foldmethod=marker
