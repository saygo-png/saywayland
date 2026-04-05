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
makeFieldsId ''WL_display
data WL_registry = WL_registry {wlid :: Word32}
makeFieldsId ''WL_registry
data WL_callback = WL_callback {wlid :: Word32}
makeFieldsId ''WL_callback
data WL_compositor = WL_compositor {wlid :: Word32}
makeFieldsId ''WL_compositor
data WL_shm_pool = WL_shm_pool {wlid :: Word32, fd :: Fd, size :: IORef Int}
makeFieldsId ''WL_shm_pool
data WL_shm = WL_shm {wlid :: Word32, formats :: IORef [Int]}
makeFieldsId ''WL_shm
data WL_buffer = WL_buffer {wlid :: Word32}
makeFieldsId ''WL_buffer
data WL_data_offer = WL_data_offer {wlid :: Word32}
makeFieldsId ''WL_data_offer
data WL_data_source = WL_data_source {wlid :: Word32}
makeFieldsId ''WL_data_source
data WL_data_device = WL_data_device {wlid :: Word32}
makeFieldsId ''WL_data_device
data WL_data_device_manager = WL_data_device_manager {wlid :: Word32}
makeFieldsId ''WL_data_device_manager
data WL_shell = WL_shell {wlid :: Word32}
makeFieldsId ''WL_shell
data WL_shell_surface = WL_shell_surface {wlid :: Word32}
makeFieldsId ''WL_shell_surface
data WL_surface = WL_surface {wlid :: Word32}
makeFieldsId ''WL_surface
data WL_seat = WL_seat {wlid :: Word32}
makeFieldsId ''WL_seat
data WL_pointer = WL_pointer {wlid :: Word32}
makeFieldsId ''WL_pointer
data WL_keyboard = WL_keyboard {wlid :: Word32}
makeFieldsId ''WL_keyboard
data WL_touch = WL_touch {wlid :: Word32}
makeFieldsId ''WL_touch
data WL_output = WL_output {wlid :: Word32}
makeFieldsId ''WL_output
data WL_region = WL_region {wlid :: Word32}
makeFieldsId ''WL_region
data WL_subcompositor = WL_subcompositor {wlid :: Word32}
makeFieldsId ''WL_subcompositor
data WL_subsurface = WL_subsurface {wlid :: Word32}
makeFieldsId ''WL_subsurface
data WL_fixes = WL_fixes {wlid :: Word32}
makeFieldsId ''WL_fixes

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
  -- requests {{{
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
  -- }}}
  -- events {{{
  wl_display_error objectId code message _self = do
    liftIO $ print $ "Unhandled error from `" <> show objectId <> "`: [" <> show code <> "] " <> message
  wl_display_delete_id objectId _self = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.delete objectId)
  -- }}}
instance Interface_wl_display WL_display i Server where
  -- requests {{{
  wl_display_sync callbackId self = {-HANDLE sync request-} undefined
  wl_display_get_registry registryId self = {-HANDLE get_registry request-} undefined
  -- }}}
  -- events {{{
  wl_display_error objectId code message self = {-SEND error event-} undefined
  wl_display_delete_id id = {-SEND delete_id event-} undefined
  -- }}}
-- }}}

-- WL_registry {{{
instance InterfaceSet i => Interface_wl_registry WL_registry i Client where
  -- requests {{{
  wl_registry_bind name interfaceName interfaceVersion newID _self = do
    --TODO: append `interfaceFromName name` to client interfaces
    let body = runPut $ wl_registry_bindBuilder (AdditionalParserData []) name interfaceName interfaceVersion newID
    sendMessage newID wl_registry_bindOpcode body
    liftIO . strReq ("wl_registry", newID, "registry_bind") $ "name: " <> show name <> " newID: " <> show interfaceName <> ", " <> show interfaceVersion <> ", " <> show newID
  -- }}}
  -- events {{{
  wl_registry_global name interfaceName _interfaceVersion _self = do
    -- likely this requires a version check, like this maybe:
    -- liftIO $ unless (wl_registryVersion == fromIntegral _interfaceVersion) (putStrLn $ "warning: WL_registry version mismatch: [compositor " <> show _interfaceVersion <> "], client [" <> show wl_registryVersion <> "]")
    int <- liftIO $ interfaceByStringName interfaceName
    ClientEnv env <- ask
    modifyIORef env.globals $ Map.insert name int
  wl_registry_global_remove name _self = do
    ClientEnv env <- ask
    modifyIORef env.globals $ Map.delete name
  -- }}}
instance Interface_wl_registry WL_registry i Server where
-- }}}

-- WL_callback {{{
instance Interface_wl_callback WL_callback i Client where
  wl_callback_done dat self = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.delete self.wlid)
-- }}}

-- WL_compositor {{{
instance Interface_wl_compositor WL_compositor i Client where
  -- requests {{{
  wl_compositor_create_surface surfaceId self = undefined
  wl_compositor_create_region regionId self = undefined
  wl_compositor_release self = undefined
  -- }}}
-- }}}

-- WL_shm_pool {{{
instance Interface_wl_shm_pool WL_shm_pool i Client where
  -- requests {{{
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
-- }}}

-- WL_shm {{{
instance (Protocol_wayland i
        , HasWlid (Type_wl_shm_pool i) Word32
        , HasFd (Type_wl_shm_pool i) Fd
        , HasSize (Type_wl_shm_pool i) Int
        ) => Interface_wl_shm WL_shm i Client where
  -- requests {{{
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
  -- }}}
  -- events {{{
  wl_shm_format format self = modifyIORef self.formats (fromIntegral format:)
  -- }}}
-- }}}

-- WL_buffer {{{
instance Interface_wl_buffer WL_buffer i Client where
  -- requests
  wl_buffer_destroy = undefined
  -- events
  wl_buffer_release = undefined
-- }}}

-- WL_data_offer {{{
instance Interface_wl_data_offer WL_data_offer i Client where
  -- requests {{{
  wl_data_offer_accept = undefined
  wl_data_offer_receive = undefined
  wl_data_offer_destroy = undefined
  wl_data_offer_finish = undefined
  wl_data_offer_set_actions = undefined
  -- }}}
  -- events {{{
  wl_data_offer_offer = undefined
  wl_data_offer_source_actions = undefined
  wl_data_offer_action = undefined
  -- }}}
-- }}}

-- WL_data_source {{{
instance Interface_wl_data_source WL_data_source i Client where
  -- requests {{{
  wl_data_source_offer = undefined
  wl_data_source_destroy = undefined
  wl_data_source_set_actions = undefined
  -- }}}
  -- events {{{
  wl_data_source_target = undefined 
  wl_data_source_send = undefined
  wl_data_source_cancelled = undefined 
  wl_data_source_dnd_drop_performed = undefined 
  wl_data_source_dnd_finished = undefined
  wl_data_source_action = undefined
  -- }}}
-- }}}

-- WL_data_device {{{
instance Interface_wl_data_device WL_data_device i Client where
  -- requests {{{
  wl_data_device_start_drag = undefined 
  wl_data_device_set_selection = undefined 
  wl_data_device_release = undefined 
  -- }}}
  -- events {{{
  wl_data_device_data_offer = undefined 
  wl_data_device_enter = undefined 
  wl_data_device_leave = undefined 
  wl_data_device_motion = undefined 
  wl_data_device_drop = undefined 
  wl_data_device_selection = undefined
  -- }}}
-- }}}

-- wl_data_device_manager {{{
instance Interface_wl_data_device_manager WL_data_device_manager i Client where
  -- requests {{{
  wl_data_device_manager_create_data_source = undefined
  wl_data_device_manager_get_data_device = undefined
  wl_data_device_manager_release = undefined
  --}}}
  -- events {{{
  --}}}
--}}}

-- wl_shell {{{
instance Interface_wl_shell WL_shell i Client where
  -- requests {{{
  wl_shell_get_shell_surface = undefined
  --}}}
  -- events {{{
  --}}}
--}}}

-- wl_shell_surface {{{
instance Interface_wl_shell_surface WL_shell_surface i Client where
  -- requests {{{
  wl_shell_surface_pong = undefined
  wl_shell_surface_move = undefined
  wl_shell_surface_resize = undefined
  wl_shell_surface_set_toplevel = undefined
  wl_shell_surface_set_transient = undefined
  wl_shell_surface_set_fullscreen = undefined
  wl_shell_surface_set_popup = undefined
  wl_shell_surface_set_maximized = undefined
  wl_shell_surface_set_title = undefined
  wl_shell_surface_set_class = undefined
  --}}}
  -- events {{{
  wl_shell_surface_ping = undefined
  wl_shell_surface_configure = undefined
  wl_shell_surface_popup_done = undefined
  --}}}
--}}}

-- wl_surface {{{
instance Interface_wl_surface WL_surface i Client where
  -- requests {{{
  wl_surface_destroy = undefined
  wl_surface_attach = undefined
  wl_surface_damage = undefined
  wl_surface_frame = undefined
  wl_surface_set_opaque_region = undefined
  wl_surface_set_input_region = undefined
  wl_surface_commit = undefined
  wl_surface_set_buffer_transform = undefined
  wl_surface_set_buffer_scale = undefined
  wl_surface_damage_buffer = undefined
  wl_surface_offset = undefined
  wl_surface_get_release = undefined
  --}}}
  -- events {{{
  wl_surface_enter = undefined
  wl_surface_leave = undefined
  wl_surface_preferred_buffer_scale = undefined
  wl_surface_preferred_buffer_transform = undefined
  --}}}
--}}}

-- wl_seat {{{
instance Interface_wl_seat WL_seat i Client where
  -- requests {{{
  wl_seat_get_pointer = undefined
  wl_seat_get_keyboard = undefined
  wl_seat_get_touch = undefined
  wl_seat_release = undefined
  --}}}
  -- events {{{
  wl_seat_capabilities = undefined
  wl_seat_name = undefined
  --}}}
--}}}

-- wl_pointer {{{
instance Interface_wl_pointer WL_pointer i Client where
  -- requests {{{
  wl_pointer_set_cursor = undefined
  wl_pointer_release = undefined
  --}}}
  -- events {{{
  wl_pointer_enter = undefined
  wl_pointer_leave = undefined
  wl_pointer_motion = undefined
  wl_pointer_button = undefined
  wl_pointer_axis = undefined
  wl_pointer_frame = undefined
  wl_pointer_axis_source = undefined
  wl_pointer_axis_stop = undefined
  wl_pointer_axis_discrete = undefined
  wl_pointer_axis_value120 = undefined
  wl_pointer_axis_relative_direction = undefined
  --}}}
--}}}

-- wl_keyboard {{{
instance Interface_wl_keyboard WL_keyboard i Client where
  -- requests {{{
  wl_keyboard_release = undefined
  --}}}
  -- events {{{
  wl_keyboard_keymap = undefined
  wl_keyboard_enter = undefined
  wl_keyboard_leave = undefined
  wl_keyboard_key = undefined
  wl_keyboard_modifiers = undefined
  wl_keyboard_repeat_info = undefined
  --}}}
--}}}

-- wl_touch {{{
instance Interface_wl_touch WL_touch i Client where
  -- requests {{{
  wl_touch_release = undefined
  --}}}
  -- events {{{
  wl_touch_down = undefined
  wl_touch_up = undefined
  wl_touch_motion = undefined
  wl_touch_frame = undefined
  wl_touch_cancel = undefined
  wl_touch_shape = undefined
  wl_touch_orientation = undefined
  --}}}
--}}}

-- wl_output {{{
instance Interface_wl_output WL_output i Client where
  -- requests {{{
  wl_output_release = undefined
  --}}}
  -- events {{{
  wl_output_geometry = undefined
  wl_output_mode = undefined
  wl_output_done = undefined
  wl_output_scale = undefined
  wl_output_name = undefined
  wl_output_description = undefined
  --}}}
--}}}

-- wl_region {{{
instance Interface_wl_region WL_region i Client where
  -- requests {{{
  wl_region_destroy = undefined
  wl_region_add = undefined
  wl_region_subtract = undefined
  --}}}
  -- events {{{
  --}}}
--}}}

-- wl_subcompositor {{{
instance Interface_wl_subcompositor WL_subcompositor i Client where
  -- requests {{{
  wl_subcompositor_destroy = undefined
  wl_subcompositor_get_subsurface = undefined
  --}}}
  -- events {{{
  --}}}
--}}}

-- wl_subsurface {{{
instance Interface_wl_subsurface WL_subsurface i Client where
  -- requests {{{
  wl_subsurface_destroy = undefined
  wl_subsurface_set_position = undefined
  wl_subsurface_place_above = undefined
  wl_subsurface_place_below = undefined
  wl_subsurface_set_sync = undefined
  wl_subsurface_set_desync = undefined
  --}}}
  -- events {{{
  --}}}
--}}}

-- wl_fixes {{{
instance Interface_wl_fixes WL_fixes i Client where
  -- requests {{{
  wl_fixes_destroy = undefined
  wl_fixes_destroy_registry = undefined
  --}}}
  -- events {{{
  --}}}
--}}}



-- vim: foldmethod=marker
