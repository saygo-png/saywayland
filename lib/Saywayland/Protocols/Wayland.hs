{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances, DefaultSignatures, FunctionalDependencies #-}
module Saywayland.Protocols.Wayland where
-- this module imlements some interfaces using classes defined by the `Protocol` module.

import Relude hiding (get)
import Protocol
import Saywayland.Types

import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Bimap qualified as BM
import Data.Binary.Put (runPut)
import System.Posix (Fd)
import Control.Lens (makeFieldsId, (.~))
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Control.Concurrent (threadDelay)

-- TemplateHaskell Definitions {{{
$(loadProtocolFile False "protocols/wayland.xml")
-- }}}


-- Interfaces {{{
data WL_display = WL_display {wlid :: Word32}
makeFieldsId ''WL_display
data WL_registry = WL_registry {wlid :: Word32}
makeFieldsId ''WL_registry
data WL_callback = WL_callback {wlid :: Word32, done :: MVar ()}
makeFieldsId ''WL_callback
data WL_compositor = WL_compositor {wlid :: Word32}
makeFieldsId ''WL_compositor
data WL_shm_pool = WL_shm_pool {wlid :: Word32, fd :: Fd, size :: IORef Int}
makeFieldsId ''WL_shm_pool
data WL_shm = WL_shm {wlid :: Word32, formats :: IORef [Int]}
makeFieldsId ''WL_shm
data WL_buffer = WL_buffer
  { wlid    :: Word32
  , offset  :: Int
  , width   :: Int
  , height  :: Int
  , stride  :: Int
  , format  :: Enum_wl_shm_format
  }
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

instance DefaultIO WL_display where
  defM = pure WL_display {wlid=wlDisplayID}
instance DefaultIO WL_registry where
  defM = pure WL_registry {wlid=0}
instance DefaultIO WL_buffer where
  defM = pure WL_buffer {wlid=0, offset=0, width=0, height=0,stride=0,format=Enum_wl_shm_formatargb8888}
instance DefaultIO WL_region where
  defM = pure WL_region {wlid=0}
instance DefaultIO WL_callback where
  defM = newEmptyMVar <&> WL_callback 0
instance DefaultIO WL_compositor where
  defM = pure WL_compositor {wlid=0}
instance DefaultIO WL_shm_pool where
  defM = newIORef 0 <&> WL_shm_pool 0 0
instance DefaultIO WL_shm where
  defM = newIORef [] <&> WL_shm 0
instance DefaultIO WL_surface where
  defM = pure WL_surface {wlid=0}

instance DefaultIO WL_data_offer where
  defM = pure WL_data_offer {wlid=0}
instance DefaultIO WL_data_device where
  defM = pure WL_data_device {wlid=0}
instance DefaultIO WL_data_device_manager where
  defM = pure WL_data_device_manager {wlid=0}
instance DefaultIO WL_data_source where
  defM = pure WL_data_source {wlid=0}
instance DefaultIO WL_shell where
  defM = pure WL_shell {wlid=0}
instance DefaultIO WL_shell_surface where
  defM = pure WL_shell_surface {wlid=0}
instance DefaultIO WL_seat where
  defM = pure WL_seat {wlid=0}
instance DefaultIO WL_pointer where
  defM = pure WL_pointer {wlid=0}
instance DefaultIO WL_keyboard where
  defM = pure WL_keyboard {wlid=0}
instance DefaultIO WL_touch where
  defM = pure WL_touch {wlid=0}
instance DefaultIO WL_output where
  defM = pure WL_output {wlid=0}
instance DefaultIO WL_subcompositor where
  defM = pure WL_subcompositor {wlid=0}
instance DefaultIO WL_subsurface where
  defM = pure WL_subsurface {wlid=0}
instance DefaultIO WL_fixes where
  defM = pure WL_fixes {wlid=0}

-- }}}

-- Tables {{{
$(generateTables False (\(x1:x2:xs) -> toUpper x1:toUpper x2:xs) "protocols/wayland.xml")
-- }}}

-- Interface Implementations {{{

-- WL_display {{{
instance Interface' WL_display Client where
  type Event WL_display = Event_wl_display
  type Request WL_display = Request_wl_display
  runEvent _display Event_wl_display_delete_id{id = did} = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.delete did)
  runEvent _display Event_wl_display_error{object_id, code, message} = do
    liftIO $ print $ "Unhandled error from `" <> show object_id <> "`: [" <> show code <> "] " <> message
  runRequest display request@Request_wl_display_sync{callback} = do
    mvar <- newEmptyMVar
    callbackObject <- newObject callback WL_callback {wlid=callback, done=mvar}
    swapMVar callbackObject.done ()
    sendMessage' request display.wlid (getOpcode request) $ runPut $ putEvent nodata request
  runRequest display request@Request_wl_display_get_registry{registry} = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.insert registry $ Interface $ WL_registry {wlid=registry})
    let body = runPut $ putEvent nodata request
    sendMessage' request display.wlid (getOpcode request) body
-- }}}

-- WL_callback {{{
instance Interface' WL_callback Client where
  type Event WL_callback = Event_wl_callback
  type Request WL_callback = Request_wl_callback
  runEvent callback Event_wl_callback_done{callback_data} = do
    ClientEnv env <- ask
    putMVar callback.done ()
    modifyIORef env.objects (Map.delete callback.wlid)

  runRequest _ _ = pure ()
instance Interface' WL_callback Server where
  type Event WL_callback = Event_wl_callback
  type Request WL_callback = Request_wl_callback
  runEvent callback Event_wl_callback_done{callback_data} = do
    putMVar callback.done ()
    ServerEnv senv <- ask
    clients <- readIORef senv.clients
    let env = fromJust $ senv.attached >>= (`Map.lookup` clients)
    modifyIORef env.objects $ Map.delete callback.wlid
  runRequest _ _ = pure ()
 
-- }}}

-- WL_registry {{{


instance Interface' WL_registry Client where
  type Event WL_registry = Event_wl_registry
  type Request WL_registry = Request_wl_registry
  runEvent registry Event_wl_registry_global{name,interface,version} = do
    ClientEnv env <- ask
    let interface' = BS.init interface
    modifyIORef env.globals $ BM.insert interface' name
    vertable <- readIORef env.versionTable
    case Map.lookup (BS8.unpack interface') vertable of
      Just clientVer -> do
        when (clientVer > version) $
          modifyIORef env.versionTable $ Map.insert (BS8.unpack interface') version
      Nothing -> pure ()

  runEvent registry Event_wl_registry_global_remove{name} = do
    ClientEnv env <- ask
    modifyIORef env.globals $ BM.deleteR name


  runRequest registry request@Request_wl_registry_bind{name, id = (interfaceName, interfaceVersion, newId)} = do
    ClientEnv env <- ask
    interfaceFromName name >>= \case
      Just x -> do
        y' <- fromJust . Map.lookup (BS8.unpack x) <$> readIORef env.interfaceTable
        Interface y <- liftIO y'
        modifyIORef env.objects . Map.insert newId $ Interface $ y & wlid .~ newId
      Nothing -> error $ "interface with name `" <> show name <> "` not found."
    sendMessage' request registry.wlid  (getOpcode request) $ runPut $ putEvent nodata request


-- }}}

-- WL_compositor {{{

instance Interface' WL_compositor Client where
  type Event WL_compositor = Event_wl_compositor
  type Request WL_compositor = Request_wl_compositor
  runRequest compositor request@Request_wl_compositor_create_surface {id=surfaceId} = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.insert surfaceId $ Interface $ WL_surface {wlid=surfaceId}
    sendMessage' request compositor.wlid  (getOpcode request) $ runPut $ putEvent nodata request

  runRequest compositor request@Request_wl_compositor_create_region {id=regionId} = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.insert regionId $ Interface $ WL_region {wlid=regionId}
    sendMessage' request compositor.wlid (getOpcode request) $ runPut $ putEvent nodata request

  runRequest compositor request@Request_wl_compositor_release = do
    sendMessage' request compositor.wlid (getOpcode request) $ runPut $ putEvent nodata request

-- }}}

-- WL_shm_pool {{{
instance Interface' WL_shm_pool Client where
  type Event WL_shm_pool = Event_wl_shm_pool
  type Request WL_shm_pool = Request_wl_shm_pool

  runRequest shm_pool request@Request_wl_shm_pool_create_buffer{id = bufId, offset=offset', width=width', height=height', stride=stride', format=format'} = do
    ClientEnv env <- ask
    let buffer = WL_buffer {wlid = bufId, offset = offset', width = width', height = height', stride = stride', format = enum_wl_shm_format' format'}
    modifyIORef env.objects $ Map.insert bufId $ Interface buffer
    sendMessage' request shm_pool.wlid (getOpcode request) $ runPut $ putEvent nodata request

  runRequest shm_pool request@Request_wl_shm_pool_destroy = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete shm_pool.wlid
    sendMessage' request shm_pool.wlid (getOpcode request) $ runPut $ putEvent nodata request

  runRequest shm_pool request@Request_wl_shm_pool_resize{size} = do
    writeIORef shm_pool.size size
    sendMessage' request shm_pool.wlid (getOpcode request) $ runPut $ putEvent nodata request

  runEvent _ _ = pure ()
-- }}}

-- WL_shm {{{
instance Interface' WL_shm Client where
  type Event WL_shm = Event_wl_shm
  type Request WL_shm = Request_wl_shm
  runRequest shm request@Request_wl_shm_create_pool{id=poolId, fd=fd', size=size'} = do
    ClientEnv env <- ask
    sizeRef <- newIORef size'
    modifyIORef env.objects $ Map.insert poolId $ Interface $ WL_shm_pool {wlid=poolId, fd=fd', size=sizeRef}
    sendMessageWithFds' request [fd'] shm.wlid (getOpcode request) $ runPut $ putEvent (AdditionalParserData [fd']) request

  runRequest shm request@Request_wl_shm_release{} = do
    ClientEnv env <- ask
    modifyIORef env.objects $ Map.delete shm.wlid
    sendMessage' request shm.wlid (getOpcode request) $ runPut $ putEvent nodata request


  runEvent shm Event_wl_shm_format{format} = modifyIORef shm.formats (fromIntegral format:)
-- }}}

-- WL_buffer {{{
instance Interface' WL_buffer Client where
  type Event WL_buffer = Event_wl_buffer
  type Request WL_buffer = Request_wl_buffer
  runRequest buffer Request_wl_buffer_destroy{} = pure ()
  runEvent buffer Event_wl_buffer_release{} = pure ()
-- }}}

-- wl_data_offer {{{
instance Interface' WL_data_offer Client where
  type Event WL_data_offer = Event_wl_data_offer
  type Request WL_data_offer = Request_wl_data_offer
  runRequest _ Request_wl_data_offer_accept{} = pure ()
  runRequest _ Request_wl_data_offer_receive{} = pure ()
  runRequest _ Request_wl_data_offer_destroy{} = pure ()
  runRequest _ Request_wl_data_offer_finish{} = pure ()
  runRequest _ Request_wl_data_offer_set_actions{} = pure ()
  runEvent _ Event_wl_data_offer_offer{} = pure ()
  runEvent _ Event_wl_data_offer_source_actions{} = pure ()
  runEvent _ Event_wl_data_offer_action{} = pure ()
--}}}

-- wl_data_source {{{
instance Interface' WL_data_source Client where
  type Event WL_data_source = Event_wl_data_source
  type Request WL_data_source = Request_wl_data_source
  runRequest _ Request_wl_data_source_offer{} = pure ()
  runRequest _ Request_wl_data_source_destroy{} = pure ()
  runRequest _ Request_wl_data_source_set_actions{} = pure ()
  runEvent _ Event_wl_data_source_target{} = pure ()
  runEvent _ Event_wl_data_source_send{} = pure ()
  runEvent _ Event_wl_data_source_cancelled{} = pure ()
  runEvent _ Event_wl_data_source_dnd_drop_performed{} = pure ()
  runEvent _ Event_wl_data_source_dnd_finished{} = pure ()
  runEvent _ Event_wl_data_source_action{} = pure ()
--}}}

-- wl_data_device {{{
instance Interface' WL_data_device Client where
  type Event WL_data_device = Event_wl_data_device
  type Request WL_data_device = Request_wl_data_device
  runRequest _ Request_wl_data_device_start_drag{} = pure ()
  runRequest _ Request_wl_data_device_set_selection{} = pure ()
  runRequest _ Request_wl_data_device_release{} = pure ()
  runEvent _ Event_wl_data_device_data_offer{} = pure ()
  runEvent _ Event_wl_data_device_enter{} = pure ()
  runEvent _ Event_wl_data_device_leave{} = pure ()
  runEvent _ Event_wl_data_device_motion{} = pure ()
  runEvent _ Event_wl_data_device_drop{} = pure ()
  runEvent _ Event_wl_data_device_selection{} = pure ()
--}}}

-- wl_data_device_manager {{{
instance Interface' WL_data_device_manager Client where
  type Event WL_data_device_manager = Event_wl_data_device_manager
  type Request WL_data_device_manager = Request_wl_data_device_manager
  runRequest _ Request_wl_data_device_manager_create_data_source{} = pure ()
  runRequest _ Request_wl_data_device_manager_get_data_device{} = pure ()
  runRequest _ Request_wl_data_device_manager_release{} = pure ()
--}}}

-- wl_shell {{{
instance Interface' WL_shell Client where
  type Event WL_shell = Event_wl_shell
  type Request WL_shell = Request_wl_shell
  runRequest _ Request_wl_shell_get_shell_surface{} = pure ()
--}}}

-- wl_shell_surface {{{
instance Interface' WL_shell_surface Client where
  type Event WL_shell_surface = Event_wl_shell_surface
  type Request WL_shell_surface = Request_wl_shell_surface
  runRequest _ Request_wl_shell_surface_pong{} = pure ()
  runRequest _ Request_wl_shell_surface_move{} = pure ()
  runRequest _ Request_wl_shell_surface_resize{} = pure ()
  runRequest _ Request_wl_shell_surface_set_toplevel{} = pure ()
  runRequest _ Request_wl_shell_surface_set_transient{} = pure ()
  runRequest _ Request_wl_shell_surface_set_fullscreen{} = pure ()
  runRequest _ Request_wl_shell_surface_set_popup{} = pure ()
  runRequest _ Request_wl_shell_surface_set_maximized{} = pure ()
  runRequest _ Request_wl_shell_surface_set_title{} = pure ()
  runRequest _ Request_wl_shell_surface_set_class{} = pure ()
  runEvent _ Event_wl_shell_surface_ping{} = pure ()
  runEvent _ Event_wl_shell_surface_configure{} = pure ()
  runEvent _ Event_wl_shell_surface_popup_done{} = pure ()
--}}}

-- wl_surface {{{
instance Interface' WL_surface Client where
  type Event WL_surface = Event_wl_surface
  type Request WL_surface = Request_wl_surface
  runRequest _ Request_wl_surface_destroy{} = pure ()
  runRequest surface request@Request_wl_surface_attach{buffer=bufferId, x, y} = do
    sendMessage' request surface.wlid (getOpcode request) $ runPut $ putEvent nodata request
  runRequest _ Request_wl_surface_damage{} = pure ()
  runRequest _ Request_wl_surface_frame{} = pure ()
  runRequest _ Request_wl_surface_set_opaque_region{} = pure ()
  runRequest _ Request_wl_surface_set_input_region{} = pure ()
  runRequest surface request@Request_wl_surface_commit{} = do
    sendMessage' request surface.wlid (getOpcode request) $ runPut $ putEvent nodata request
  runRequest _ Request_wl_surface_set_buffer_transform{} = pure ()
  runRequest _ Request_wl_surface_set_buffer_scale{} = pure ()
  runRequest surface request@Request_wl_surface_damage_buffer{} = do
    sendMessage' request surface.wlid (getOpcode request) $ runPut $ putEvent nodata request
  runRequest _ Request_wl_surface_offset{} = pure ()
  runRequest _ Request_wl_surface_get_release{} = pure ()
  runEvent _ Event_wl_surface_enter{} = pure ()
  runEvent _ Event_wl_surface_leave{} = pure ()
  runEvent _ Event_wl_surface_preferred_buffer_scale{} = pure ()
  runEvent _ Event_wl_surface_preferred_buffer_transform{} = pure ()
-- }}}


-- wl_seat {{{
instance Interface' WL_seat Client where
  type Event WL_seat = Event_wl_seat
  type Request WL_seat = Request_wl_seat
  runRequest _ Request_wl_seat_get_pointer{} = pure ()
  runRequest _ Request_wl_seat_get_keyboard{} = pure ()
  runRequest _ Request_wl_seat_get_touch{} = pure ()
  runRequest _ Request_wl_seat_release{} = pure ()
  runEvent _ Event_wl_seat_capabilities{} = pure ()
  runEvent _ Event_wl_seat_name{} = pure ()
--}}}
-- wl_pointer {{{
instance Interface' WL_pointer Client where
  type Event WL_pointer = Event_wl_pointer
  type Request WL_pointer = Request_wl_pointer
  runRequest _ Request_wl_pointer_set_cursor{} = pure ()
  runRequest _ Request_wl_pointer_release{} = pure ()
  runEvent _ Event_wl_pointer_enter{} = pure ()
  runEvent _ Event_wl_pointer_leave{} = pure ()
  runEvent _ Event_wl_pointer_motion{} = pure ()
  runEvent _ Event_wl_pointer_button{} = pure ()
  runEvent _ Event_wl_pointer_axis{} = pure ()
  runEvent _ Event_wl_pointer_frame{} = pure ()
  runEvent _ Event_wl_pointer_axis_source{} = pure ()
  runEvent _ Event_wl_pointer_axis_stop{} = pure ()
  runEvent _ Event_wl_pointer_axis_discrete{} = pure ()
  runEvent _ Event_wl_pointer_axis_value120{} = pure ()
  runEvent _ Event_wl_pointer_axis_relative_direction{} = pure ()
--}}}
-- wl_keyboard {{{
instance Interface' WL_keyboard Client where
  type Event WL_keyboard = Event_wl_keyboard
  type Request WL_keyboard = Request_wl_keyboard
  runRequest _ Request_wl_keyboard_release{} = pure ()
  runEvent _ Event_wl_keyboard_keymap{} = pure ()
  runEvent _ Event_wl_keyboard_enter{} = pure ()
  runEvent _ Event_wl_keyboard_leave{} = pure ()
  runEvent _ Event_wl_keyboard_key{} = pure ()
  runEvent _ Event_wl_keyboard_modifiers{} = pure ()
  runEvent _ Event_wl_keyboard_repeat_info{} = pure ()
--}}}
-- (getOpcode request) wl_touch {{{
instance Interface' WL_touch Client where
  type Event WL_touch = Event_wl_touch
  type Request WL_touch = Request_wl_touch
  runRequest _ Request_wl_touch_release{} = pure ()
  runEvent _ Event_wl_touch_down{} = pure ()
  runEvent _ Event_wl_touch_up{} = pure ()
  runEvent _ Event_wl_touch_motion{} = pure ()
  runEvent _ Event_wl_touch_frame{} = pure ()
  runEvent _ Event_wl_touch_cancel{} = pure ()
  runEvent _ Event_wl_touch_shape{} = pure ()
  runEvent _ Event_wl_touch_orientation{} = pure ()
--}}}
-- wl_output {{{
instance Interface' WL_output Client where
  type Event WL_output = Event_wl_output
  type Request WL_output = Request_wl_output
  runRequest _ Request_wl_output_release{} = pure ()
  runEvent _ Event_wl_output_geometry{} = pure ()
  runEvent _ Event_wl_output_mode{} = pure ()
  runEvent _ Event_wl_output_done{} = pure ()
  runEvent _ Event_wl_output_scale{} = pure ()
  runEvent _ Event_wl_output_name{} = pure ()
  runEvent _ Event_wl_output_description{} = pure ()
--}}}
-- WL_region {{{
instance Interface' WL_region Client where
  type Event WL_region = Event_wl_region
  type Request WL_region = Request_wl_region
  runRequest _ Request_wl_region_destroy{} = pure ()
  runRequest _ Request_wl_region_add{} = pure ()
  runRequest _ Request_wl_region_subtract{} = pure ()
--}}}
-- WL_subcompositor {{{
instance Interface' WL_subcompositor Client where
  type Event WL_subcompositor = Event_wl_subcompositor
  type Request WL_subcompositor = Request_wl_subcompositor
  runRequest _ Request_wl_subcompositor_destroy{} = pure ()
  runRequest _ Request_wl_subcompositor_get_subsurface{} = pure ()
--}}}
-- WL_subsurface {{{
instance Interface' WL_subsurface Client where
  type Event WL_subsurface = Event_wl_subsurface
  type Request WL_subsurface = Request_wl_subsurface
  runRequest _ Request_wl_subsurface_destroy{} = pure ()
  runRequest _ Request_wl_subsurface_set_position{} = pure ()
  runRequest _ Request_wl_subsurface_place_above{} = pure ()
  runRequest _ Request_wl_subsurface_place_below{} = pure ()
  runRequest _ Request_wl_subsurface_set_sync{} = pure ()
  runRequest _ Request_wl_subsurface_set_desync{} = pure ()
--}}}
-- WL_fixes {{{
instance Interface' WL_fixes Client where
  type Event WL_fixes = Event_wl_fixes
  type Request WL_fixes = Request_wl_fixes
  runRequest _ Request_wl_fixes_destroy{} = pure ()
  runRequest _ Request_wl_fixes_destroy_registry{} = pure ()
--}}}

-- }}}

-- Wrapper Functions, for QoL
bindToInterface :: WL_registry -> BS.ByteString -> Wayland Client (Maybe Word32)
bindToInterface registry intName = go 1
  where
    go :: Int -> Wayland Client (Maybe Word32)
    go count = do
      when (count >= 10)
        (putTextLn ("ERROR: the wayland global " <> show intName <> " not found") >> exitFailure) -- maybe return Nothing here?
      putTextLn $ mconcat ["Trying to bind to", show intName, "... (", show count, ")"]
      ClientEnv env <- ask
      glob <- BM.lookup intName <$> readIORef env.globals
      case glob of
        Just x -> do
          new_id <- newObjectId
          ver <- fromJust . Map.lookup (BS8.unpack intName) <$> readIORef env.versionTable
          runRequest registry Request_wl_registry_bind {name=x, id=(intName,ver,new_id)}
          pure $ Just new_id
        Nothing -> liftIO (threadDelay $ 100 * 1000) >> go (count + 1)

-- vim: foldmethod=marker

