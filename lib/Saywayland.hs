{-# OPTIONS_GHC -Wno-missing-export-lists #-}

{- HLINT ignore "Use camelCase" -}

{- |
Module      : Saywayland
Description : Module containing all of Saywayland except the template haskell.

The module contains events and requests which directly represent wayland ones.
Look for the documentation in Wayland docs, for example in <https://wayland.app/protocols/>.
The naming scheme for them is \<interface\>_\<method\>
-}
module Saywayland where

import Control.Concurrent (threadDelay)
import Data.Binary
import Data.Binary.Get hiding (remaining)
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), connect, defaultProtocol, socket)
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Relude hiding (ByteString, get, put)
import Saywayland.Types
import Saywayland.WaylandSocket (getSocketPath, openSocket)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)
import System.Posix.Types (Fd)
import Saywayland.Interfaces

-- Requests {{{

{-

-- | https://wayland.app/protocols/wayland#wl_shm:request:create_pool
wlShm_createPool :: ObjectID 'WlShm -> Fd -> WlInt -> Wayland (ObjectID 'WlShmPool)
wlShm_createPool wlShmID fileDescriptor poolSize = do
  env <- ask
  newObjectID <- nextID env.counter
  let messageBody = runPut $ do
        put newObjectID
        put poolSize
  let msg = BS.toStrict $ mkMessage wlShmID 0 messageBody
  liftIO $ sendManyWithFds env.socket [msg] [fileDescriptor]

  let sender = ("wl_shm", wlShmID, "create_pool")
  liftIO . strReq sender $ mconcat ["newID=", show newObjectID, " fd=", show fileDescriptor, " size=", show poolSize]
  modifyIORef env.objects (Map.insert newObjectID WlShmPool)
  return $ coerce newObjectID

-- | https://wayland.app/protocols/wayland#wl_surface:request:attach
wlSurface_attach :: ObjectID 'WlSurface -> ObjectID 'WlBuffer -> Wayland ()
wlSurface_attach wlSurfaceID wlBufferID = do
  let messageBody = runPut $ do
        put wlBufferID
        -- x y arguments have to be set to 0
        put (0 :: WlInt)
        put (0 :: WlInt)
  sendMessage wlSurfaceID 1 messageBody
  pure ()

-- | https://wayland.app/protocols/wayland#wl_surface:request:damage_buffer
wlSurface_damageBuffer :: ObjectID 'WlSurface -> WlInt -> WlInt -> WlInt -> WlInt -> Wayland ()
wlSurface_damageBuffer wlSurfaceID x y width height = do
  let messageBody = runPut $ do
        put x
        put y
        put width
        put height
  sendMessage wlSurfaceID 9 messageBody
  pure ()

-- | https://wayland.app/protocols/wayland#wl_surface:request:commit
wlSurface_commit :: ObjectID 'WlSurface -> Wayland ()
wlSurface_commit wlSurfaceID = do
  let messageBody = runPut mempty
  sendMessage wlSurfaceID 6 messageBody
  pure ()

-- | https://wayland.app/protocols/wlr-layer-shell-unstable-v1#zwlr_layer_shell_v1:request:get_layer_surface
zwlrLayerShellV1_getLayerSurface :: ObjectID 'ZwlrLayerShellV1 -> ObjectID 'WlSurface -> WlUint -> WlString -> Wayland (ObjectID 'ZwlrLayerSurfaceV1)
zwlrLayerShellV1_getLayerSurface zwlrLayerShellV1ID wlSurfaceID layer namespace = do
  env <- ask
  newObjectID <- nextID env.counter
  let messageBody = runPut $ do
        put newObjectID
        put wlSurfaceID
        put waylandNull
        put layer
        put namespace
  sendMessage zwlrLayerShellV1ID 0 messageBody

  let sender = ("zwlr_layer_shell_v1", zwlrLayerShellV1ID, "get_layer_surface")
  liftIO
    . strReq sender
    $ mconcat
      [ "newID="
      , show newObjectID
      , " wl_surface="
      , show wlSurfaceID
      , " output="
      , show waylandNull
      , " layer="
      , show layer
      , " namespace="
      , show namespace
      ]

  modifyIORef env.objects (Map.insert newObjectID ZwlrLayerSurfaceV1)
  return $ coerce newObjectID

-- | https://wayland.app/protocols/wlr-layer-shell-unstable-v1#zwlr_layer_surface_v1:request:set_anchor
zwlrLayerSurfaceV1_setAnchor :: ObjectID 'ZwlrLayerSurfaceV1 -> WlUint -> Wayland ()
zwlrLayerSurfaceV1_setAnchor zwlrLayerSurfaceV1ID anchor = do
  let messageBody = runPut $ put anchor
  sendMessage zwlrLayerSurfaceV1ID 1 messageBody
  let sender = ("zwlr_layer_surface_v1", zwlrLayerSurfaceV1ID, "set_anchor")
  liftIO . strReq sender $ "anchor=" <> show anchor

-- | https://wayland.app/protocols/wlr-layer-shell-unstable-v1#zwlr_layer_surface_v1:request:set_size
zwlrLayerSurfaceV1_setSize :: ObjectID 'ZwlrLayerSurfaceV1 -> WlUint -> WlUint -> Wayland ()
zwlrLayerSurfaceV1_setSize zwlrLayerSurfaceV1ID width height = do
  let messageBody = runPut $ do
        put width
        put height
  sendMessage zwlrLayerSurfaceV1ID 0 messageBody
  let sender = ("zwlr_layer_surface_v1", zwlrLayerSurfaceV1ID, "set_size")
  liftIO . strReq sender $ mconcat ["width=", show width, " height=", show height]

-- | https://wayland.app/protocols/wlr-layer-shell-unstable-v1#zwlr_layer_surface_v1:request:ack_configure
zwlrLayerSurfaceV1_ackConfigure :: ObjectID 'ZwlrLayerSurfaceV1 -> WlUint -> Wayland ()
zwlrLayerSurfaceV1_ackConfigure zwlrLayerSurfaceV1ID serial = do
  let messageBody = runPut $ put serial
  sendMessage zwlrLayerSurfaceV1ID 6 messageBody
  let sender = ("zwlr_layer_surface_v1", zwlrLayerSurfaceV1ID, "ack_configure")
  liftIO . strReq sender $ "serial=" <> show serial

-- | https://wayland.app/protocols/wlr-layer-shell-unstable-v1#zwlr_layer_surface_v1:request:set_exclusive_zone
zwlrLayerSurfaceV1_setExclusiveZone :: ObjectID 'ZwlrLayerSurfaceV1 -> WlInt -> Wayland ()
zwlrLayerSurfaceV1_setExclusiveZone zwlrLayerSurfaceV1ID zone = do
  let messageBody = runPut $ put zone
  sendMessage zwlrLayerSurfaceV1ID 2 messageBody
  let sender = ("zwlr_layer_surface_v1", zwlrLayerSurfaceV1ID, "set_exclusive_zone")
  liftIO . strReq sender $ "zone=" <> show zone

-- | https://wayland.app/protocols/wayland#wl_shm_pool:request:create_buffer
wlShmPool_createBuffer :: ObjectID 'WlShmPool -> WlInt -> WlInt -> WlInt -> WlInt -> WlColorFormat -> Wayland Buffer
wlShmPool_createBuffer wlShmPoolID offset bufferWidth bufferHeight colorChannels colorFormat = do
  env <- ask
  newObjectID <- nextID env.counter
  let messageBody = runPut $ do
        put newObjectID
        put offset
        put bufferWidth
        put bufferHeight
        put (bufferWidth * colorChannels) -- Stride
        put colorFormat
  sendMessage wlShmPoolID 0 messageBody
  let sender = ("wl_shm_pool", wlShmPoolID, "create_buffer")
  liftIO . strReq sender $ mconcat ["newID=", show newObjectID]
  modifyIORef env.objects (Map.insert newObjectID WlBuffer)
  return $ Buffer (coerce newObjectID) offset

-- | https://wayland.app/protocols/wayland#wl_registry:request:bind
wlRegistry_bind :: ObjectID 'WlRegistry -> WaylandInterface -> WlUint -> WlString -> WlUint -> WlID -> Wayland (ObjectID a)
wlRegistry_bind registryID waylandInterface globalName interfaceName interfaceVersion newObjectID = do
  env <- ask
  let messageBody = runPut $ do
        put globalName
        put interfaceName
        put interfaceVersion
        put newObjectID
  sendMessage registryID 0 messageBody
  let sender = ("wl_registry", registryID, "bind")
  liftIO
    . strReq sender
    $ mconcat
      [ "name="
      , show globalName
      , " interface="
      , show interfaceName
      , " version="
      , show interfaceVersion
      , "id="
      , show newObjectID
      ]

  modifyIORef env.objects (Map.insert newObjectID waylandInterface)
  return $ coerce newObjectID

-- | https://wayland.app/protocols/wayland#wl_compositor:request:create_surface
wlCompositor_createSurface :: ObjectID 'WlCompositor -> Wayland (ObjectID 'WlSurface)
wlCompositor_createSurface wlCompositorID = do
  env <- ask
  newObjectID <- nextID env.counter
  let messageBody = runPut $ put newObjectID
  sendMessage wlCompositorID 0 messageBody
  let sender = ("wl_compositor", wlCompositorID, "create_surface")
  liftIO . strReq sender $ mconcat ["newID=", show newObjectID]
  modifyIORef env.objects (Map.insert newObjectID WlSurface)
  return $ coerce newObjectID

-- }}}

-- Event helpers {{{

-- | Format a received event as a pretty string.
formatEvent :: WaylandEvent -> Text
formatEvent = \case
  EvWlDisplay_error h e -> fmt "wl_display" h $ "error: object_id=" <> show e.errorObjectID <> " code=" <> show e.errorCode <> " message=" <> show e.errorMessage
  EvWlDisplay_deleteID h e -> fmt "wl_display" h $ "delete_id: id=" <> show e.deletedID
  EvWlRegistry_global h e -> fmt "wl_registry" h $ "global: name=" <> show e.name <> " interface=" <> show e.interface <> " version=" <> show e.version
  EvWlShm_format h e -> fmt "wl_shm" h $ "format: format=" <> show e.format
  EvZwlrLayerSurfaceV1_configure h e -> fmt "zwlr_layer_surface_v1" h $ "configure: serial=" <> show e.serial <> " width=" <> show e.width <> " height=" <> show e.height
  EvExtWorkspaceManagerV1_workspace h e -> fmt "ext_workspace_manager_v1" h $ "workspace: handle=" <> show e.handleID
  EvExtWorkspaceManagerV1_workspaceGroup h e -> fmt "ext_workspace_manager_v1" h $ "workspace_group: handle=" <> show e.handleID
  EvExtWorkspaceManagerV1_done h _ -> fmt "ext_workspace_manager_v1" h "done: done sending workspace info"
  EvExtWorkspaceHandleV1_id h e -> fmt "ext_workspace_handle_v1" h $ "id: id=" <> show e.id
  EvExtWorkspaceHandleV1_name h e -> fmt "ext_workspace_handle_v1" h $ "name: name=" <> show e.name
  EvExtWorkspaceHandleV1_coordinates h e -> fmt "ext_workspace_handle_v1" h $ "coordinates: coordinates=" <> show e.coordinates
  EvExtWorkspaceHandleV1_state h e -> fmt "ext_workspace_handle_v1" h $ "state: state=" <> show e.state
  EvExtWorkspaceHandleV1_capabilities h e -> fmt "ext_workspace_handle_v1" h $ "capabilities: capabilities=" <> show e.capabilities
  EvExtWorkspaceHandleV1_removed h _ -> fmt "ext_workspace_handle_v1" h "removed: removed"
  EvExtWorkspaceGroupHandleV1_capabilities h e -> fmt "ext_workspace_group_handle_v1" h $ "capabilities: capabilities=" <> show e.capabilities
  EvExtWorkspaceGroupHandleV1_output_enter h e -> fmt "ext_workspace_group_handle_v1" h $ "output_enter: output=" <> show e.output
  EvExtWorkspaceGroupHandleV1_output_leave h e -> fmt "ext_workspace_group_handle_v1" h $ "output_leave: output=" <> show e.output
  EvExtWorkspaceGroupHandleV1_workspace_enter h e -> fmt "ext_workspace_group_handle_v1" h $ "workspace_enter: workspace=" <> show e.workspace
  EvExtWorkspaceGroupHandleV1_workspace_leave h e -> fmt "ext_workspace_group_handle_v1" h $ "workspace_leave: workspace=" <> show e.workspace
  EvExtWorkspaceGroupHandleV1_removed h _ -> fmt "ext_workspace_group_handle_v1" h "removed: removed"
  EvWlBuffer_release h _ -> fmt "wl_buffer" h "release: buffer released"
  EvUnknown h -> "UNKNOWN EVENT: objectID=" <> show h.objectID <> " opCode=" <> show h.opCode <> " size=" <> show h.size
  where
    fmt :: Text -> Header -> Text -> Text
    fmt interface h details = interface <> "@" <> show h.objectID <> "." <> details
-}
-- }}}

-- Event loop {{{

-- | Core event loop. It reads the socket and processes the data using 'processBuffer'
{-
eventLoop :: Wayland ()
eventLoop = do
  env <- ask
  msg <- liftIO $ receiveSocketData env.socket
  unless (BSL.null msg) $ processBuffer (BS.toStrict msg)
  eventLoop

{- | Processes the data from the wayland socket.
It does nothing if the data is partial and waits for the next call.
If the data is not partial, it parses events with 'parseEvent'
then for each event it calls 'formatEvent' and 'handleEventResponse'
-}
processBuffer :: BS.ByteString -> Wayland ()
processBuffer bytes = do
  env <- ask
  objects <- liftIO $ readIORef env.objects
  case pushChunk (runGetIncremental (parseEvent objects)) bytes of
    Done remaining _ ev -> do
      liftIO $ case ev of
        EvWlBuffer_release _ _ -> pure ()
        _ -> displayEvent ev
      handleEventResponse ev
      unless (BS.null remaining) $ processBuffer remaining
    Partial _ ->
      return ()
    Fail _ _ err ->
      liftIO $ putTextLn $ "Parse error: " <> toText err
  where
    displayEvent :: WaylandEvent -> IO ()
    displayEvent ev = putTextLn $ "<- " <> formatEvent ev

{- | Event handler.
Assigns objects, globals and performs other actions based on events.
It also processes events through custom handlers defined by 'onEvent', after the internal ones.
-}
handleEventResponse :: WaylandEvent -> Wayland ()
handleEventResponse ev = do
  case ev of
    (EvWlRegistry_global h body) -> do
      globals <- asks (.globals)
      modifyIORef globals $ Map.insert body.name (h, body)
    (EvExtWorkspaceManagerV1_workspace _ body) -> do
      objects <- asks (.objects)
      modifyIORef objects $ Map.insert (coerce body.handleID) ExtWorkspaceHandleV1
    (EvExtWorkspaceManagerV1_workspaceGroup _ body) -> do
      objects <- asks (.objects)
      modifyIORef objects $ Map.insert (coerce body.handleID) ExtWorkspaceGroupHandleV1
    _ -> return ()

  handlers <- liftIO . readIORef =<< asks (.eventHandlers)
  mapM_ ($ ev) handlers

{- | Register a handler to be called on every incoming Wayland event.
Handlers are called in registration order after the library's own handlers.

Example:

> onEvent $ \case
>   EvExtWorkspaceHandleV1_name h e ->
>     liftIO $ modifyIORef myRef (Map.insert h.objectID (decodeUtf8 (unWlString e.name)))
>   _ -> pure ()
-}
onEvent :: (WaylandEvent -> Wayland ()) -> Wayland ()
onEvent handler = do
  handlersRef <- asks (.eventHandlers)
  liftIO $ modifyIORef handlersRef (handler :)

-- }}}

-- Utils {{{

{- | Bind to a Wayland interface. It looks at the advertised globals to find an interface.
If it fails it recurses. It does this 10 times with a small delay.
Prints an exception and exits with a fail if the interface is not found within 10 tries.
-}
{-
bindToInterface :: ObjectID 'WlRegistry -> IORef Globals -> WlString -> WaylandInterface -> Wayland (ObjectID a)
bindToInterface registryID globalsRef targetInterface waylandInterface =
  let go (count :: Int) = do
        when
          (count >= 10)
          (putTextLn ("ERROR: the wayland global " <> show targetInterface <> " not found") >> exitFailure)
        putTextLn $ mconcat ["Trying to bind to", show targetInterface, "... (", show count, ")"]
        env <- ask
        globals <- readIORef globalsRef
        case findInterface globals of
          Nothing -> liftIO (threadDelay $ 100 * 1000) >> go (count + 1)
          Just e -> do
            newObjectID <- liftIO $ nextID env.counter
            wlRegistry_bind registryID waylandInterface e.name targetInterface e.version newObjectID
   in go 1
  where
    findInterface :: Globals -> Maybe BodyWlRegistry_global
    findInterface globals =
      let target = targetInterface <> "\0"
       in find (\(_, e) -> target.unWlString `BSL.isPrefixOf` e.interface.unWlString) globals >>= Just . snd
-}

{- | Convert a WlString to text while stripping null terminators.
| Simply using 'show' does not strip null terminators.
-}
wlToText :: WlString -> Text
wlToText = decodeUtf8 . BSL.toStrict . BSL.takeWhile (/= 0) . (.unWlString)

{- | Connect to the Wayland socket.
The socket path is $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY
-}
connectToWlSocket :: IO Socket
connectToWlSocket = do
  path <- getSocketPath openSocket
  when (isNothing path) $ error "No valid wayland socket found."
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix $ fromJust path)
  return sock

-- | Receive data from the wayland socket.
receiveSocketData :: Socket -> IO BSL.ByteString
receiveSocketData sock = do
  liftIO $ recv sock 4096

{- | Generates a Wayland object ID from a counter.
It does this by incrementing the counter by 1.
-}
nextID :: (MonadIO m) => IORef WlID -> m WlID
nextID counter = do
  current <- readIORef counter
  modifyIORef counter (+ 1)
  return current

-- }}}
-}
-- vim: foldmethod=marker
