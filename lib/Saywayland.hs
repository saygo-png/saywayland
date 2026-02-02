{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Internal qualified as BSL
import Data.Map qualified as Map
import GHC.Show qualified as GHC
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), Socket, SocketType (Stream), connect, defaultProtocol, socket)
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Relude hiding (ByteString, get, put)
import SaywaylandTH
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)
import System.Environment (getEnv)
import System.Posix.Types (Fd)

-- Types {{{

-- | Wayland Uint
newtype WlUint = WlUint {unWlUint :: Word32}
  deriving newtype (Eq, Ord, Num, Integral, Bits, Real, Enum, Show)

instance Binary WlUint where
  get = WlUint <$> getWord32le
  put (WlUint x) = putWord32le x

-- | Wayland object IDs are Uint.
type WlID = WlUint

-- | Wayland Int.
newtype WlInt = WlInt {unWlInt :: Int32}
  deriving newtype (Eq, Ord, Num, Integral, Bits, Real, Enum, Show)

instance Binary WlInt where
  get = WlInt <$> getInt32le
  put (WlInt x) = putInt32le x

-- | A Wayland length-prefixed, null-terminated, padded string on the wire.
newtype WlString = WlString {unWlString :: BSL.ByteString}
  deriving newtype (Eq, Ord, Semigroup, Monoid, IsString)

instance Show WlString where
  show = BSL.unpackChars . (.unWlString)

-- | Round a byte length up to the nearest 4-byte boundary.
roundLength :: Word32 -> Int64
roundLength l = (fromIntegral l + 3) .&. (-4)

instance Binary WlString where
  get = do
    len <- getWord32le
    str <- getLazyByteString (fromIntegral len)
    skip $ fromIntegral (roundLength len - fromIntegral len)
    return $ WlString str
  put (WlString bs) = do
    let str = bs <> "\0"
    putWord32le (fromIntegral $ BSL.length str)
    putLazyByteString str
    let paddingBytes = roundLength (fromIntegral $ BSL.length str) - BSL.length str
    replicateM_ (fromIntegral paddingBytes) (putWord8 0)

-- | Every interface that can appear as the object of a Wayland event.
data WaylandInterface
  = WlSurface
  | WlShmPool
  | WlBuffer
  | WlCompositor
  | ZwlrLayerSurfaceV1
  | ZwlrLayerShellV1
  | WlDisplay
  | WlRegistry
  | WlShm
  | WlOutput
  | ExtWorkspaceManagerV1
  | ExtWorkspaceHandleV1
  | ExtWorkspaceGroupHandleV1
  deriving stock (Show)

type role ObjectID phantom

-- | Phantom type representing an ID of an object.
newtype ObjectID (a :: WaylandInterface) = ObjectID {id :: WlUint}
  deriving newtype (Num, Show)

instance Binary (ObjectID a) where
  get = ObjectID <$> get
  put (ObjectID x) = put x

type role WlArray representational

-- | Wayland array type.
newtype WlArray a = WlArray [a]
  deriving stock (Show, Foldable)

-- | Little-endian length-prefixed array of Word32.
instance Binary (WlArray WlUint) where
  get = do
    len <- getWord32le
    bytes <- getLazyByteString $ roundLength len
    let elems = runGet (replicateM (fromIntegral len `div` 4) getWord32le) bytes
    return $ WlArray (coerce elems)
  put (WlArray xs) = do
    let len = fromIntegral (Relude.length xs * 4) :: Word32
    putWord32le len
    mapM_ putWord32le ((.unWlUint) <$> xs)

-- | Type representing Wayland color formats.
data WlColorFormat
  = Argb8888
  | Xrgb8888
  | UnknownColorFormat WlUint
  deriving stock (Eq, Ord)

instance Show WlColorFormat where
  show Argb8888 = "argb8888"
  show Xrgb8888 = "xrgb8888"
  show (UnknownColorFormat n) = "Unknown color format " <> show n

instance Binary WlColorFormat where
  put :: WlColorFormat -> Put
  put =
    put . \case
      Argb8888 -> 0 :: WlUint
      Xrgb8888 -> 1
      UnknownColorFormat u -> u

  get :: Get WlColorFormat
  get =
    get >>= \case
      (0 :: WlUint) -> pure Argb8888
      1 -> pure Xrgb8888
      n -> pure $ UnknownColorFormat n

$( declareEvents
     --
     [ event "WlDisplay" 0 "error" [("errorObjectID", ty ''WlUint), ("errorCode", ty ''WlUint), ("errorMessage", ty ''WlString)]
     , event "WlDisplay" 1 "deleteID" [("deletedID", ty ''WlUint)]
     , --
       event "WlRegistry" 0 "global" [("name", ty ''WlUint), ("interface", ty ''WlString), ("version", ty ''WlUint)]
     , --
       event "WlShm" 0 "format" [("format", ty ''WlUint)]
     , --
       event "ZwlrLayerSurfaceV1" 0 "configure" [("serial", ty ''WlUint), ("width", ty ''WlUint), ("height", ty ''WlUint)]
     , --
       event "ExtWorkspaceManagerV1" 0 "workspaceGroup" [("handleID", appTy ''ObjectID 'ExtWorkspaceGroupHandleV1)]
     , event "ExtWorkspaceManagerV1" 1 "workspace" [("handleID", appTy ''ObjectID 'ExtWorkspaceHandleV1)]
     , event "ExtWorkspaceManagerV1" 2 "done" []
     , --
       event "ExtWorkspaceHandleV1" 0 "id" [("id", ty ''WlString)]
     , event "ExtWorkspaceHandleV1" 1 "name" [("name", ty ''WlString)]
     , event "ExtWorkspaceHandleV1" 2 "coordinates" [("coordinates", appTy ''WlArray ''WlUint)]
     , event "ExtWorkspaceHandleV1" 3 "state" [("state", ty ''WlUint)]
     , event "ExtWorkspaceHandleV1" 4 "capabilities" [("capabilities", ty ''WlUint)]
     , event "ExtWorkspaceHandleV1" 5 "removed" []
     , --
       event "ExtWorkspaceGroupHandleV1" 0 "capabilities" [("capabilities", ty ''WlUint)]
     , event "ExtWorkspaceGroupHandleV1" 1 "output_enter" [("output", appTy ''ObjectID 'WlOutput)]
     , event "ExtWorkspaceGroupHandleV1" 2 "output_leave" [("output", appTy ''ObjectID 'WlOutput)]
     , event "ExtWorkspaceGroupHandleV1" 3 "workspace_enter" [("workspace", appTy ''ObjectID 'ExtWorkspaceHandleV1)]
     , event "ExtWorkspaceGroupHandleV1" 4 "workspace_leave" [("workspace", appTy ''ObjectID 'ExtWorkspaceHandleV1)]
     , event "ExtWorkspaceGroupHandleV1" 5 "removed" []
     , --
       event "WlBuffer" 0 "release" []
     ]
 )

-- | Globals storage by name.
type Globals = Map WlUint (Header, BodyWlRegistry_global)

{- | Record containing the essential state needed for Wayland.
The state contained is only essential, the user is expected to make their own structures
to store state required for their specific application. This can be done using event handlers made with 'onEvent'.
-}
data WaylandEnv = WaylandEnv
  { socket :: Socket
  -- ^ The connected UNIX socket.
  , counter :: IORef WlID
  -- ^ Counter used for generating unique object IDs.
  , globals :: IORef Globals
  -- ^ Map of received globals. Globals might be removed.
  , objects :: IORef (Map WlID WaylandInterface)
  -- ^ Map of existing objects. Objects might be removed.
  , eventHandlers :: IORef [WaylandEvent -> Wayland ()]
  -- ^ List of custom event handlers. Use 'onEvent' to add a custom handler.
  }

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland = ReaderT WaylandEnv IO

-- | Type representing a Wayland buffer.
data Buffer = Buffer
  { id :: ObjectID 'WlBuffer
  , offset :: WlInt
  -- ^ Memory offset of the buffer.
  }

-- | Type representing a Wayland header.
data Header = Header
  { objectID :: WlID
  , opCode :: Word16
  {- ^ Operation codes in Wayland are 0 indexed, separate for events and requests.
  They are numbered based on the order of appearance in the protocol.
  -}
  , size :: Word16
  }
  deriving stock (Show)

instance Binary Header where
  put :: Header -> Put
  put header = do
    put header.objectID
    putWord16le header.opCode
    putWord16le header.size
  get :: Get Header
  get = Header <$> get <*> getWord16le <*> getWord16le

instance ToText Header where
  toText :: Header -> Text
  toText (Header objectID opCode size) =
    mconcat ["-- wl_header: objectID=", show objectID, " opCode=", show opCode, " size=", show size]

-- }}}

-- Requests {{{

-- | https://wayland.app/protocols/wayland#wl_display:request:get_registry
wlDisplay_getRegistry :: Wayland (ObjectID 'WlRegistry)
wlDisplay_getRegistry = do
  env <- ask
  registryID <- nextID env.counter
  -- The object here is actually saved before the request is sent
  -- This is because the registry id is needed for parsing its requests/events
  modifyIORef env.objects (Map.insert registryID WlRegistry)
  let messageBody = runPut $ put registryID
  sendMessage wlDisplayID 1 messageBody
  liftIO . strReq ("wl_display", wlDisplayID, "get_registry") $ "wl_registry=" <> show registryID
  return $ coerce registryID

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

-- }}}

-- Event loop {{{

-- | Core event loop. It reads the socket and processes the data using 'processBuffer'
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

-- | The header size is always 8 in Wayland.
headerSize :: Word16
headerSize = 8

-- | Constant representing the Wayland null, which is just 0.
waylandNull :: WlUint
waylandNull = 0

-- | Constant representing the wl_display ID which is always 1 in Wayland.
wlDisplayID :: ObjectID 'WlDisplay
wlDisplayID = 1

{- | Convert a WlString to text while stripping null terminators.
| Simply using 'show' does not strip null terminators.
-}
wlToText :: WlString -> Text
wlToText = decodeUtf8 . BSL.toStrict . BSL.takeWhile (/= 0) . (.unWlString)

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

{- | Convenience function for formatting a Wayland message.
It takes an objectID, operation code and a message body.
The header is generated based on this, the size is derived automatically.
-}
mkMessage :: ObjectID a -> Word16 -> BSL.ByteString -> BSL.ByteString
mkMessage objectID opCode messageBody =
  runPut $ do
    put $ Header objectID.id opCode (headerSize + fromIntegral (BSL.length messageBody))
    putLazyByteString messageBody

{- | Convenience function for sending a Wayland message.
See 'mkMessage'.
-}
sendMessage :: ObjectID a -> Word16 -> BSL.ByteString -> Wayland ()
sendMessage objectID opCode messageBody = do
  wlSocket <- asks (.socket)
  liftIO . sendAll wlSocket $ mkMessage objectID opCode messageBody

{- | Connect to the Wayland socket.
The socket path is $XDG_RUNTIME_DIR/$WAYLAND_DISPLAY
-}
connectToWlSocket :: IO Socket
connectToWlSocket = do
  xdg_runtime_dir <- getEnv "XDG_RUNTIME_DIR"
  wayland_display <- getEnv "WAYLAND_DISPLAY"
  let path = xdg_runtime_dir <> "/" <> wayland_display
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock (SockAddrUnix path)
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

-- vim: foldmethod=marker
