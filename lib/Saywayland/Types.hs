{-# LANGUAGE TemplateHaskell #-}

module Saywayland.Types where

import Data.Binary
import Data.Binary.Get hiding (remaining)
import Data.Binary.Put
import Data.Bits
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Internal qualified as BSL
import Data.Map qualified as Map
import GHC.Show qualified as GHC
import Network.Socket (Socket)
import Relude hiding (ByteString, get, put)
import SaywaylandTH

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
data WaylandEnv
  = ClientEnv
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
  | ServerEnv
      { serverSocket :: Socket
      , clients :: [WaylandEnv]
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
