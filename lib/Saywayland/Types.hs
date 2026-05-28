{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Saywayland.Types where

import Control.Lens (Lens')
import Data.Bimap qualified as BM
import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Data (cast)
import Data.Map qualified as Map
import Debug.Trace (traceIO)
import Network.Socket (Socket)
import Network.Socket.ByteString (sendManyWithFds)
import Network.Socket.ByteString.Lazy (sendAll)
import Relude hiding (ByteString, get, put)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)
import System.Posix (Fd)

-- Constants {{{

-- | The header size is always 8 in Wayland.
headerSize :: Word16
headerSize = 8

-- | Constant representing the Wayland null, which is just 0.
waylandNull :: Word32
waylandNull = 0

-- | Constant representing the wl_display ID which is always 1 in Wayland.
wlDisplayID :: Word32
wlDisplayID = 1

-- | predefined empty AdditionalParserData
nodata :: AdditionalParserData
nodata = AdditionalParserData []

-- }}}

-- Types {{{
type ObjectID = Word32

type NewID = (BS.ByteString, Word32, ObjectID)

-- | HasWlid, a lens defined globally due to ID being a part of every wayland interface.
class HasWlid s a | s -> a where
  wlid :: Lens' s a

-- | A Default-like structure, but using IO
class DefaultIO a where
  defM :: IO a

-- | Perspective of the current Wayland Environment
data Perspective = Client | Server

-- | EventHandlers, called whenever an event is received | TODO: switch Client to p
data EventHandler where
  EventHandler :: (Typeable e, WaylandEvent e) => (e -> Wayland Client ()) -> EventHandler

-- | Wayland Environment

type role WaylandEnv nominal
data WaylandEnv (p :: Perspective) where
  ClientEnv :: ClientEnvironment Client -> WaylandEnv 'Client
  ServerEnv :: ServerEnvironment -> WaylandEnv 'Server

data ServerEnvironment = ServerEnvironment
  { socket :: Socket
  -- ^ global server socket
  , clients :: IORef (Map Int (ClientEnvironment Server))
  -- ^ currently connected clients
  , attached :: Maybe Int
  -- ^ Id of the currently attached client. Nothing if the env is global.
  }

type role ClientEnvironment nominal
data ClientEnvironment (p :: Perspective) = ClientEnvironment
  { socket :: Socket
  , counter :: IORef Word32
  , objects :: IORef (Map Word32 (Interface p))
  , globals :: IORef (BM.Bimap {-string name-} BS.ByteString {-global name-} Word32)
  , interfaceTable :: IORef (Map String (IO (Interface p)))
  , versionTable :: IORef (Map String Word32)
  , eventHandlers :: IORef [EventHandler]
  }

class
  ( WaylandEvent (Event a)
  , WaylandEvent (Request a)
  , HasWlid a ObjectID
  , Typeable a
  ) =>
  Interface' a (p :: Perspective)
  where
  type Event a
  type Request a
  runEvent :: a -> Event a -> Wayland p ()
  runRequest :: a -> Request a -> Wayland p ()

type role Interface nominal
data Interface (p :: Perspective) where
  Interface :: (Interface' i p, Typeable i) => i -> Interface p

class (Typeable e) => WaylandEvent e where
  getEvent :: Word16 -> AdditionalParserData -> Get e
  putEvent :: AdditionalParserData -> e -> Put
  getOpcode :: e -> Word16
  showEvent :: ObjectID -> e -> String

-- | Additional data passed to the TemplateHaskell-generated `getEvent`.
data AdditionalParserData = AdditionalParserData
  { fds :: [Fd]
  }

-- | The Wayland monad. Allows easy access to the Wayland environment state without threading repetitive arguments.
type Wayland p = ReaderT (WaylandEnv p) IO

-- }}}

-- Utils {{{

-- | function that increases the counter by 1 and returns it's new value
newObjectId :: Wayland p Word32
newObjectId = do
  ClientEnv env <- ask
  liftIO $ modifyIORef env.counter (+ 1)
  liftIO $ readIORef env.counter

-- | function that inserts the given interface to the objects map with provided id as key.
newObject :: (Interface' i 'Client) => Word32 -> i -> Wayland p i
newObject intId int = do
  ClientEnv env <- ask
  liftIO $ modifyIORef env.objects (Map.insert intId $ Interface int)
  pure int

{- | Convenience function for sending a Wayland message.
See 'mkMessage'.
-}
sendMessageWithFds :: [Fd] -> Word32 -> Word16 -> BSL.ByteString -> Wayland p ()
sendMessageWithFds fds objectID opcode messageBody = do
  ClientEnv env <- ask
  liftIO $ sendManyWithFds env.socket [BS.toStrict $ mkMessage objectID opcode messageBody] fds

sendMessage :: Word32 -> Word16 -> BSL.ByteString -> Wayland p ()
sendMessage objectID opcode messageBody = do
  ClientEnv env <- ask
  let msg = mkMessage objectID opcode messageBody
  liftIO . sendAll env.socket $ msg

{- | Convenience function for formatting events, before sending them.
Events are colored in magenta following the wayland.app colorscheme.
-}
sendMessage' :: (WaylandEvent e) => e -> Word32 -> Word16 -> BSL.ByteString -> Wayland p ()
sendMessage' e o op body = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Magenta $ showEvent o e)
  sendMessage o op body

-- | sendMessageWithFds, but with sendMessage' aspect.
sendMessageWithFds' :: (WaylandEvent e) => e -> [Fd] -> Word32 -> Word16 -> BSL.ByteString -> Wayland p ()
sendMessageWithFds' e fd o op body = do
  colorize <- liftIO getColorize
  liftIO (traceIO $ colorize Vivid Magenta $ showEvent o e)
  sendMessageWithFds fd o op body

{- | Convenience function for formatting a Wayland message.
It takes an objectID, operation code and a message body.
The header is generated based on this, the size is derived automatically.
-}
mkMessage :: Word32 -> Word16 -> BSL.ByteString -> BSL.ByteString
mkMessage objectID opcode messageBody =
  runPut $ do
    putWord32le objectID
    putWord16le opcode
    putWord16le $ 8 + fromIntegral (BSL.length messageBody)
    putLazyByteString messageBody

getColorize :: (IsString s, Semigroup s) => IO (ColorIntensity -> Color -> s -> s)
getColorize = do
  ansiSupport <- hNowSupportsANSI stdout
  pure
    $ if ansiSupport
      then \ci c t -> fromString (setSGRCode [SetColor Foreground ci c]) <> t <> fromString (setSGRCode [Reset])
      else const $ const id

-- | helper function for getting an object from a global
interfaceFromName :: Word32 -> Wayland p (Maybe BS.ByteString)
interfaceFromName n = do
  ClientEnv env <- ask
  glob <- readIORef env.globals
  pure $ BM.lookupR n glob

-- | get an Interface from objects table using its Id.
getInterface :: Word32 -> Wayland p (Maybe (Interface p))
getInterface objectID = do
  ClientEnv env <- ask
  Map.lookup objectID <$> readIORef env.objects

-- | getInterface chained with proxyInterface.
getInterface' :: forall i p. (Typeable i) => Proxy i -> Word32 -> Wayland p (Maybe i)
getInterface' p objectID = do
  ClientEnv env <- ask
  (proxyInterface p <=< Map.lookup objectID) <$> readIORef env.objects

-- | Round a byte length up to the nearest 4-byte boundary.
roundLength :: Word32 -> Int64
roundLength l = (fromIntegral l + 3) .&. (-4)

-- | Cast provided interface into proxied type.
proxyInterface :: forall i p. (Typeable i) => Proxy i -> Interface p -> Maybe i
proxyInterface _ (Interface i) = cast i

getServerClientEnv :: Wayland Server (Maybe (ClientEnvironment Server))
getServerClientEnv = do
  ServerEnv senv <- ask
  clients <- readIORef senv.clients
  pure $ senv.attached >>= (`Map.lookup` clients)


-- }}}

-- vim: foldmethod=marker
