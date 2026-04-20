module Saywayland.WaylandSocket (module Saywayland.WaylandSocket) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Control.Concurrent (forkIO)
import Data.Bool (bool)
import Data.Map qualified as Map
import Data.Bimap qualified as BM
import Data.String (IsString (fromString))
import Network.Socket
import Relude (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), newIORef, Word16, Word32, readIORef, LazyStrict (toLazy))
import Saywayland.Types
import System.Directory (doesFileExist)
import System.Environment.Blank (getEnv)
import System.FilePath
import Prelude

import Saywayland.Interfaces
import Network.Socket.ByteString (recvMsg)
import Foreign.C
import System.Posix (Fd (Fd))
import Foreign (Storable(sizeOf, peek), castPtr)
import GHC.IO (unsafePerformIO)
import Data.Binary.Get
import Data.Binary.Put (runPut, putWord16le)
import Protocol
import GHC.IORef (IORef(IORef))

-- Listeners {{{

-- | listen for client connections in provided socket.
listenForClients :: Socket -> Wayland Server ()
listenForClients sock = do
  env <- ask
  -- NO IMPL: should create WL_display on start.
  globalsref <- liftIO $ newIORef Map.empty -- Map.singleton 1 $ mkInterface WL_display
  serial <- liftIO $ newIORef 0
  objectsref <- liftIO $ newIORef Map.empty
  globalsref <- liftIO $ newIORef BM.empty
  handlers <- newIORef []
  let clientenv = ClientEnv $ ClientEnvironment{socket = sock, {-globals = globalsref,-} counter = serial, objects = objectsref{-, eventHandlers = handlers-}, globals = globalsref} :: WaylandEnv Client
  _ <- liftIO $ forkIO $ runReaderT (clientLoop Server sock) clientenv
  liftIO $ runReaderT (listenForClients sock) env



getHeader :: Get (Word32, Word16, Word16)
getHeader = (,,) <$> getWord32le <*> getWord16le <*> getWord16le


decodeFds :: BS.ByteString -> [Fd]
decodeFds bs =
  Fd <$> go bs []
  where
    go b acc
      | BS.length b < sizeOf (0 :: CInt) = reverse acc
      | otherwise =
          let (x, rest) = BS.splitAt (sizeOf (0 :: CInt)) b
              v = unsafePerformIO $
                    BS.useAsCString x (peek . castPtr)
          in go rest (v:acc)

-- | handle communication between a server and a client in provided socket, works both on the server and the client.
clientLoop :: Perspective -> Socket -> Wayland Client ()
clientLoop = clientLoop' [] ""

clientLoop' :: [Fd] -> BS.ByteString -> Perspective -> Socket -> Wayland Client ()
clientLoop' fds bytes' as sock = do
  (_, bytes'', cmsgs, flags) <- liftIO $ recvMsg sock 8 4096 mempty
  let newFds = concatMap (decodeFds . cmsgData) $ filter (\x -> cmsgId x == CmsgIdFds) cmsgs
  let bytes = bytes' <> bytes''
  bool (case extractMessage bytes of
    Just (id, opcode, x,y) -> handleMessage as id (fds ++ newFds) opcode x >> clientLoop' [{-hopefully fd passing won't break horrifyingly-}] y as sock
    Nothing -> undefined
    )
    (clientLoop' (fds ++ newFds) bytes as sock)
    (isPartial bytes)

isPartial :: BS.ByteString -> Bool
isPartial s = case runGetOrFail getHeader (BL.fromStrict s) of
              Left (_,_,_) -> False
              Right (rest,_,(_,_,size)) -> size == 8 * (1 + fromIntegral (BL.length rest))
extractMessage :: BS.ByteString -> Maybe (Word32,Word16, BS.ByteString, BS.ByteString)
extractMessage s = case runGetOrFail getHeader (BL.fromStrict s) of
              Left (_,_,_) -> Nothing
              Right (rest,_,(id,opcode,size)) -> Just (id, opcode, BS.take (fromIntegral size) $ BS.toStrict rest, BS.drop (fromIntegral size) $ BS.toStrict rest)
handleMessage :: Perspective -> ObjectID -> [Fd] -> Word16 -> BS.ByteString -> Wayland Client ()
handleMessage as id fds opcode msg = do
  ClientEnv env <- ask
  objects <- readIORef env.objects
  case Map.lookup id objects of
    Just (Interface x) -> case as of
      Client -> dispatchClientMessage x id fds opcode msg
      Server -> undefined
    Nothing -> error $ "invalid object reference with id: " <> show id
  undefined

dispatchClientMessage
  :: forall i. Interface' i Client
  => i
  -> ObjectID
  -> [Fd]
  -> Word16
  -> BS.ByteString
  -> Wayland Client ()
dispatchClientMessage x _ fds opcode msg = do
  case runGetOrFail (getEvent opcode $ AdditionalParserData fds) (toLazy msg) of
    Left (_, _, err)      -> fail err
    Right (_, _, event)   -> runEvent x event

getObjectParser = undefined 
  {-message <- parseMessage sock
  case message of
    Nothing -> liftIO $ print "failed to read message"
    Just Message {msgSender, msgFunction} -> do
      env <- ask
      obj <- liftIO $ readIORef (clientGlobalObjects env)
      case Map.lookup msgSender obj of
        Just x -> displayRequest msgFunction >> executeFunction x msgFunction
        Nothing -> liftIO $ print $ "couldn't find the object: `" <> show msgSender <> "`"
  clientLoop sock-}

-- }}}

-- Socket Finding Utilities {{{
getXdgRuntimeDir :: IO (Maybe String)
getXdgRuntimeDir = getEnv "XDG_RUNTIME_DIR"

getWaylandDisplay :: IO (Maybe String)
getWaylandDisplay = getEnv "WAYLAND_DISPLAY"

getSocketPath :: IO (Maybe FilePath) -> IO (Maybe FilePath)
getSocketPath = liftA2 (liftA2 (</>)) getXdgRuntimeDir

-- | Find an already existing socket, if the environment variable does not exist
openSocket :: IO (Maybe FilePath)
openSocket = findSocket' doesFileExist

-- | Find a non-existing valid socket file
availableSocket :: IO (Maybe FilePath)
availableSocket = findSocket' (fmap not . doesFileExist)

-- | Helper function allowing filtering of wayland sockets
findSocket' :: (FilePath -> IO Bool) -> IO (Maybe FilePath)
findSocket' f =
  getWaylandDisplay >>= \case
    Just x -> pure $ Just x
    Nothing ->
      getXdgRuntimeDir >>= \case
        Just xdg -> newNumbered (f . (xdg </>)) "wayland-" 0 99
        Nothing -> pure Nothing

-- }}}

-- Helpers {{{
newNumbered :: (FilePath -> IO Bool) -> FilePath -> Int -> Int -> IO (Maybe FilePath)
newNumbered req s i maxi = bool (req this >>= bool (newNumbered req s (i + 1) maxi) (pure $ Just this)) (pure Nothing) (maxi < i)
  where
    this = s <> fromString (show i)

-- }}}

-- vim: foldmethod=marker
