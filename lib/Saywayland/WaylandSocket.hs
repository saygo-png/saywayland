module Saywayland.WaylandSocket (module Saywayland.WaylandSocket) where

import Control.Concurrent (forkIO)
import Data.Bool (bool)
import Data.Map qualified as M
import Data.String (IsString (fromString))
import Network.Socket
import Relude (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT), newIORef)
import Saywayland.Types
import System.Directory (doesFileExist)
import System.Environment.Blank (getEnv)
import System.FilePath
import Prelude

import Saywayland.Interfaces
-- Listeners {{{

-- | listen for client connections in provided socket.
listenForClients :: Socket -> Wayland 'Server ()
listenForClients sock = do
  env <- ask
  -- NO IMPL: should create WL_display on start.
  globalsref <- liftIO $ newIORef M.empty -- Map.singleton 1 $ mkInterface WL_display
  serial <- liftIO $ newIORef 0
  objectsref <- liftIO $ newIORef M.empty
  handlers <- newIORef []
  let clientenv = ClientEnv $ ClientEnvironment{socket = sock, {-globals = globalsref,-} counter = serial, objects = objectsref{-, eventHandlers = handlers-}}
  _ <- liftIO $ forkIO $ runReaderT (clientLoop sock) clientenv
  liftIO $ runReaderT (listenForClients sock) env

-- | handle communication between a server and a client in provided socket, works both on the server and the client.
clientLoop :: Socket -> Wayland p ()
clientLoop sock = undefined

-- do
--   message <- parseMessage sock
--   case message of
--     Nothing -> liftIO $ print "failed to read message"
--     Just Message {msgSender, msgFunction} -> do
--       env <- ask
--       obj <- liftIO $ readIORef (clientGlobalObjects env)
--       case Map.lookup msgSender obj of
--         Just x -> displayRequest msgFunction >> executeFunction x msgFunction
--         Nothing -> liftIO $ print $ "couldn't find the object: `" <> show msgSender <> "`"
--   clientLoop sock

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
