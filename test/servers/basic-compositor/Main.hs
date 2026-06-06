module Main (main) where

-- THIS IS JUST A SERVER. IT DOES NOT RENDER ANYTHING.

import Control.Concurrent.Async (async, wait)
import Control.Exception (bracket)
import Data.Map qualified as Map
import Network.Socket
import Relude
import Saywayland
import System.Directory (removeFile)

main = bracket env cleanup (runReaderT program)
  where
    cleanup :: WaylandEnv 'Server -> IO ()
    cleanup senv' = do
      let ServerEnv senv = senv'
      clients <- readIORef senv.clients
      mapM_ (\client -> close client.socket) clients
      close senv.socket
      removeFile senv.socketPath
    env :: IO (WaylandEnv Server)
    env = do
      getSocketPath availableSocket >>= \case
        Nothing -> error "couldn't find a socket path"
        Just path -> do
          sock <- socket AF_UNIX Stream 0
          bind sock (SockAddrUnix path)
          listen sock 5

          ref <- newIORef Map.empty
          iref <- newIORef $ Map.fromList waylandInterfaceServerTable
          vref <- newIORef $ Map.fromList waylandVersionTable
          pure
            $ ServerEnv
              ServerEnvironment
                { socket = sock
                , socketPath = path
                , clients = ref
                , interfaceTable = iref
                , versionTable = vref
                }

program :: Wayland Server ()
program = listenForClients
