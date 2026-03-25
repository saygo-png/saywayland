{-# LANGUAGE TemplateHaskell #-}
module Saywayland.Interfaces where

import Relude hiding (get,put)
import Protocol hiding (ObjectID)
--import Saywayland.Protocol
import Saywayland.Types
import Language.Haskell.TH (Type(ConT))

import Data.ByteString.Lazy qualified as BSL
import Data.Map qualified as Map
import Data.Binary.Put (runPut, putLazyByteString)
import Data.Binary (Binary (get, put))
import Network.Socket.ByteString.Lazy (sendAll)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), hNowSupportsANSI, setSGRCode)

-- ObjectID Type {{{
type role ObjectID phantom

-- | Phantom type representing an ID of an object.
newtype ObjectID (a :: Interface) = ObjectID {id :: WlUint}
  deriving newtype (Num, Show)

instance Binary (ObjectID a) where
  get = ObjectID <$> get
  put (ObjectID x) = put x
-- }}}

-- Interfaces {{{
data WL_display = WL_display {}
data WL_registry = WL_registry {}
data WL_callback = WL_callback {id :: Word32}

data Interface = WLDisplay WL_display | WLRegistry WL_registry | WLCallback WL_callback
-- }}}

-- The Wayland Monad {{{
type Wayland p = WaylandM Interface p
-- }}}

-- TemplateHaskell Definitions {{{
$(loadProtocols (ConT ''Wayland) False "protocols")
--}}}

-- Utils {{{
newObjectId :: Wayland 'Client Word32
newObjectId = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.counter (+ 1)
    liftIO $ readIORef env.counter

newObject :: Word32 -> Interface -> Wayland 'Client Interface
newObject intId int = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.insert intId int)
    pure int

{- | Convenience function for sending a Wayland message.
See 'mkMessage'.
-}
sendMessage :: Word32 -> Word16 -> BSL.ByteString -> Wayland 'Client ()
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

-- }}}

-- WL_display {{{

instance Interface_wl_display WL_display Client where
  wl_display_sync callbackId self = do
    ClientEnv _env <- ask
    (WLCallback _callback) <- newObject callbackId (WLCallback WL_callback {id = callbackId})
    pure ()
  wl_display_get_registry registryID self = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.insert registryID (WLRegistry WL_registry {}))

    let body = runPut $ wl_display_get_registryBuilder (AdditionalParserData []) registryID
    let opcode = wl_display_get_registryOpcode
    sendMessage wlDisplayID opcode body
    liftIO . strReq ("wl_display", fromIntegral wlDisplayID, "get_registry") $ "wl_registry=" <> show registryID
  wl_display_error objectId code message _self = do
    liftIO $ print $ "Unhandled error from `" <> show objectId <> "`: [" <> show code <> "] " <> message
  wl_display_delete_id objectId _self = do
    ClientEnv env <- ask
    liftIO $ modifyIORef env.objects (Map.delete objectId)


instance Interface_wl_display WL_display Server where
  wl_display_sync callbackId self = {-HANDLE sync request-} undefined
  wl_display_get_registry registryId self = {-HANDLE get_registry request-} undefined
  wl_display_error objectId code message self = {-SEND error event-} undefined
  wl_display_delete_id id = {-SEND delete_id event-} undefined
-- }}}

-- WL_registry {{{
instance Interface_wl_registry WL_registry Client where
 
instance Interface_wl_registry WL_registry Server where
-- }}}

-- WL_callback {{{
instance Interface_wl_callback WL_callback Client where
  wl_callback_done dat self = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.delete self.id)
-- }}}
-- vim: foldmethod=marker
