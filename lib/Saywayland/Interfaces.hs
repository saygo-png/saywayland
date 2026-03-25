{-# LANGUAGE TemplateHaskell #-}
module Saywayland.Interfaces where

import Relude
import Protocol
--import Saywayland.Protocol
import Saywayland.Types
import Language.Haskell.TH (Type(ConT))

import Data.Map qualified as Map
import Data.Binary.Put (runPut)

--data ClientWL_display = ClientWL_display {}
--data ServerWL_display = ServerWL_display {}

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
-- }}}

-- WL_display {{{
wlDisplayID = 1

instance Interface_wl_display WL_display Client where
  wl_display_sync callbackId self = do
    ClientEnv _env <- ask
    (WLCallback _callback) <- newObject callbackId (WLCallback WL_callback {id = callbackId})
    pure ()
  wl_display_get_registry registryID self = do
    ClientEnv env <- ask
    modifyIORef env.objects (Map.insert registryID (WLRegistry WL_registry {}))
    {- TODO:
    let messageBody = runPut $ put registryID
    sendMessage wlDisplayID 1 messageBody
    liftIO . strReq ("wl_display", wlDisplayID, "get_registry") $ "wl_registry=" <> show registryID
    return $ coerce registryID
    -}
  wl_display_error objectId code message self = do
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
