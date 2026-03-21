module Saywayland.Interfaces where

import Relude
import Saywayland.Protocol

-- WL_display {{{
data ClientWL_display = ClientWL_display {}
data ServerWL_display = ServerWL_display {}

instance Interface_wl_display ClientWL_display where
  wl_display_sync callbackId self = {-SEND sync request-} undefined
  wl_display_get_registry registryId self = {-SEND get_registry request-} undefined
  wl_display_error objectId code message self = {-HANDLE error event-} undefined
  wl_display_delete_id id = {-HANDLE delete_id event-} undefined
 
instance Interface_wl_display ServerWL_display where
  wl_display_sync callbackId self = {-HANDLE sync request-} undefined
  wl_display_get_registry registryId self = {-HANDLE get_registry request-} undefined
  wl_display_error objectId code message self = {-SEND error event-} undefined
  wl_display_delete_id id = {-SEND delete_id event-} undefined

-- }}}

-- vim: foldmethod=marker
