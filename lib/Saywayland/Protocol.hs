{-# LANGUAGE TemplateHaskell #-}
module Saywayland.Protocol where

import Relude
import Protocol
import Data.Binary
import Data.Binary.Get
import Control.Applicative

$(loadProtocols False "protocols")
