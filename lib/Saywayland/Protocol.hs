{-# LANGUAGE TemplateHaskell #-}
module Saywayland.Protocol where

import Protocol
import Data.Binary
import Data.Binary.Get
import Control.Applicative

$(loadProtocols "protocols")
