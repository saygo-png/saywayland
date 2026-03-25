{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Language.Haskell.TH
import Prelude
import Protocol

import Saywayland.Interfaces

main :: IO ()
main = do
  runQ (loadProtocols (ConT ''Wayland) True "protocols") >>= writeFile "output.th.hs" . pprint
