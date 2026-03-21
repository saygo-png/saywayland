module Main (main) where

import Prelude
import Protocol

main :: IO ()
main = do
  writeFile "output.th.hs" $ loadProtocols "protocols"
