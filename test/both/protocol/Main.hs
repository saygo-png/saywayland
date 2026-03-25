module Main (main) where

import Language.Haskell.TH
import Prelude
import Protocol

main :: IO ()
main = do
  runQ (loadProtocols True "protocols") >>= writeFile "output.th.hs" . pprint
