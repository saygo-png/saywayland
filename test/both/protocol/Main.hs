module Main (main) where

import Language.Haskell.TH
import Protocol
import Prelude
import Data.ByteString qualified as BS
import Text.XML.Light
import Data.List (intersperse)
import Data.Maybe (fromJust)
import Data.String (IsString(fromString))
import Saywayland.Types
mkTemplate :: FilePath -> IO ()
mkTemplate prot = do
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> BS.readFile prot
  let a = fmap fromProtocol protocols
  BS.writeFile "output.prot.hs" $ BS.concat a

fromProtocol :: Element -> BS.ByteString
fromProtocol e = BS.concat $ intersperse "\n" $ loadInt' <$> findChildren (qname "interface") e

loadInt' :: Element -> BS.ByteString
loadInt' e = "-- " <> intName <> " {{{\ninstance Interface' " <> intName <> " Client where\n  type Event " <> intName <> " = Event_" <> intName <> "\n  type Request " <> intName <> " = Request_" <> intName <> "\n" <> requests <> "\n\n" <> events <> "\n--}}}"
  where
    intName = fromString $ fromJust $ findAttr (qname "name") e
    events = BS.concat $ fmap ("\n  " <>) $ getDeff "Event" intName <$> findChildren (qname "event") e
    requests = BS.concat $ fmap ("\n  " <>) $ getDeff "Request" intName <$> findChildren (qname "request") e

getDeff :: BS.ByteString -> BS.ByteString -> Element -> BS.ByteString
getDeff p intName e = "run" <> p <> " " <> p <> "_" <> intName <> "_" <> name <> "{} = undefined" where name = fromString . fromJust $ findAttr (qname "name") e

mkDatas :: FilePath -> IO ()
mkDatas l = interfaces >>= BS.writeFile "defs.hs" . BS.concat . fmap define
  where
    interfaces = fmap (fromString . fromJust . findAttr (qname "name")) . concatMap (findChildren $ qname "interface") . filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> BS.readFile l
    define x = "data " <> x <> " = " <> x <> " {wlid :: Word32}\nmakeFieldsId ''" <> x <> "\ninstance DefaultIO " <> x <> " where\n  defM = undefined\n"
main :: IO ()
main = runQ (loadProtocols True "protocols") >>= writeFile "output.th.hs" . pprint --mkDatas "protocols/wayland.xml"--mkTemplate "protocols/wayland.xml" --wlr-layer-shell-unstable-v1.xml
