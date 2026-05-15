{-# LANGUAGE TemplateHaskellQuotes #-}

module Protocol (module Protocol) where

-- this module's purpose is to define all requests and events that exist and should be implemented. Implementing them is handled in `Protocols/`

import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (putByteString, putInt32le, putWord32le)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.Functor
import Data.Maybe (fromJust)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Relude qualified
import Saywayland.Types
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import System.Posix (Fd)
import Text.XML.Light
import Prelude

type VersionTable = [(String, Word32)]

-- | generates a VersionTable for the given protocol
generateVersionTable :: Element -> [Dec]
generateVersionTable e =
  [ SigD name $ ConT ''VersionTable
  , ValD (VarP name) (NormalB $ ListE defs) []
  ]
  where
    protocol = fromJust $ findAttr (qname "name") e
    name = mkName $ protocol <> "VersionTable"
    tuple x = TupE [Just $ VarE $ mkName $ x <> "Name", Just $ VarE $ mkName $ x <> "Version"]
    defs = tuple . fromJust . findAttr (qname "name") <$> findChildren (qname "interface") e

type InterfaceTable = [(String, IO (Interface Client))] -- todo: s/Client/p

-- | generates an InterfaceTable, using formatter to format classes names - as they are to be defined by the user.
generateInterfaceTable :: Element -> (String -> String) -> [Dec]
generateInterfaceTable e formatter =
  [ SigD name $ ConT ''InterfaceTable
  , ValD (VarP name) (NormalB $ ListE defs) []
  ]
  where
    protocol = fromJust $ findAttr (qname "name") e
    name = mkName $ protocol <> "InterfaceTable"
    tuple x = TupE [Just $ VarE $ mkName $ x <> "Name", Just $ AppE (AppE (VarE $ mkName "<$>") $ ConE 'Interface) $ SigE (VarE $ mkName "defM") (AppT (ConT ''IO) $ ConT $ mkName $ formatter x)]
    defs = tuple . fromJust . findAttr (qname "name") <$> findChildren (qname "interface") e

-- Getters/Putters {{{

-- | Get a Wire-encoded String
getString :: Get BS.ByteString
getString = do
  len <- getWord32le
  str <- getByteString $ fromIntegral len
  let padding = (4 - (len `mod` 4)) `mod` 4
  _ <- getByteString $ fromIntegral padding
  pure str

-- | Put a Wire-encoded string
putString :: BS.ByteString -> Put
putString s' =
  putWord32le (fromIntegral $ BS.length s)
    >> putByteString s
    >> putByteString (BS.replicate padding 0)
  where
    s = s' <> BS.pack [0]
    padding = (4 - BS.length s `mod` 4) `mod` 4

-- | Get a Wire-encoded Fixed
getFixed24_8 :: Get Double
getFixed24_8 = getInt32le <&> (/ 256.0) . fromIntegral

-- | Put a Wire-encoded Fixed
putFixed24_8 :: Double -> Put
putFixed24_8 d = putInt32le $ fromIntegral @Integer $ round $ d * 256

-- | Get an Fd from previously obtained ancillary data
getFd :: AdditionalParserData -> Get Fd
getFd dat = case Relude.nonEmpty dat.fds of
  Nothing -> error "No file descriptor found in ancillary data!"
  Just fd -> pure $ Relude.head fd

-- | return a TH getter expression for a given Type
getForType :: Type -> Q Exp
getForType t = case t of
  ConT name
    | name == ''Int -> [|(const $ fromIntegral <$> getWord32le)|]
    | name == ''Word32 -> [|(const getWord32le)|]
    | name == ''BS.ByteString -> [|(const getString)|]
    | name == ''Double -> [|(const getFixed24_8)|]
    | name == ''ObjectID -> [|(const getWord32le)|]
    | name == ''NewID ->
        [|
          ( const
              ( do
                  strname <- getString
                  name' <- getWord32le
                  id' <- getWord32le
                  pure (strname, name', id')
              )
          )
          |]
    | name == ''Fd -> [|getFd|]
  _ -> error $ "unsupported type" <> show t

-- | return a TH putter expression for a given Type
putForType :: Type -> Q Exp
putForType t = case t of
  ConT name
    | name == ''Int -> [|(const $ putWord32le . fromIntegral)|]
    | name == ''Word32 -> [|(const putWord32le)|]
    | name == ''BS.ByteString -> [|(const putString)|]
    | name == ''Double -> [|(const putFixed24_8)|]
    | name == ''ObjectID -> [|(const putWord32le)|]
    | name == ''NewID -> [|(const (\(x, y, z) -> putString x >> putWord32le y >> putWord32le z))|]
    | name == ''Fd -> [|const (const (pure ()))|]
  _ -> error $ "unsupported type" <> show t

-- }}}

-- Utils {{{
qname :: String -> QName
qname x = QName x Nothing Nothing

-- }}}

-- TemplateHaskell Utils {{{
adata :: Name
adata = mkName "additionalData"

-- | Defines an integer variable with name `name` and value `x`
mkIntVariable :: String -> Integer -> [Dec]
mkIntVariable name x =
  [ SigD (mkName name) (ConT ''Int)
  , ValD (VarP (mkName name)) (NormalB (LitE $ IntegerL x)) []
  ]

-- | Returns a declaration of the `Function`s opcode as an integer variable.
mkOpcode :: String -> String -> Word16 -> [Dec]
mkOpcode interfaceName fname opcode =
  [ SigD (mkName $ interfaceName <> "_" <> fname <> "Opcode") (ConT ''Word16)
  , FunD (mkName $ interfaceName <> "_" <> fname <> "Opcode") [Clause [] (NormalB $ LitE $ IntegerL $ fromIntegral opcode) []]
  ]

{- | Defines an enum-like along with a function to look up the value of each element
example output:
data EnumName = A | B | C | D ... deriving Eq
enumName' A = 1 ...
-}
mkEnum :: String -> String -> [(String, Int)] -> [Dec]
mkEnum interfaceName enumName enumKV =
  [ DataD [] (mkName enumName') [] Nothing constructors [DerivClause (Just StockStrategy) [ConT ''Eq]]
  , SigD (mkName funName) (AppT (AppT ArrowT (ConT $ mkName enumName')) (ConT ''Word32))
  , FunD (mkName funName) clauses
  , SigD (mkName funName') (AppT (AppT ArrowT (ConT ''Word32)) (ConT $ mkName enumName'))
  , FunD (mkName funName') clauses'
  ]
  where
    enumName' = "Enum_" <> interfaceName <> "_" <> enumName
    funName = "enum_" <> interfaceName <> "_" <> enumName
    funName' = "enum_" <> interfaceName <> "_" <> enumName <> "'"
    constructors = (`NormalC` []) . mkName . (enumName' <>) <$> fmap fst enumKV
    clauses = [Clause [ConP (mkName $ enumName' <> k) [] []] (NormalB (LitE (IntegerL (fromIntegral v)))) [] | (k, v) <- enumKV]

    clauses' = [Clause [LitP (IntegerL (fromIntegral v))] (NormalB (ConE (mkName $ enumName' <> k))) [] | (k, v) <- enumKV]

-- }}}

{- | Loads all .xml files in `path` as protocols.
Set `isIO` to True only when running the function within an IO monad. This should be used *only* for debugging purposes.
`monad` defines the monad in which all events and requests operate in.
-}
loadProtocols :: Bool -> FilePath -> Q [Dec]
loadProtocols isIO path = do
  protocol_files <- filter ((== ".xml") . takeExtension) <$> runIO (listDirectory path)
  concat <$> mapM (loadProtocolFile isIO . (path </>)) protocol_files

-- | Load a protocol from the specified `path`. Arguments have the same meaning as in `loadProtocols`.
loadProtocolFile :: Bool -> FilePath -> Q [Dec]
loadProtocolFile isIO path = do
  unless isIO $ addDependentFile path
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (BS.readFile path)
  let findInterfaces = findChildren (qname "interface")
  concat
    <$> mapM
      ((<&> concat) . mapM loadInterface . findInterfaces)
      protocols

generateTables :: Bool -> (String -> String) -> FilePath -> Q [Dec]
generateTables isIO formatter path = do
  unless isIO $ addDependentFile path
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (BS.readFile path)
  pure $
    concatMap (`generateInterfaceTable` formatter) protocols
      <> concatMap generateVersionTable protocols

mkEvents :: String -> String -> [Element] -> [Dec]
mkEvents interfaceName prefix events = [DataD [] (mkName prefix') [] Nothing constructors []]
  where
    prefix' = prefix <> "_" <> interfaceName
    buildBang x = (mkName . fromJust $ findAttr (qname "name") x, Bang NoSourceUnpackedness NoSourceStrictness, argType x)
    buildRecord x = RecC (mkName $ prefix' <> "_" <> fromJust (findAttr (qname "name") x)) $ buildBang <$> findChildren (qname "arg") x
    constructors = fmap buildRecord events

mkShow :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkShow interfaceName prefix prefix2 events =
  mapM (pure . mkShowC) (fmap snd events) <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''ObjectID) $ AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''String)
      , FunD (mkName prefix) m
      ]
      []
      (null m)
  where
    arrow = case prefix2 of
      "Request_" -> "        -> "
      "Event_" -> "        <- "
      _ -> "        ?? "
    mkShowC :: Element -> Clause
    mkShowC e = Clause [VarP $ mkName "oid", RecP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) $ fmap (\x -> (x, VarP x)) args] (NormalB $ chainShow (reverse args)) []
      where
        single x = AppE (AppE (VarE '(<>)) $ LitE $ StringL $ " " <> nameBase x <> ": ") $ AppE (VarE 'show) $ VarE x
        chainShow [] = LitE $ StringL ""
        chainShow [x] =
          AppE
            ( AppE (VarE '(<>)) $
                AppE (AppE (VarE '(<>)) $ LitE $ StringL $ mconcat [arrow, interfaceName, "@"]) $
                  AppE (AppE (VarE '(<>)) (AppE (VarE 'show) $ VarE (mkName "oid"))) (LitE $ StringL $ mconcat [".", eventName, ": "])
            )
            $ single x
        chainShow (x : xs) = InfixE (Just $ chainShow xs) (VarE '(<>)) (Just $ single x)
        args = mkName . fromJust . findAttr (qname "name") <$> findChildren (qname "arg") e
        eventName = fromJust $ findAttr (qname "name") e

mkOpcodeGetter :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkOpcodeGetter interfaceName prefix prefix2 events =
  mapM mkClause events <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''Word16)
      , FunD (mkName prefix) m
      ]
      []
      (null m)
  where
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (opcode, element) = pure $ Clause [RecP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) []] (NormalB $ LitE $ IntegerL $ fromIntegral opcode) []
      where
        eventName = fromJust $ findAttr (qname "name") element

mkPut :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkPut interfaceName prefix prefix2 events =
  mapM mkClause events <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''AdditionalParserData) $ AppT (AppT ArrowT $ ConT $ mkName $ prefix2 <> interfaceName) $ ConT ''Put)
      , FunD (mkName prefix) m
      ]
      []
      (null m)
  where
    nestPutters [] = AppE (VarE 'pure) $ ConE '()
    nestPutters [x] = x
    nestPutters (x : xs) = InfixE (Just $ nestPutters xs) (VarE '(>>)) (Just x)
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (_opcode, element) =
      mapM (\(a, b) -> putForType b <&> (`AppE` (GetFieldE (VarE $ mkName "event") $ fromJust $ findAttr (qname "name") a)) . (`AppE` VarE adata)) (zip args argTypes)
        <&> \x ->
          ( Clause
              [ VarP adata
              , AsP (mkName "event") (RecP (mkName $ prefix2 <> interfaceName <> "_" <> eventName) [])
              ]
              $ NormalB
              $ nestPutters (reverse x)
          )
            []
      where
        args = findChildren (qname "arg") element
        argTypes = fmap argType args
        eventName = fromJust $ findAttr (qname "name") element

mkParser :: String -> String -> String -> [(Word16, Element)] -> Q [Dec]
mkParser interfaceName prefix prefix2 events =
  mapM mkClause events <&> \m ->
    bool
      [ SigD (mkName prefix) (AppT (AppT ArrowT $ ConT ''Word16) $ AppT (AppT ArrowT $ ConT ''AdditionalParserData) (AppT (ConT ''Get) $ ConT $ mkName $ prefix2 <> interfaceName))
      , FunD (mkName prefix) m
      ]
      []
      (null m)
  where
    nestGetters [] = undefined
    nestGetters [x] = AppE (VarE 'pure) x
    nestGetters [x, y] = InfixE (Just y) (VarE '(<$>)) (Just $ AppE x $ VarE adata)
    nestGetters (x : xs) = InfixE (Just $ nestGetters xs) (VarE '(<*>)) (Just $ AppE x $ VarE adata)
    mkClause :: (Word16, Element) -> Q Clause
    mkClause (opcode, element) =
      mapM getForType argTypes
        <&> \x -> (Clause [LitP $ IntegerL $ fromIntegral opcode, VarP adata] $ NormalB $ nestGetters (reverse $ ConE (mkName $ prefix2 <> interfaceName <> "_" <> eventName) : x)) []
      where
        args = findChildren (qname "arg") element
        argTypes = fmap argType args
        eventName = fromJust $ findAttr (qname "name") element

mkWLEvent :: String -> String -> [(Word16, Element)] -> Q [Dec]
mkWLEvent interfaceName prefix2 events = do
  put' <- mkPut interfaceName "putEvent" prefix2 events
  get' <- mkParser interfaceName "getEvent" prefix2 events
  opc' <- mkOpcodeGetter interfaceName "getOpcode" prefix2 events
  show' <- mkShow interfaceName "showEvent" prefix2 events
  pure [InstanceD Nothing [] (AppT (ConT ''WaylandEvent) $ ConT . mkName $ prefix2 <> interfaceName) $ put' <> get' <> opc' <> show']

-- | Create all definitions for a single interface - version, the class, parsers, builders, enums, opcodes,
loadInterface :: Element -> Q [Dec]
loadInterface int = do
  let events = findChildren (qname "event") int
  let requests = findChildren (qname "request") int
  let opcodes = concatMap (\(x, y) -> mkOpcode name' (fromJust $ findAttr (qname "name") y) x) $ zip [1 ..] $ findChildren (qname "event") int

  concat
    <$> sequence
      [ -- WaylandEvent
        pure $ mkEvents name' "Request" requests
      , pure $ mkEvents name' "Event" events
      , mkWLEvent name' "Event_" $ zip [0 ..] events
      , mkWLEvent name' "Request_" $ zip [0 ..] requests
      , pure
          [ -- Version
            SigD (mkName $ name' <> "Version") $ ConT ''Word32
          , ValD (VarP verName) (NormalB . LitE . IntegerL $ version') []
          , -- Name
            SigD (mkName $ name' <> "Name") $ ConT ''String
          , ValD (VarP nameName) (NormalB . LitE . StringL $ name') []
          -- Class definition
          ]
      , -- Enums
        pure $ concatMap (uncurry $ mkEnum name') enums'
      , -- Opcodes
        pure opcodes
      ]
  where
    name' = fromJust $ findAttr (qname "name") int
    verName = mkName $ name' <> "Version"
    nameName = mkName $ name' <> "Name"
    version' = read . fromJust $ findAttr (qname "version") int
    enums' = loadEnum <$> findChildren (qname "enum") int

-- | Load enum data from XML spec.
loadEnum :: Element -> (String, [(String, Int)])
loadEnum e' = (fromJust $ findAttr (qname "name") e', f <$> findChildren (qname "entry") e')
  where
    f e = (fromJust $ findAttr (qname "name") e, read $ fromJust $ findAttr (qname "value") e)

argType :: Element -> Type
argType x = case findAttr (qname "type") x of
  Nothing -> error $ "arg without a type discovered" <> show x
  Just "new_id" -> case findAttr (qname "interface") x of
    Just _ -> ConT ''ObjectID
    Nothing -> ConT ''NewID
  Just "int" -> ConT ''Int
  Just "uint" -> ConT ''Word32
  Just "fixed" -> ConT ''Double
  Just "string" -> ConT ''BS.ByteString
  Just "object" -> ConT ''ObjectID
  Just "array" -> ConT ''BS.ByteString
  Just "fd" -> ConT ''Fd
  Just y -> error $ "unknown type: " <> y

-- }}}

-- vim: foldmethod=marker
