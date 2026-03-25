{-# LANGUAGE TemplateHaskell, DataKinds, OverloadedStrings #-}
module Protocol where
-- this module's purpose is to define all requests and events that exist and should be implemented. Implementing them is handled in `Interfaces.hs`

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Prelude
import Control.Monad (guard, unless, zipWithM)
import Data.Functor
import Data.Word
import Data.Maybe (fromJust)
import Data.Char (toUpper, toLower)
import Data.List (singleton)
import Data.Binary
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Binary.Put (putInt32be, putByteString)

import System.Posix (Fd)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import Text.XML.Light

type ObjectID = Word32

-- Getters/Putters {{{
getString :: Get BS.ByteString
getString = do 
  len <- getWord32le
  str <- getBytes $ fromIntegral len
      --str = BS.take len (BS.drop 4 bytes)
  let padding = (4 - (len `mod` 4)) `mod` 4
  _ <- getBytes $ fromIntegral padding
  pure str

getFixed24_8 :: Get Double
getFixed24_8 = getInt32be <&> (/ 256.0) . fromIntegral

putFixed24_8 :: Double -> Put
putFixed24_8 d = putInt32be $ fromIntegral $ round $ d * 256

getFd :: AdditionalParserData -> Get Fd
getFd dat = pure $ head dat.fds

getForType :: Type -> Q Exp
getForType t = case t of
  ConT name
    | name == ''Int       -> [| (const $ fromIntegral <$> getWord32le) |]
    | name == ''Word32    -> [| (const getWord32le) |]
    | name == ''BS.ByteString    -> [| (const getString) |]
    | name == ''Double    -> [| (const getFixed24_8) |]
    | name == ''ObjectID  -> [| (const getWord32le) |]
    | name == ''Fd        -> [| getFd |]
  _ -> error $ "unsupported type" <> show t

putForType :: Type -> Q Exp
putForType t = case t of
  ConT name
    | name == ''Int       -> [| (const $ put . fromIntegral) |]
    | name == ''Word32    -> [| (const put) |]
    | name == ''BS.ByteString    -> [| (const putByteString) |]
    | name == ''Double    -> [| (const putFixed24_8) |]
    | name == ''ObjectID  -> [| (const put) |]
    | name == ''Fd        -> [| undefined |]
  _ -> error $ "unsupported type" <> show t
-- }}}

-- Utils {{{
classCase, functCase :: String -> String
classCase (x:xs) = toUpper x : xs
classCase [] = []
functCase (x:xs) = toLower x : xs
functCase [] = []
qname :: String -> QName
qname x = QName x Nothing Nothing

collectArgs :: Type -> ([Type], Type)
collectArgs (AppT (AppT ArrowT a) b) =
  let (args, ret) = collectArgs b
  in (a : args, ret)
collectArgs t = ([], t)

genBindsGet :: [Type] -> Name -> Q ([Name], [Stmt])
genBindsGet tys additionaldata = do
  let names = fmap (mkName . ("arg" ++) . show) [1..length tys]
  stmts <- sequence
    [ do
        e <- getForType ty
        pure (BindS (VarP n) $ AppE e $ VarE additionaldata)
    | (n, ty) <- zip names tys
    ]
  pure (names, stmts)

genBindsPut :: [Type] -> Name -> Q ([Name], [Stmt])
genBindsPut tys additionaldata = do
  let names = fmap (mkName . ("arg" ++) . show) [1..length tys]
  stmts <- sequence
    [ do
        e <- putForType ty
        pure (NoBindS $ AppE (AppE e $ VarE additionaldata) (VarE n))
    | (n, ty) <- zip names tys
    ]
  pure (names, stmts)
-- }}}

-- TemplateHaskell Utils {{{
adata :: Name
adata = mkName "additionalData"

-- | Creates a declaration of class header comparable to `class t arg1 arg2 where`.
mkClass :: String -> [String] -> [Dec] -> Dec
mkClass t args = ClassD [] (mkName $ classCase t) (fmap ((`PlainTV` BndrReq) . mkName) args) []

-- | Defines an integer variable with name `name` and value `x`
mkIntVariable :: String -> Integer -> [Dec]
mkIntVariable name x = [
    SigD (mkName name) (ConT ''Int)
  , ValD (VarP (mkName name)) (NormalB (LitE $ IntegerL x)) []
  ]

-- | Given a list of types chains them together using the `->` operator.
constructType :: [Type] -> Type
constructType [] = undefined
constructType [x] = x
constructType (x:xs) = AppT (AppT ArrowT x) $ constructType xs

-- | Returns a function signature declaration for the provided `Function`.
mkFunction :: Type -> String -> Function a -> Dec
mkFunction monad interfaceName f = SigD (mkName $ functCase interfaceName <> "_" <> f.name) $ constructType $ f.fType
                                    <> [VarT (mkName "a"), AppT (AppT monad $ VarT $ mkName "p") $ TupleT 0]

-- | Returns a declaration of the `Function`s opcode as an integer variable.
mkOpcode :: String -> Function a -> [Dec]
mkOpcode interfaceName f = 
  [ SigD (mkName $ interfaceName <> "_" <> f.name <> "Opcode") (ConT ''Word16)
  , FunD (mkName $ interfaceName <> "_" <> f.name <> "Opcode") [Clause [] (NormalB $ LitE $ IntegerL $ fromIntegral f.opcode) []]
  ]

-- | Defines a builder for the provided request/event. The Builder is of type `AdditionalParserData -> Arg1 -> ... -> Put`.
mkBuilder :: String -> Function a -> Q [Dec]
mkBuilder interfaceName f = do 
  (argNames, argStmts) <- genBindsPut f.fType adata
  ret <- NoBindS <$> [e|pure ()|]
  let body = DoE Nothing $ argStmts ++ [ret]
  pure [
      SigD (mkName $ functCase interfaceName <> "_" <> f.name <> "Builder") $ constructType $ ConT ''AdditionalParserData:f.fType ++ [ConT ''Put]
    , FunD (mkName $ interfaceName <> "_" <> f.name <> "Builder") [Clause (VarP adata:fmap VarP argNames) (NormalB body) []]
    ]
{- | Defines an enum-like along with a function to look up the value of each element
example output:
data EnumName = A | B | C | D ... deriving Eq
enumName' A = 1 ...
-}
mkEnum :: String -> String -> [(String,Int)] -> [Dec]
mkEnum interfaceName enumName enumKV = [
    DataD [] (mkName enumName') [] Nothing constructors [DerivClause Nothing [ConT ''Eq]]
  , SigD (mkName funName) (AppT (AppT ArrowT (ConT $ mkName enumName')) (ConT ''Int))
  , FunD (mkName funName) clauses
  ]
  where
    enumName' = "Enum_" <> interfaceName <> "_" <> enumName
    funName = "enum_" <> interfaceName <> "_" <> enumName
    constructors = (`NormalC` []) . mkName . (enumName'<>) <$> fmap fst enumKV
    clauses = [Clause [ConP (mkName $ enumName' <> k) [] []] (NormalB (LitE (IntegerL (fromIntegral v)))) [] | (k,v)<-enumKV]

-- | Define an accumulated parser for the entire interface, combining all request xor event parsers.
mkParserChain :: String -> [Function a] -> Q Exp
mkParserChain interfaceName [x] = appE (varE $ mkName $ interfaceName <> "_" <> x.name <> "Parser") $ varE $ mkName "dat"
mkParserChain interfaceName (x:xs) = infixApp (mkParserChain interfaceName xs) op (appE (varE $ mkName $ interfaceName <> "_" <> x.name <> "Parser") $ varE $ mkName "dat") 
  where op = varE (mkName "<|>")
mkParserChain _ [] = [e|undefined|]--[e| Parser <$> const Nothing |]

-- }}}

-- Data Types {{{
-- | Differentiates between requests and events
data FunctionType = Event | Request
-- | A Function data type, used to pass data read from XML spec.
data Function _type = Function
  {
    opcode    :: Word16
  , name      :: String
  , fType     :: [Type]
  }

-- TODO: this can be a newtype, check if there's any more fields required.
data EnumEntry = EnumEntry {
    name    :: T.Text
  }
data AdditionalParserData = AdditionalParserData {
    fds :: [Fd]
  }
-- }}}

-- Loading from File {{{
{- | Loads all .xml files in `path` as protocols.
Set `isIO` to True only when running the function within an IO monad. This should be used *only* for debugging purposes.
`monad` defines the monad in which all events and requests operate in.
-}
loadProtocols :: Type -> Bool -> FilePath -> Q [Dec]
loadProtocols monad isIO path = do
  protocol_files <- filter ((== ".xml") . takeExtension) <$> runIO (listDirectory path)
  concat <$> mapM (loadProtocolFile monad isIO . (path </>)) protocol_files

-- | Load a protocol from the specified `path`. Arguments have the same meaning as in `loadProtocols`.
loadProtocolFile :: Type -> Bool -> FilePath -> Q [Dec]
loadProtocolFile monad isIO path = do
  unless isIO $ addDependentFile path
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (BS.readFile path)
  runIO $ print protocols
  concat <$> mapM 
    ((<&> concat) . mapM (loadInterface monad) . findChildren (qname "interface"))
    protocols

-- | Define a parser for the opcode (last 2 bytes of the header) and the body of a message for the given request/event.
mkParser :: Type -> String -> Function a -> Q [Dec]
mkParser monad interfaceName f = do
  (argNames, argStmts) <- genBindsGet f.fType adata
  opCheck <- [| getWord16le |] <&> BindS (VarP (mkName "op"))
  guardStmt <- [| guard (op == $(lit)) |] <&> NoBindS
  let func = VarE $ mkName $ functCase interfaceName <> "_" <> f.name
      args = map VarE argNames
      ret  = NoBindS $ AppE (VarE (mkName "pure")) (foldl AppE func args)
  --ret <- [| $(f.name) $(argNameList)|] <&> NoBindS
  let body = DoE Nothing (opCheck : guardStmt : argStmts ++ [ret])
  pure
    --     name:: InterfaceNameD -> AdditionalParserData -> Get (Interface (a) -> Wayland p ())
    [ SigD name (ForallT 
          [PlainTV (mkName "a") SpecifiedSpec, PlainTV (mkName "p") SpecifiedSpec]
          [AppT (AppT (ConT $ mkName $ "Interface_" <> interfaceName) (VarT (mkName "a"))) (VarT (mkName "p"))]
          $ AppT (AppT ArrowT $ ConT ''AdditionalParserData) (AppT (ConT ''Get) $ AppT (AppT ArrowT $ VarT $ mkName "a") (AppT (AppT monad $ VarT $ mkName "p") (TupleT 0))))
    , FunD name [Clause [VarP adata] (NormalB body) []]
    ]
  where
    name = mkName $ interfaceName <> "_" <> f.name <> "Parser"
    lit = litE $ integerL $ fromIntegral f.opcode

-- | Create all definitions for a single interface - version, the class, parsers, builders, enums, opcodes,
loadInterface :: Type -> Element -> Q [Dec]
loadInterface monad int = do 

  events' <- loadF "event"
  requests' <- loadF "request"
  let definitions = fmap (mkFunction monad name') requests' ++ fmap (mkFunction monad name') events'
  let opcodes = concatMap (mkOpcode name') requests' ++ concatMap (mkOpcode name') events'
  let builders = ((++) <$> mapM (mkBuilder name') requests' <*> mapM (mkBuilder name') events') <&> concat

  parserChain <- mkParserChain name' events'
  let parserChainD = FunD eventParserName [ Clause [VarP (mkName "dat")] (NormalB parserChain) [] ]
  reqParserChain <- mkParserChain name' requests'
  let reqParserChainD = FunD requestParserName [ Clause [VarP (mkName "dat")] (NormalB reqParserChain) [] ]

  concat <$> sequence [
    -- Version
      [d| $(varP (mkName $ name' <> "Version")) = $(pure . LitE . IntegerL $ version') |]
    -- Class definition
    , pure [mkClass ("Interface_" <> name') ["a", "p"] definitions]
    -- Request Parsers
    , concat <$> mapM (mkParser monad name') requests'
    , singleton <$> ([t| $(conT . mkName $ "Interface_" <> name') $a $(pure . VarT $ mkName "p") => AdditionalParserData -> Get ($a -> $(pure monad) $(pure . VarT $ mkName "p") ()) |] <&> SigD requestParserName)
    , pure [reqParserChainD]
    -- Event Parsers
    , concat <$> mapM (mkParser monad name') events'
    , singleton <$> ([t| $(conT . mkName $ "Interface_" <> name') $a $(pure . VarT $ mkName "p") => AdditionalParserData -> Get ($a -> $(pure monad) $(pure . VarT $ mkName "p") ()) |] <&> SigD eventParserName)
    , pure [parserChainD]
    -- Builders
    , builders
    -- Enums
    , pure $ concatMap (uncurry $ mkEnum name') enums'
    -- Opcodes
    , pure opcodes
    ]
  where
    a = varT $ mkName "a"
    eventParserName = mkName $ "eventParser_" <> name'
    requestParserName = mkName $ "requestParser_" <> name'
    name' = fromJust $ findAttr (qname "name") int
    loadF x = zipWithM loadFunction [0..] $ findChildren (qname x) int
    version' = read . fromJust $ findAttr (qname "version") int
    enums' = loadEnum <$> findChildren (qname "enum") int

-- | Load enum data from XML spec.
loadEnum :: Element -> (String, [(String, Int)])
loadEnum e' = (fromJust $ findAttr (qname "name") e', f <$> findChildren (qname "entry") e')
  where
    f e = (fromJust $ findAttr (qname "name") e, read $ fromJust $ findAttr (qname "value") e)

-- | Load function data from XML spec.
loadFunction :: Word16 -> Element -> Q (Function Event)
loadFunction opcode element = stack (findChildren (qname "arg") element) <&> \x -> Function 
  {
    opcode = opcode
  , name = fromJust $ findAttr (qname "name") element
  , fType = x
  }
  where
    stack :: [Element] -> Q [Type]
    stack [x] = pure $ argType x
    stack []  = pure []
    stack (x:xs) = (argType x ++) <$> stack xs
    --stack (x:xs) = AppT (AppT ArrowT $ argType x) <$> stack xs
    --stack [] = sequence [[t|Wayland.Types.Wayland ()|]]
    argType :: Element -> [Type]
    argType x = case findAttr (qname "type") x of
      Nothing -> error $ "arg without a type discovered" <> show element
      Just "new_id" -> case findAttr (qname "interface") x of
        Just _ -> [ConT ''ObjectID]
        Nothing -> [ConT ''BS.ByteString, ConT ''Word32, ConT ''ObjectID]
      Just "int" -> [ConT ''Int]
      Just "uint" -> [ConT ''Word32]
      Just "fixed" -> [ConT ''Double]
      Just "string" -> [ConT ''BS.ByteString]
      Just "object" -> [ConT ''ObjectID]
      Just "array" -> [ConT ''BS.ByteString]
      Just "fd" -> [ConT ''Fd]
      Just y -> error $ "unknown type: " <> y

-- }}}

-- vim: foldmethod=marker
