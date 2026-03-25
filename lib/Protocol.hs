{-# LANGUAGE TemplateHaskell, DataKinds, OverloadedStrings #-}
module Protocol where
-- this file's purpose is to define all requests and events that exist and should be implemented. Implementing them is handled in Wayland/Core.hs

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

--import Saywayland.Types (Wayland)

import Prelude
import Control.Applicative
import Control.Monad (guard, unless, zipWithM)
import Data.Functor
import Data.Word (Word16,Word32)
import Data.Maybe (fromJust,Maybe,Maybe(Just,Nothing))
import Data.Char (toUpper, toLower)
import Data.List (singleton)
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.Text as T

import System.Posix (Fd)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))

import Text.XML.Light

type ObjectID = Word32

classCase, functCase :: String -> String
classCase (x:xs) = toUpper x : xs
classCase [] = []
functCase (x:xs) = toLower x : xs
functCase [] = []

mkClass :: String -> [String] -> [Dec] -> Dec
mkClass t args = ClassD [] (mkName $ classCase t) (fmap ((`PlainTV` BndrReq) . mkName) args) []

mkIntVariable :: String -> Integer -> [Dec]
mkIntVariable name x = [
    SigD (mkName name) (ConT ''Int)
  , ValD (VarP (mkName name)) (NormalB (LitE $ IntegerL x)) []
  ]

constructType :: [Type] -> Type
constructType [] = undefined
constructType [x] = x
constructType (x:xs) = AppT (AppT ArrowT x) $ constructType xs
-- example output:
-- functionName -> Arg1 -> Arg2 -> a -> W ()
mkFunction :: Type -> String -> Function a -> Dec
mkFunction monad interfaceName f = SigD (mkName $ functCase interfaceName <> "_" <> f.name) $ constructType $ f.fType <> [{-a is the instance the data the class interface is implemented to-} VarT (mkName "a"), AppT monad $ TupleT 0]
-- example output:
-- data EnumName = A | B | C | D ... deriving Eq
-- enumName' A = 1 ...
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

data FunctionType = Event | Request
data Function _type = Function
  {
    opcode    :: Word16
  , name      :: String
  , fType     :: [Type]
  }

data EnumEntry = EnumEntry {
    name    :: T.Text
  }


loadProtocols :: Type -> Bool -> FilePath -> Q [Dec]
loadProtocols monad isIO path = do
  protocol_files <- filter ((== ".xml") . takeExtension) <$> runIO (listDirectory path)
  concat <$> mapM (loadProtocolFile monad isIO . (path </>)) protocol_files

qname :: String -> QName
qname x = QName x Nothing Nothing

loadProtocolFile :: Type -> Bool -> FilePath -> Q [Dec]
loadProtocolFile monad isIO path = do
  unless isIO $ addDependentFile path
  protocols <- filter ((== qname "protocol") . elName) . onlyElems . parseXML <$> runIO (BS.readFile path)
  runIO $ print protocols
  concat <$> mapM 
    ((<&> concat) . mapM (loadInterface monad) . findChildren (qname "interface"))
    protocols

mkParserChain :: String -> [Function a] -> Q Exp
mkParserChain interfaceName [x] = appE (varE $ mkName $ interfaceName <> "_" <> x.name <> "Parser") $ varE $ mkName "dat"
mkParserChain interfaceName (x:xs) = infixApp (mkParserChain interfaceName xs) op (appE (varE $ mkName $ interfaceName <> "_" <> x.name <> "Parser") $ varE $ mkName "dat") 
  where op = varE (mkName "<|>")
mkParserChain _ [] = [e|undefined|]--[e| Parser <$> const Nothing |]

getString :: Get BS.ByteString
getString = do 
  len <- getWord32le
  str <- getBytes $ fromIntegral len
      --str = BS.take len (BS.drop 4 bytes)
  let padding = (4 - (len `mod` 4)) `mod` 4
  _ <- getBytes $ fromIntegral padding
  pure str

collectArgs :: Type -> ([Type], Type)
collectArgs (AppT (AppT ArrowT a) b) =
  let (args, ret) = collectArgs b
  in (a : args, ret)
collectArgs t = ([], t)

genBinds :: [Type] -> Name -> Q ([Name], [Stmt])
genBinds tys additionaldata = do
  let names = fmap (mkName . ("arg" ++) . show) [1..length tys]
  stmts <- sequence
    [ do
        e <- getForType ty
        pure (BindS (VarP n) $ AppE e $ VarE additionaldata)
    | (n, ty) <- zip names tys
    ]
  pure (names, stmts)


getFixed24_8 :: Get Double
getFixed24_8 = do
    raw <- getInt32be
    return $ fromIntegral raw / 256.0


data AdditionalParserData = AdditionalParserData {
    fds :: [Fd]
  }

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

adata :: Name
adata = mkName "additionalData"

mkParser :: Type -> String -> Function a -> Q [Dec]
mkParser monad interfaceName f = do
  (argNames, argStmts) <- genBinds f.fType adata
  opCheck <- [| getWord16le |] <&> BindS (VarP (mkName "op"))
  guardStmt <- [| guard (op == $(lit)) |] <&> NoBindS
  let func = VarE $ mkName $ functCase interfaceName <> "_" <> f.name
      args = map VarE argNames
      ret  = NoBindS $ AppE (VarE (mkName "pure")) (foldl AppE func args)
  --ret <- [| $(f.name) $(argNameList)|] <&> NoBindS
  let body = DoE Nothing (opCheck : guardStmt : argStmts ++ [ret])
  pure
    --     name:: InterfaceNameD -> AdditionalParserData -> Get (Interface (a) -> Wayland ())
    [ SigD name (ForallT 
          [PlainTV (mkName "a") SpecifiedSpec]
          [AppT (ConT $ mkName $ "Interface_" <> interfaceName) (VarT (mkName "a"))]
          $ AppT (AppT ArrowT $ ConT ''AdditionalParserData) (AppT (ConT ''Get) $ AppT (AppT ArrowT $ VarT $ mkName "a") (AppT monad (TupleT 0))))
    , FunD name [Clause [VarP adata] (NormalB body) []]
    ]
  where
    name = mkName $ interfaceName <> "_" <> f.name <> "Parser"
    lit = litE $ integerL $ fromIntegral f.opcode

loadInterface :: Type -> Element -> Q [Dec]
loadInterface monad int = do 

  events' <- loadF "event"
  requests' <- loadF "request"
  let definitions = fmap (mkFunction monad name') requests' ++ fmap (mkFunction monad name') events'

  parserChain <- mkParserChain name' events'
  let parserChainD = FunD eventParserName [ Clause [VarP (mkName "dat")] (NormalB parserChain) [] ]
  reqParserChain <- mkParserChain name' requests'
  let reqParserChainD = FunD requestParserName [ Clause [VarP (mkName "dat")] (NormalB reqParserChain) [] ]

  concat <$> sequence [
    -- Version
      [d| $(varP (mkName $ name' <> "Version")) = $(pure . LitE . IntegerL $ version') |]
    -- Class definition
    , pure [mkClass ("Interface_" <> name') ["a"] definitions]
    -- Request Parsers
    , concat <$> mapM (mkParser monad name') requests'
    , singleton <$> ([t| $(conT . mkName $ "Interface_" <> name') $a => AdditionalParserData -> Get ($a -> $(pure monad) ()) |] <&> SigD requestParserName)
    , pure [reqParserChainD]
    -- Event Parsers
    , concat <$> mapM (mkParser monad name') events'
    , singleton <$> ([t| $(conT . mkName $ "Interface_" <> name') $a => AdditionalParserData -> Get ($a -> $(pure monad) ()) |] <&> SigD eventParserName)
    , pure [parserChainD]
    -- Enums
    , pure $ concatMap (uncurry $ mkEnum name') enums'
    ]
  where
    a = varT $ mkName "a"
    eventParserName = mkName $ "eventParser_" <> name'
    requestParserName = mkName $ "requestParser_" <> name'
    name' = fromJust $ findAttr (qname "name") int
    loadF x = zipWithM loadFunction [0..] $ findChildren (qname x) int
    version' = read . fromJust $ findAttr (qname "version") int
    enums' = loadEnum <$> findChildren (qname "enum") int
loadEnum :: Element -> (String, [(String, Int)])
loadEnum e' = (fromJust $ findAttr (qname "name") e', f <$> findChildren (qname "entry") e')
  where
    f e = (fromJust $ findAttr (qname "name") e, read $ fromJust $ findAttr (qname "value") e)
loadFunction :: Word16 -> Element -> Q (Function Event)
loadFunction opcode element = stack (findChildren (qname "arg") element) <&> \x -> Function 
  {
    opcode = opcode
  , name = fromJust $ findAttr (qname "name") element
  , fType = x
  }
  where
    -- TODO convert stack to [Type]. Build the horrors beyond comprehension from the list instead. the list will be useful elsewhere.

    --stack' x = stack x <&> AppT (AppT ArrowT $ VarT (mkName "a"))
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
