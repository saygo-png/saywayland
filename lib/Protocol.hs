{-# LANGUAGE TemplateHaskell, DataKinds, OverloadedStrings #-}
module Protocol where
-- this file's purpose is to define all requests and events that exist and should be implemented. Implementing them is handled in Wayland/Core.hs

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Saywayland.Types (Wayland, ObjectID)

import Prelude
import Control.Applicative
import Control.Monad (guard)
import Data.Functor
import Data.Word (Word16,Word32)
import Data.Maybe (fromJust,Maybe,Maybe(Just,Nothing))
import Data.Char (toUpper, toLower)
import Data.List (singleton)
import Data.Binary (Get)
import Data.Binary.Get (getWord16le, getWord32le, getBytes)
import qualified Data.ByteString as BS
import qualified Data.Text as T

import System.Posix (Fd)
import System.Directory (listDirectory)
import System.FilePath (takeExtension)
import System.FilePath ((</>))

import Text.XML.Light

classCase (x:xs) = toUpper x : xs
functCase (x:xs) = toLower x : xs

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
mkFunction :: Function a -> Dec
mkFunction f = SigD (mkName f.name) $ constructType $ f.fType <> [{-a is the instance the data the class interface is implemented to-} ConT (mkName "a"), AppT (ConT ''Wayland) $ TupleT 0]
-- example output:
-- data EnumName = A | B | C | D ... deriving Eq
-- enumName' A = 1 ...
mkEnum :: String -> [(String,Int)] -> [Dec]
mkEnum enumName enumKV = [
    DataD [] enumName' [] Nothing constructors [DerivClause Nothing [ConT ''Eq]]
  , SigD funName (AppT (AppT ArrowT (ConT enumName')) (ConT ''Int))
  , FunD funName clauses
  ]
  where
    enumName' = mkName $ classCase enumName <> "Enum"
    funName = mkName $ functCase enumName <> "Enum'"
    constructors = fmap ((`NormalC` []) . mkName) $ fmap fst enumKV
    clauses = [Clause [ConP (mkName k) [] []] (NormalB (LitE (IntegerL (fromIntegral v)))) [] | (k,v)<-enumKV]

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


loadProtocols :: FilePath -> Q [Dec]
loadProtocols path = do
  --addDependentFile path
  protocol_files <- filter ((==".xml") . takeExtension) <$> runIO (listDirectory path)
  concat <$> mapM (loadProtocolFile . (path </>)) protocol_files

qname :: String -> QName
qname x = QName x Nothing Nothing

loadProtocolFile :: FilePath -> Q [Dec]
loadProtocolFile path = do
  --addDependentFile path
  protocols <- filter ((==(qname "protocol")) . elName) . onlyElems . parseXML <$> runIO (BS.readFile path)
  runIO $ print protocols
  concat <$> mapM 
    ((<&>concat) . mapM loadInterface . findChildren (qname "interface"))
    protocols

newtype Parser a = Parser {runParser :: BS.ByteString -> Maybe (BS.ByteString, a)}

instance Functor Parser where
  fmap f (Parser p) = 
    Parser $ \input -> do
      (input', x) <- p input
      Just(input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = 
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> 
    p1 input <|> p2 input


mkParserChain :: [Function a] -> Q Exp
mkParserChain [x] = varE $ mkName $ x.name <> "Parser"
mkParserChain (x:xs) = infixApp (mkParserChain xs) op (varE $ mkName $ x.name <> "Parser") 
  where op = varE (mkName "<|>")
mkParserChain [] = [e|undefined|]--[e| Parser <$> const Nothing |]

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

genBinds :: [Type] -> Q ([Name], [Stmt])
genBinds tys = do
  names <- mapM (\i -> newName ("arg" ++ show i)) [1..length tys]
  stmts <- sequence
    [ do
        e <- getForType ty
        pure (BindS (VarP n) e)
    | (n, ty) <- zip names tys
    ]
  pure (names, stmts)

getForType :: Type -> Q Exp
getForType t = case t of
  ConT name
    | name == ''Int       -> [| fromIntegral <$> getWord32le |]
    | name == ''Word32    -> [| getWord32le |]
    | name == ''BS.ByteString    -> [| getString |]
    | name == ''Double    -> [| getFixed24_8 |]
    | name == ''ObjectID  -> [| getWord32le |]
    | name == ''Fd        -> [| {-fuck.-} getFd |]
  _ -> error $ "unsupported type" <> show t

mkParser :: Function a -> Q [Dec]
mkParser f = do
  (_argNames, argStmts) <- genBinds $ f.fType
  opCheck <- [| getWord16le |] <&> BindS (VarP (mkName "op"))
  guardStmt <- [| guard (op == $(lit)) |] <&> NoBindS
  ret <- [| pure () |] <&> NoBindS
  let body = DoE Nothing (opCheck : guardStmt : argStmts ++ [ret])
  pure
    [ SigD name (AppT (ConT ''Get) (AppT (ConT ''Wayland) (TupleT 0)))
    , FunD name [Clause [] (NormalB body) []]
    ]
  where
    name = mkName $ f.name <> "Parser"
    lit = litE $ integerL $ fromIntegral f.opcode


{-mkParser :: Function a -> Parser (Wayland.Types.Wayland ())
mkParser f = expectBytes (fromIntegral f.opcode) *> 

expectBytes :: BS.ByteString -> Parser BS.ByteString
expectBytes bs = Parser f
  where
    f x = case BS.stripPrefix bs x of
      Nothing -> Nothing
      Just y  -> Just (x, y)
-}

loadInterface :: Element -> Q [Dec]
loadInterface int = do 

  events' <- loadF "event"
  requests' <- loadF "request"
  let definitions = fmap mkFunction events' ++ fmap mkFunction requests'

  let parserChain = mkParserChain events'
  let reqParserChain = mkParserChain requests'

  concat <$> sequence [
    -- Version
      [d| $(varP (mkName $ name' <> "Version")) = $(pure . LitE . IntegerL $ version') |]
    -- Class definition
    , pure [mkClass (name' <> "D") ["a"] definitions]
    -- Event Parsers
    , concat <$> mapM mkParser events'
    , (singleton) <$> ([t| Parser (Wayland ()) |] <&> SigD eventParserName)
    , [d|
      $(varP eventParserName) = $(parserChain)
      |]
    -- Request Parsers
    , concat <$> mapM mkParser requests'
    , (singleton) <$> ([t| Parser (Wayland ()) |] <&> SigD requestParserName)
    , [d|
      $(varP requestParserName) = $(reqParserChain)
      |]
    -- Enums
    , pure $ concatMap (uncurry mkEnum) enums'
    ]
  where
    eventParserName = mkName $ (name' <> "EventParser")
    requestParserName = mkName $ (name' <> "RequestParser")
    name' = fromJust $ findAttr (qname "name") int
    loadF x = mapM (uncurry loadFunction) $ zip [0..] $ findChildren (qname x) int
    version' = read . fromJust $ findAttr (qname "version") int
    enums' = fmap loadEnum $ findChildren (qname "enum") int
loadEnum :: Element -> (String, [(String, Int)])
loadEnum e' = (fromJust $ findAttr (qname "name") e', fmap f $ findChildren (qname "entry") e')
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
    stack (x:xs) = (argType x++) <$> stack xs
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
