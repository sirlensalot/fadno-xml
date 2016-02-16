{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Generate code using types emitted from XSD.
module Fadno.Xml.Codegen
    (
     -- * Codegen API
     outputTypes,outputType,outputHeader
    -- * Monad
    ,Output
    ,OutputState(..),names
    ,OutputEnv(..),handle
    ,runOut,runOut'
    ) where

import Fadno.Xml.EmitTypes
import Fadno.Xml.ParseXsd
import Control.Lens hiding (Choice,element,elements)
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Map.Strict as M
import Data.Char
import System.IO
import Data.List (intercalate)
import Data.Maybe


-- | Codegen state.
data OutputState = OutputState { _names :: M.Map String Name }
$(makeLenses ''OutputState)
-- | Codegen reader environment.
data OutputEnv = OutputEnv { _handle :: Handle }
$(makeLenses ''OutputEnv)

-- | Codegen monad.
type Output a = ReaderT OutputEnv (StateT OutputState IO) a

-- | Enumerate types to avoid in mangling.
defaultNames :: M.Map String Name
defaultNames = M.fromList $ map (\n -> (n,Name NSBuiltIn (QN n Nothing) 0))
               ["Eq","Typeable","Generic","Ord","Bounded",
                "Enum","Num","Real","Integral","Show",
               "String","Double","Float","Boolean","Int"]

-- | Run output monad.
runOut :: OutputEnv -> OutputState -> Output a -> IO (a, OutputState)
runOut e s a = runStateT (runReaderT a e) s

-- | Convenience runner.
runOut' :: Handle -> Output a -> IO (a, OutputState)
runOut' h = runOut (OutputEnv h) (OutputState defaultNames)

-- | putStr in codegen.
outStr :: String -> Output ()
outStr s = view handle >>= \h -> liftIO $ hPutStr h s

-- | putStrLn in codegen.
outStrLn :: String -> Output ()
outStrLn s = view handle >>= \h -> liftIO $ hPutStrLn h s

-- | indent.
indent :: Int -> Output ()
indent i = outStr $ replicate i ' '

-- | Output all types.
outputTypes :: EmitState -> Output ()
outputTypes = mapM_ outputType . M.elems . _types

-- | Comment header.
header :: Name -> Maybe Documentation -> Output ()
header (Name ns n i) doc = do
  outStrLn ""
  outStr $ "-- | @" ++ show n ++ "@"
  outStrLn $ " /(" ++ drop 2 (map toLower $ show ns) ++ ")/"
  case doc of
    Nothing -> return ()
    Just (Documentation d) ->
        do
          outStrLn "--"
          let ls = lines d
          if length ls < 10 -- lame "longer docs" heuristic
          then mapM_ (outStrLn . ("-- " ++)) ls
          else do
            -- longer docs: head unformatted
            outStrLn $ "-- " ++ head ls
            -- explicit formatting for tail
            outStrLn "--"
            outStrLn "-- @"
            mapM_ (outStrLn . ("-- " ++)) (tail ls)
            outStrLn "-- @"
  when (i > 0) $ do
    outStrLn ""
    outStrLn $ "-- mangled: " ++ show i


-- | Codegen a type.
outputType :: Type -> Output ()
outputType (BuiltIn {}) = return ()

-- NEWTYPE --
outputType nt@(NewType {..}) = do
  header _typeName _typeDoc
  mn <- mangleType nt
  mf <- mangleField _typeName "" 0
  rt <- refType _typeType
  outStrLn $ "newtype " ++ mn ++ " = " ++ mn ++
               " { " ++ mf ++ " :: " ++ rt ++ " }"
  outStrLn $ "    deriving (" ++ outputDerives _typeDerives ++ ")"
  mapM_ (outputImpls nt) _typeImpls
  outputEmitXml mn
  outStrLn $ "    emitXml = emitXml . " ++ mf
  -- PARSING
  outStrLn $ "parse" ++ mn ++ " :: String -> P.XParse " ++ mn
  if _typeDerives == NewTypeString
  then outStrLn $ "parse" ++ mn ++ " = return . fromString"
  else outStrLn $ "parse" ++ mn ++ " = P.xread \"" ++ mn ++ "\""


-- DATA --
outputType dt@(DataType {..}) = do
  header _typeName _typeDoc
  mn <- mangleType dt
  outStrLn $ "data " ++ mn ++ " = "
  forM_ (zip [(0 :: Int)..] _typeCtors) $ \(i,Ctor {..}) ->
      do
        outStr (if i > 0 then "    | " else "      ")
        mangleCtor _typeName _ctorName >>= outStr
        if null _ctorFields then outStrLn ""
        else do
          outStrLn " {"
          forM_ (zip [(0 :: Int)..] _ctorFields) $ \(j,Field fn ft fc femit fi) ->
            do
              outStr (if j > 0 then "        , " else "          ")
              rt <- refType ft
              mf <- mangleField _typeName (_qLocal fn) fi
              let docs = if _typeEmit == DataTypeSimple then ""
                         else case femit of
                           FieldAttribute -> " -- ^ /" ++ show fn ++ "/ attribute"
                           FieldElement -> " -- ^ /" ++ show fn ++ "/ child element"
                           FieldText -> " -- ^ text content"
                           FieldOther -> ""

              outStrLn $ mf ++ " :: " ++ card fc rt ++ docs
          outStrLn "       }"
  outStrLn $ "    deriving (" ++ outputDerives _typeDerives ++ ")"
  -- EmitXml instance
  outputEmitXml mn
  forM_ _typeCtors $ \(Ctor {..}) -> do
    mcn <- mangleCtor _typeName _ctorName
    case _typeEmit of
      DataTypeSimple ->
          outStrLn $ "    emitXml (" ++ mcn ++ " a) = emitXml a"
      _ -> do
        let fas = zip fieldArgs _ctorFields
            genEls [] = "[]"
            genEls es = "(" ++ intercalate " ++\n        "
                        (map (\f -> genEl (_fieldXmlEmit (snd f)) f) es) ++ ")"
            genEl FieldElement f = genPart "XElement" f
            genEl FieldOther (c,_) = "[emitXml " ++ c ++ "]"
            genEl _ f = error "c'est impossible: " ++ show f
            genParts _ [] = "[]"
            genParts xctor ffs = "(" ++ intercalate " ++\n        " (map (genPart xctor) ffs) ++ ")"
            genPart xctor (c,Field fn _ fc _ _) =
                case fc of
                  One -> "[" ++ genct xctor fn ++ " (emitXml " ++ c ++ ")]"
                  ZeroOrOne -> "[maybe XEmpty (" ++ genct xctor fn ++ ".emitXml) " ++ c ++ "]"
                  Many -> "map (" ++ genct xctor fn ++ ".emitXml) " ++ c
            genct x q = x ++ " " ++ genqn q
            genqn (QN l p) = "(QN \"" ++ l ++ "\" " ++
                             maybe "Nothing" (\v -> "(Just \"" ++ v ++ "\")") p ++
                             ")"
            genreps ffs = "[" ++ intercalate "," (map (("emitXml "++).fst) ffs) ++ "]"
            findFields fpred = filter (fpred . _fieldXmlEmit . snd) fas
            oths = findFields (==FieldOther)
        outStrLn $ "    emitXml (" ++ mcn ++ concatMap ((" " ++) . fst) fas  ++ ") ="
        if length oths < length fas  -- heuristic for "passthrough" compositors
        then do
          indent 6
          if TopLevel `elem` _typeImpls
          then outStr ("XElement " ++ genqn (nName _typeName) ++ " $ XContent ")
          else outStr "XContent "
          case map fst $ findFields (==FieldText) of
            [c] -> outStrLn $ "(emitXml " ++ c ++ ")"
            [] -> outStrLn "XEmpty"
            _ -> die $ "More than one text field: " ++ show dt
          indent 8 >> outStrLn (genParts "XAttr" (findFields (==FieldAttribute)))
          indent 8 >> outStrLn (genEls (findFields (`elem` [FieldElement,FieldOther])))

        else
          indent 6 >> outStrLn ("XReps " ++ genreps fas)
  -- PARSING

  if _typeEmit == DataTypeSimple
  then do
    outStrLn $ "parse" ++ mn ++ " :: String -> P.XParse " ++ mn
    outStrLn $ "parse" ++ mn ++ " s = "
  else do
    outStrLn $ "parse" ++ mn ++ " :: P.XParse " ++ mn
    outStrLn $ "parse" ++ mn ++ " = "
  forM_ (zip [(0 :: Int)..] _typeCtors) $ \(j,Ctor {..}) -> do
    mcn <- mangleCtor _typeName _ctorName
    outStr "      "
    when (j > 0) $ outStr "<|> "
    if null _ctorFields
    then outStrLn $ "return " ++ mcn
    else outStrLn mcn
    forM_ (zip [(0 :: Int) ..] _ctorFields) $ \(i,Field {..}) ->
      do
        outStr $ "        " ++ (if i == 0 then "<$> " else "<*> ")
        ftn <- mangleType _fieldType
        case _typeEmit of
          DataTypeSimple -> outStrLn $ parseFun ftn ++ " s"
          _ ->
            do
              let pname = "(P.name \"" ++ show _fieldName ++ "\")"
                  parser = parseFun ftn
                  attrParse = "(P.xattr " ++ pname ++ " >>= " ++ parser ++ ")"
                  elParse = parseEl parser pname _fieldType
                  -- gross heuristic to handle horrible musicxml things
                  pmany | length _ctorFields == 1 && length _typeCtors > 1 = "P.some"
                        | otherwise = "P.many"
              outStrLn $ case (_fieldXmlEmit,_fieldCardinality) of
                (FieldAttribute,ZeroOrOne) -> "P.optional " ++ attrParse
                (FieldAttribute,_) -> attrParse
                (FieldText,_) -> "(P.xtext >>= " ++ parser ++ ")"
                (FieldElement,Many) -> pmany ++ " " ++ elParse
                (FieldElement,ZeroOrOne) -> "P.optional " ++ elParse
                (FieldElement,One) -> elParse
                (FieldOther,ZeroOrOne) -> "P.optional (" ++ parser ++ ")"
                (FieldOther,Many) -> "P.many (" ++ parser ++ ")"
                (FieldOther,_) -> parser


  outStrLn ""
  -- smart ctors
  unless (_typeEmit == DataTypeSimple) $
         forM_ _typeCtors $ \(Ctor {..}) ->
           do
             mcn <- mangleCtor _typeName _ctorName
             let fas = zip fieldArgs _ctorFields
             mfs <- forM fas $ \(c,Field _ ft fc _ _) ->
                     case fc of
                       One -> (c,) . Just . (c,) <$> refType ft
                       ZeroOrOne -> return ("Nothing",Nothing)
                       Many -> return ("[]",Nothing)
             let args = mapMaybe snd mfs
             outStrLn $ "-- | Smart constructor for '" ++ mcn ++ "'"
             outStrLn $ "mk" ++ mcn ++ " :: " ++ concatMap ((++ " -> ") . snd) args ++ mn
             outStrLn $ "mk" ++ mcn ++ " " ++ concatMap ((++ " ") . fst) args ++
                          "= " ++ mcn ++ " " ++ unwords (map fst mfs)
  mapM_ (outputImpls dt) _typeImpls


-- ENUM --
outputType et@(EnumType {..}) = do
  header _typeName _typeDoc
  mn <- mangleType et
  outStrLn $ "data " ++ mn ++ " = "
  forM_ (zip [(0 :: Int)..] _typeEnumValues) $ \(i,s) ->
      do
        outStr (if i > 0 then "    | " else "      ")
        mangleCtor _typeName s >>= \e -> outStrLn $ e ++ " -- ^ /" ++ s ++ "/"
  outStrLn $ "    deriving (" ++ outputDerives _typeDerives  ++ ")"
  mapM_ (outputImpls et) _typeImpls
  outputEmitXml mn
  forM_ _typeEnumValues $ \s -> do
    cn <- mangleCtor _typeName s
    outStrLn $ "    emitXml " ++ cn ++ " = XLit \"" ++ s ++ "\""
  -- PARSING
  outStrLn $ "parse" ++ mn ++ " :: String -> P.XParse " ++ mn
  outStrLn $ "parse" ++ mn ++ " s"
  forM_  _typeEnumValues $ \s ->
      do
        cn <- mangleCtor _typeName s
        outStrLn $ "        | s == \"" ++ s ++ "\" = return $ " ++ cn
  outStrLn $ "        | otherwise = P.xfail $ \"" ++ mn ++ ": \" ++ s"


-- | breaking off because RecordWildCards breaks haskell
parseEl :: String -> String -> Type -> String
parseEl parser pname fType =
    case firstOf typeEmit fType of
      Just DataTypeSimple -> simpleEl
      Nothing -> simpleEl
      _ -> "(P.xchild " ++ pname ++ " (" ++ parser ++ "))"
    where simpleEl = "(P.xchild " ++ pname ++ " (P.xtext >>= " ++ parser ++ "))"

parseFun :: String -> String
parseFun tn | tn == "Decimal" = rp
            | tn == "DefString" = "return"
            | tn == "Integer" = rp
            | otherwise = "parse" ++ tn
            where rp = "(P.xread \"" ++ tn ++ "\")"

-- | List of usable field arguments.
fieldArgs :: [String]
fieldArgs = concatMap (\p -> map ((++p).pure) ['a'..'z']) ("": map pure ['1'..'9'])

-- | Begin an EmitXml instance.
outputEmitXml :: String -> Output ()
outputEmitXml typename =
  outStrLn $ "instance EmitXml " ++ typename ++ " where"

-- | Codegen for cardinality (Maybe or List).
card :: Cardinality -> String -> String
card One s = s
card ZeroOrOne s = "(Maybe " ++ s ++ ")"
card Many s = "[" ++ s ++ "]"

-- | Mangling for type names.
mangleType :: Type -> Output String
mangleType = m . _typeName where
    m n@(Name _ bare _) = mangle n (firstUpper $ fixChars (_qLocal bare)) firstUpper


-- | Run mangling rules.
mangle :: Name -> String -> (String -> String) -> Output String
mangle n@(Name ns _ i) tname mangledFun =
  tryName n tname $ do
    let pfx NSBuiltIn = "Def"
        pfx NSComplex = "Cmp"
        pfx NSUnion = "Sum"
        pfx NSSimple = "Smp"
        pfx NSElement = "El"
        pfx NSChoice = "Chx"
        pfx NSSequence = "Seq"
        pfx NSGroup = "Grp"
        tnameP = mangledFun $ pfx ns ++ tname
    tryName n tnameP $ do
                    let tnamei = tnameP ++ show i
                    tryName n tnamei $
                            die $ "type already exists for mangled name: " ++ tnamei

-- | Check if name exists, if not register it.
tryName :: Name -> String -> Output String -> Output String
tryName n tn ifnot = do
  fn <- M.lookup tn <$> use names
  case fn of
    Nothing -> do
            names %= M.insert tn n
            return tn
    (Just found)
        | found == n -> return tn
        | otherwise -> ifnot

-- | Type/Ctor naming.
firstUpper :: String -> String
firstUpper (s:ss) = toUpper s:ss
firstUpper [] = []
-- | Field naming.
firstLower :: String -> String
firstLower (s:ss) = toLower s:ss
firstLower [] = []

-- | Mangling for fields.
mangleField :: Name -> String -> Int -> Output String
mangleField nm n i = mangle nm
                     (firstLower $ fixChars (_qLocal (nName nm) ++ firstUpper n ++ if i > 0 then show i else ""))
                     firstLower

-- | Mangling for ctors.
mangleCtor :: Name -> String -> Output String
mangleCtor nm n = mangle nm (firstUpper $ fixChars (_qLocal (nName nm) ++ firstUpper n)) firstUpper

-- | Substitute valid Haskell chars.
fixChars :: String -> String
fixChars = reverse . snd . foldl fc (True,"")
    where fc (uc,s) c | c `elem` ("- :" :: String) = (True,s)
                      | otherwise = (False,(if uc then toUpper c else c):s)

-- | Get referred type name, handling builtins.
refType :: Type -> Output String
refType t@(BuiltIn {}) = return $ drop 2 $ show (_coreType t)
refType t = mangleType t

-- | Output derive types.
outputDerives :: DerivesFamily -> String
outputDerives NewTypeIntegral = allDerives ++ "Ord,Bounded,Enum,Num,Real,Integral"
outputDerives NewTypeNum = allDerives ++ "Ord,Num,Real,Fractional,RealFrac"
outputDerives NewTypeString = allDerives ++ "Ord,IsString"
outputDerives OtherDerives = allDerives ++ "Show"
outputDerives DataEnum = allDerives ++ "Show,Ord,Enum,Bounded"

-- | Common derived types.
allDerives :: String
allDerives = "Eq,Typeable,Generic,"

-- | Handle impls.
-- | TODO patterns, bounds.
outputImpls :: Type -> Impl -> Output ()
outputImpls t NewTypeShow = do
  tn <- refType t
  outStrLn $ "instance Show " ++ tn ++ " where show (" ++ tn ++ " a) = show a"
  outStrLn $ "instance Read " ++ tn ++ " where readsPrec i = map (A.first " ++ tn ++ ") . readsPrec i"
outputImpls _ _ = return ()

-- | Output pragmas, module, imports.
outputHeader :: String -> Output ()
outputHeader moduleName = mapM_ outStrLn
    [ "{-# LANGUAGE TupleSections #-}"
    , "{-# LANGUAGE DeriveGeneric #-}"
    , "{-# LANGUAGE FlexibleContexts #-}"
    , "{-# LANGUAGE DeriveDataTypeable #-}"
    , "{-# LANGUAGE TemplateHaskell #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE DeriveDataTypeable #-}"
    , "{-# LANGUAGE MultiParamTypeClasses #-}"
    , ""
    , "module " ++ moduleName ++ " where"
    , ""
    , "import GHC.Generics"
    , "import Data.Data"
    , "import Data.Decimal"
    , "import Data.String"
    , "import Fadno.Xml.EmitXml"
    , "import qualified Fadno.Xml.XParse as P"
    , "import qualified Control.Applicative as P"
    , "import Control.Applicative ((<|>))"
    , "import Control.Arrow as A"
    ]
