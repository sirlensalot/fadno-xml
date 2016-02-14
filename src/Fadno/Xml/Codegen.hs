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
outputType nt@(NewType n t ds is doc) = do
  header n doc
  mn <- mangleType n
  mf <- mangleField n "" 0
  rt <- refType t
  outStrLn $ "newtype " ++ mn ++ " = " ++ mn ++
               " { " ++ mf ++ " :: " ++ rt ++ " }"
  outStrLn $ "    deriving (" ++ outputDerives ds ++ ")"
  mapM_ (outputImpls nt) is
  outputEmitXml mn
  outStrLn $ "    emitXml = emitXml . " ++ mf



-- DATA --
outputType dt@(DataType n ctors ds is dtemit doc) = do
  header n doc
  mn <- mangleType n
  outStrLn $ "data " ++ mn ++ " = "
  forM_ (zip [(0 :: Int)..] ctors) $ \(i,Ctor cn fs) ->
      do
        outStr (if i > 0 then "    | " else "      ")
        mangleCtor n cn >>= outStr
        if null fs then outStrLn ""
        else do
          outStrLn " {"
          forM_ (zip [(0 :: Int)..] fs) $ \(j,Field fn ft fc femit fi) ->
            do
              outStr (if j > 0 then "        , " else "          ")
              rt <- refType ft
              mf <- mangleField n (_qLocal fn) fi
              let docs = if dtemit == DataTypeSimple then ""
                         else case femit of
                           FieldAttribute -> " -- ^ /" ++ show fn ++ "/ attribute"
                           FieldElement -> " -- ^ /" ++ show fn ++ "/ child element"
                           FieldText -> " -- ^ text content"
                           FieldOther -> ""

              outStrLn $ mf ++ " :: " ++ card fc rt ++ docs
          outStrLn "       }"
  outStrLn $ "    deriving (" ++ outputDerives ds ++ ")"
  -- EmitXml instance
  outputEmitXml mn
  forM_ ctors $ \(Ctor cn fs) -> do
    mcn <- mangleCtor n cn
    case dtemit of
      DataTypeSimple ->
          outStrLn $ "    emitXml (" ++ mcn ++ " a) = emitXml a"
      _ -> do
        let fas = zip fieldArgs fs
            genEls [] = "[]"
            genEls es = "(" ++ intercalate "++"
                        (map (\f -> genEl (_fieldXmlEmit (snd f)) f) es) ++ ")"
            genEl FieldElement f = genPart "XElement" f
            genEl FieldOther (c,_) = "[emitXml " ++ c ++ "]"
            genEl _ f = error "c'est impossible: " ++ show f
            genParts _ [] = "[]"
            genParts xctor ffs = "(" ++ intercalate "++" (map (genPart xctor) ffs) ++ ")"
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
          if elem TopLevel $ _typeImpls dt
          then outStr ("XElement " ++ genqn (nName n) ++ " $ XContent ")
          else outStr "XContent "
          case map fst $ findFields (==FieldText) of
            [c] -> outStrLn $ "(emitXml " ++ c ++ ")"
            [] -> outStrLn "XEmpty"
            _ -> die $ "More than one text field: " ++ show dt
          indent 8 >> outStrLn (genParts "XAttr" (findFields (==FieldAttribute)))
          indent 8 >> outStrLn (genEls (findFields (`elem` [FieldElement,FieldOther])))

          -- indent 8 >> outStrLn ("(" ++ genParts "XElement" (findFields (==FieldElement)))
          -- indent 8 >> outStrLn (" ++ " ++ genreps (findFields (==FieldOther)) ++ ")")
        else
          indent 6 >> outStrLn ("XReps " ++ genreps fas)
  -- smart ctors
  unless (dtemit == DataTypeSimple) $
         forM_ ctors $ \(Ctor cn fs) ->
           do
             mcn <- mangleCtor n cn
             let fas = zip fieldArgs fs
             mfs <- forM fas $ \(c,Field _ ft fc _ _) ->
                     case fc of
                       One -> (c,) . Just . (c,) <$> refType ft
                       ZeroOrOne -> return ("Nothing",Nothing)
                       Many -> return ("[]",Nothing)
             let args = mapMaybe snd mfs
             outStrLn $ "-- | Smart constructor for '" ++ mcn ++ "'"
             outStrLn $ "mk" ++ mcn ++ " :: " ++ concatMap ((++ " -> ") . snd) args ++ mn
             outStrLn $ "mk" ++ mcn ++ " " ++ concatMap ((++ " ") . fst) args ++ "= " ++ mcn ++ " " ++
                      unwords (map fst mfs)
  mapM_ (outputImpls dt) is


-- ENUM --
outputType et@(EnumType n vals ds is doc) = do
  header n doc
  mn <- mangleType n
  outStrLn $ "data " ++ mn ++ " = "
  forM_ (zip [(0 :: Int)..] vals) $ \(i,s) ->
      do
        outStr (if i > 0 then "    | " else "      ")
        mangleCtor n s >>= \e -> outStrLn $ e ++ " -- ^ /" ++ s ++ "/"
  outStrLn $ "    deriving (" ++ outputDerives ds  ++ ")"
  mapM_ (outputImpls et) is
  outputEmitXml mn
  forM_ vals $ \s -> do
    cn <- mangleCtor n s
    outStrLn $ "    emitXml " ++ cn ++ " = XLit \"" ++ s ++ "\""

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
mangleType :: Name -> Output String
mangleType n@(Name _ bare _) = mangle n (firstUpper $ fixChars (_qLocal bare)) firstUpper


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
refType t = mangleType $ _typeName t

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
outputImpls _ _ = return ()

-- | Output pragmas, module, imports.
outputHeader :: String -> Output ()
outputHeader moduleName = do
    outStrLn "{-# LANGUAGE TupleSections #-}"
    outStrLn "{-# LANGUAGE DeriveGeneric #-}"
    outStrLn "{-# LANGUAGE FlexibleContexts #-}"
    outStrLn "{-# LANGUAGE DeriveDataTypeable #-}"
    outStrLn "{-# LANGUAGE TemplateHaskell #-}"
    outStrLn "{-# LANGUAGE OverloadedStrings #-}"
    outStrLn "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    outStrLn "{-# LANGUAGE DeriveDataTypeable #-}"
    outStrLn "{-# LANGUAGE MultiParamTypeClasses #-}"
    outStrLn ""
    outStrLn $ "module " ++ moduleName ++ " where"
    outStrLn ""
    outStrLn "import GHC.Generics"
    outStrLn "import Data.Data"
    outStrLn "import Data.Decimal"
    outStrLn "import Data.String"
    outStrLn "import Fadno.Xml.EmitXml"
    outStrLn ""
