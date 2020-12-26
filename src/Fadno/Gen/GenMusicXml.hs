{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Fadno.Gen.GenMusicXml where

import Fadno.Xml.ParseXsd
import Fadno.Xml.EmitTypes
import Fadno.Xml.Codegen
import Data.Monoid
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import System.IO

-- | Emit the first element only.
emitOneElement :: IO (Type,EmitState)
emitOneElement = do
  s <- loadMusicXml "xsd/musicxml.20.xsd"
  runEmit (Env s) mempty $ emitElement (head $ M.elems $ _elements s)

-- | Emit 2.0 code.
runMusicXml20 :: IO ()
runMusicXml20 = runMusicXml "xsd/musicxml.20.xsd" "src/Fadno/MusicXml/MusicXml20.hs" "Fadno.MusicXml.MusicXml20"

-- | Emit 3.0 code.
runMusicXml30 :: IO ()
runMusicXml30 = runMusicXml "xsd/musicxml.30.xsd" "src/Fadno/MusicXml/MusicXml30.hs" "Fadno.MusicXml.MusicXml30"

-- | Emit 3.1 code.
runMusicXml31 :: IO ()
runMusicXml31 = runMusicXml "xsd/musicxml.31.xsd" "src/Fadno/MusicXml/MusicXml31.hs" "Fadno.MusicXml.MusicXml31"


runMusicXml :: FilePath -> FilePath -> String -> IO ()
runMusicXml xsd moduleFile moduleName = do
  s <- loadMusicXml xsd
  e <- snd <$> runEmit (Env s) mempty (emitSchema s)
  withFile moduleFile WriteMode $ \h ->
      void $ runOut' h $ do
                  outputHeader moduleName
                  outputTypes e


-- | Load XSD,XML and Xlink schemas.
loadXlinkXmlSchemas :: IO Schema
loadXlinkXmlSchemas = do
  xml <- namespaceSchema "xml" <$> parseFile "xsd/xml.xsd"
  xlink <- namespaceSchema "xlink" <$> parseFile "xsd/xlink.xsd"
  xsd <- loadXsdSchema "xsd/XMLSchema.xsd"
  return (xml <> xlink <> xsd)

-- | Load Music XML XSD and deps
loadMusicXml :: FilePath -> IO Schema
loadMusicXml xsd = do
  x <- parseFile xsd
  deps <- loadXlinkXmlSchemas
  return (x <> deps)
