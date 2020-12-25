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
  s <- loadMusicXml20
  runEmit (Env s) mempty $ emitElement (head $ M.elems $ _elements s)

-- | Emit 2.0 code.
runMusicXml20 :: IO ()
runMusicXml20 = do
  s <- loadMusicXml20
  e <- snd <$> runEmit (Env s) mempty (emitSchema s)
  withFile "src/Fadno/MusicXml/MusicXml20.hs" WriteMode $ \h ->
      void $ runOut' h $ do
                  outputHeader "Fadno.MusicXml.MusicXml20"
                  outputTypes e

-- | Emit 3.0 code.
runMusicXml30 :: IO ()
runMusicXml30 = do
  s <- loadMusicXml30
  e <- snd <$> runEmit (Env s) mempty (emitSchema s)
  withFile "src/Fadno/MusicXml/MusicXml30.hs" WriteMode $ \h ->
      void $ runOut' h $ do
                  outputHeader "Fadno.MusicXml.MusicXml30"
                  outputTypes e


-- | Load XSD,XML and Xlink schemas.
loadXlinkXmlSchemas :: IO Schema
loadXlinkXmlSchemas = do
  xml <- namespaceSchema "xml" <$> parseFile "xsd/xml.xsd"
  xlink <- namespaceSchema "xlink" <$> parseFile "xsd/xlink.xsd"
  xsd <- loadXsdSchema "xsd/XMLSchema.xsd"
  return (xml <> xlink <> xsd)

-- | Load Music XML 2.0 schema
loadMusicXml20 :: IO Schema
loadMusicXml20 = do
  x <- parseFile "xsd/musicxml.20.xsd"
  deps <- loadXlinkXmlSchemas
  return (x <> deps)

-- | Load Music XML 2.0 schema
loadMusicXml30 :: IO Schema
loadMusicXml30 = do
  x <- parseFile "xsd/musicxml.30.xsd"
  deps <- loadXlinkXmlSchemas
  return (x <> deps)
