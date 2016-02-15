{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fadno.MusicXml.MusicXml20 where

import GHC.Generics
import Data.Data
import Data.Decimal
import Data.String
import Fadno.Xml.EmitXml
import qualified Fadno.Xml.XParser as P
import qualified Control.Applicative as P
import Control.Applicative ((<|>))
import Control.Arrow as A

-- | @xs:ID@ /(simple)/
newtype ID = ID { iD :: NCName }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show ID where show (ID a) = show a
instance Read ID where readsPrec i = map (A.first ID) . readsPrec i
instance EmitXml ID where
    emitXml = emitXml . iD
parseID :: String -> P.XParse ID
parseID = return . fromString

-- | @xs:IDREF@ /(simple)/
newtype IDREF = IDREF { iDREF :: NCName }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show IDREF where show (IDREF a) = show a
instance Read IDREF where readsPrec i = map (A.first IDREF) . readsPrec i
instance EmitXml IDREF where
    emitXml = emitXml . iDREF
parseIDREF :: String -> P.XParse IDREF
parseIDREF = return . fromString

-- | @xs:NCName@ /(simple)/
newtype NCName = NCName { nCName :: Name }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show NCName where show (NCName a) = show a
instance Read NCName where readsPrec i = map (A.first NCName) . readsPrec i
instance EmitXml NCName where
    emitXml = emitXml . nCName
parseNCName :: String -> P.XParse NCName
parseNCName = return . fromString

-- | @xs:NMTOKEN@ /(simple)/
newtype NMTOKEN = NMTOKEN { nMTOKEN :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show NMTOKEN where show (NMTOKEN a) = show a
instance Read NMTOKEN where readsPrec i = map (A.first NMTOKEN) . readsPrec i
instance EmitXml NMTOKEN where
    emitXml = emitXml . nMTOKEN
parseNMTOKEN :: String -> P.XParse NMTOKEN
parseNMTOKEN = return . fromString

-- | @xs:Name@ /(simple)/
newtype Name = Name { name :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Name where show (Name a) = show a
instance Read Name where readsPrec i = map (A.first Name) . readsPrec i
instance EmitXml Name where
    emitXml = emitXml . name
parseName :: String -> P.XParse Name
parseName = return . fromString

-- | @above-below@ /(simple)/
--
-- The above-below type is used to indicate whether one element appears above or below another element.
data AboveBelow = 
      AboveBelowAbove -- ^ /above/
    | AboveBelowBelow -- ^ /below/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml AboveBelow where
    emitXml AboveBelowAbove = XLit "above"
    emitXml AboveBelowBelow = XLit "below"
parseAboveBelow :: String -> P.XParse AboveBelow
parseAboveBelow s
        | s == "above" = return $ AboveBelowAbove
        | s == "below" = return $ AboveBelowBelow
        | otherwise = P.xfail $ "AboveBelow: " ++ s

-- | @accidental-value@ /(simple)/
--
-- The accidental-value type represents notated accidentals supported by MusicXML. In the MusicXML 2.0 DTD this was a string with values that could be included. The XSD strengthens the data typing to an enumerated list.
data AccidentalValue = 
      AccidentalValueSharp -- ^ /sharp/
    | AccidentalValueNatural -- ^ /natural/
    | AccidentalValueFlat -- ^ /flat/
    | AccidentalValueDoubleSharp -- ^ /double-sharp/
    | AccidentalValueSharpSharp -- ^ /sharp-sharp/
    | AccidentalValueFlatFlat -- ^ /flat-flat/
    | AccidentalValueNaturalSharp -- ^ /natural-sharp/
    | AccidentalValueNaturalFlat -- ^ /natural-flat/
    | AccidentalValueQuarterFlat -- ^ /quarter-flat/
    | AccidentalValueQuarterSharp -- ^ /quarter-sharp/
    | AccidentalValueThreeQuartersFlat -- ^ /three-quarters-flat/
    | AccidentalValueThreeQuartersSharp -- ^ /three-quarters-sharp/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml AccidentalValue where
    emitXml AccidentalValueSharp = XLit "sharp"
    emitXml AccidentalValueNatural = XLit "natural"
    emitXml AccidentalValueFlat = XLit "flat"
    emitXml AccidentalValueDoubleSharp = XLit "double-sharp"
    emitXml AccidentalValueSharpSharp = XLit "sharp-sharp"
    emitXml AccidentalValueFlatFlat = XLit "flat-flat"
    emitXml AccidentalValueNaturalSharp = XLit "natural-sharp"
    emitXml AccidentalValueNaturalFlat = XLit "natural-flat"
    emitXml AccidentalValueQuarterFlat = XLit "quarter-flat"
    emitXml AccidentalValueQuarterSharp = XLit "quarter-sharp"
    emitXml AccidentalValueThreeQuartersFlat = XLit "three-quarters-flat"
    emitXml AccidentalValueThreeQuartersSharp = XLit "three-quarters-sharp"
parseAccidentalValue :: String -> P.XParse AccidentalValue
parseAccidentalValue s
        | s == "sharp" = return $ AccidentalValueSharp
        | s == "natural" = return $ AccidentalValueNatural
        | s == "flat" = return $ AccidentalValueFlat
        | s == "double-sharp" = return $ AccidentalValueDoubleSharp
        | s == "sharp-sharp" = return $ AccidentalValueSharpSharp
        | s == "flat-flat" = return $ AccidentalValueFlatFlat
        | s == "natural-sharp" = return $ AccidentalValueNaturalSharp
        | s == "natural-flat" = return $ AccidentalValueNaturalFlat
        | s == "quarter-flat" = return $ AccidentalValueQuarterFlat
        | s == "quarter-sharp" = return $ AccidentalValueQuarterSharp
        | s == "three-quarters-flat" = return $ AccidentalValueThreeQuartersFlat
        | s == "three-quarters-sharp" = return $ AccidentalValueThreeQuartersSharp
        | otherwise = P.xfail $ "AccidentalValue: " ++ s

-- | @accordion-middle@ /(simple)/
--
-- The accordion-middle type may have values of 1, 2, or 3, corresponding to having 1 to 3 dots in the middle section of the accordion registration symbol.
newtype AccordionMiddle = AccordionMiddle { accordionMiddle :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show AccordionMiddle where show (AccordionMiddle a) = show a
instance Read AccordionMiddle where readsPrec i = map (A.first AccordionMiddle) . readsPrec i
instance EmitXml AccordionMiddle where
    emitXml = emitXml . accordionMiddle
parseAccordionMiddle :: String -> P.XParse AccordionMiddle
parseAccordionMiddle = P.xread "AccordionMiddle"

-- | @xlink:actuate@ /(simple)/
data Actuate = 
      ActuateOnRequest -- ^ /onRequest/
    | ActuateOnLoad -- ^ /onLoad/
    | ActuateOther -- ^ /other/
    | ActuateNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Actuate where
    emitXml ActuateOnRequest = XLit "onRequest"
    emitXml ActuateOnLoad = XLit "onLoad"
    emitXml ActuateOther = XLit "other"
    emitXml ActuateNone = XLit "none"
parseActuate :: String -> P.XParse Actuate
parseActuate s
        | s == "onRequest" = return $ ActuateOnRequest
        | s == "onLoad" = return $ ActuateOnLoad
        | s == "other" = return $ ActuateOther
        | s == "none" = return $ ActuateNone
        | otherwise = P.xfail $ "Actuate: " ++ s

-- | @backward-forward@ /(simple)/
--
-- The backward-forward type is used to specify repeat directions. The start of the repeat has a forward direction while the end of the repeat has a backward direction.
data BackwardForward = 
      BackwardForwardBackward -- ^ /backward/
    | BackwardForwardForward -- ^ /forward/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml BackwardForward where
    emitXml BackwardForwardBackward = XLit "backward"
    emitXml BackwardForwardForward = XLit "forward"
parseBackwardForward :: String -> P.XParse BackwardForward
parseBackwardForward s
        | s == "backward" = return $ BackwardForwardBackward
        | s == "forward" = return $ BackwardForwardForward
        | otherwise = P.xfail $ "BackwardForward: " ++ s

-- | @bar-style@ /(simple)/
--
-- The bar-style type represents barline style information. Choices are regular, dotted, dashed, heavy, light-light, light-heavy, heavy-light, heavy-heavy, tick (a short stroke through the top line), short (a partial barline between the 2nd and 4th lines), and none.
data BarStyle = 
      BarStyleRegular -- ^ /regular/
    | BarStyleDotted -- ^ /dotted/
    | BarStyleDashed -- ^ /dashed/
    | BarStyleHeavy -- ^ /heavy/
    | BarStyleLightLight -- ^ /light-light/
    | BarStyleLightHeavy -- ^ /light-heavy/
    | BarStyleHeavyLight -- ^ /heavy-light/
    | BarStyleHeavyHeavy -- ^ /heavy-heavy/
    | BarStyleTick -- ^ /tick/
    | BarStyleShort -- ^ /short/
    | BarStyleNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml BarStyle where
    emitXml BarStyleRegular = XLit "regular"
    emitXml BarStyleDotted = XLit "dotted"
    emitXml BarStyleDashed = XLit "dashed"
    emitXml BarStyleHeavy = XLit "heavy"
    emitXml BarStyleLightLight = XLit "light-light"
    emitXml BarStyleLightHeavy = XLit "light-heavy"
    emitXml BarStyleHeavyLight = XLit "heavy-light"
    emitXml BarStyleHeavyHeavy = XLit "heavy-heavy"
    emitXml BarStyleTick = XLit "tick"
    emitXml BarStyleShort = XLit "short"
    emitXml BarStyleNone = XLit "none"
parseBarStyle :: String -> P.XParse BarStyle
parseBarStyle s
        | s == "regular" = return $ BarStyleRegular
        | s == "dotted" = return $ BarStyleDotted
        | s == "dashed" = return $ BarStyleDashed
        | s == "heavy" = return $ BarStyleHeavy
        | s == "light-light" = return $ BarStyleLightLight
        | s == "light-heavy" = return $ BarStyleLightHeavy
        | s == "heavy-light" = return $ BarStyleHeavyLight
        | s == "heavy-heavy" = return $ BarStyleHeavyHeavy
        | s == "tick" = return $ BarStyleTick
        | s == "short" = return $ BarStyleShort
        | s == "none" = return $ BarStyleNone
        | otherwise = P.xfail $ "BarStyle: " ++ s

-- | @beam-level@ /(simple)/
--
-- The MusicXML format supports six levels of beaming, up to 256th notes. Unlike the number-level type, the beam-level type identifies concurrent beams in a beam group. It does not distinguish overlapping beams such as grace notes within regular notes, or beams used in different voices.
newtype BeamLevel = BeamLevel { beamLevel :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show BeamLevel where show (BeamLevel a) = show a
instance Read BeamLevel where readsPrec i = map (A.first BeamLevel) . readsPrec i
instance EmitXml BeamLevel where
    emitXml = emitXml . beamLevel
parseBeamLevel :: String -> P.XParse BeamLevel
parseBeamLevel = P.xread "BeamLevel"

-- | @beam-value@ /(simple)/
--
-- The beam-value type represents the type of beam associated with each of 6 beam levels (up to 256th notes) available for each note.
data BeamValue = 
      BeamValueBegin -- ^ /begin/
    | BeamValueContinue -- ^ /continue/
    | BeamValueEnd -- ^ /end/
    | BeamValueForwardHook -- ^ /forward hook/
    | BeamValueBackwardHook -- ^ /backward hook/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml BeamValue where
    emitXml BeamValueBegin = XLit "begin"
    emitXml BeamValueContinue = XLit "continue"
    emitXml BeamValueEnd = XLit "end"
    emitXml BeamValueForwardHook = XLit "forward hook"
    emitXml BeamValueBackwardHook = XLit "backward hook"
parseBeamValue :: String -> P.XParse BeamValue
parseBeamValue s
        | s == "begin" = return $ BeamValueBegin
        | s == "continue" = return $ BeamValueContinue
        | s == "end" = return $ BeamValueEnd
        | s == "forward hook" = return $ BeamValueForwardHook
        | s == "backward hook" = return $ BeamValueBackwardHook
        | otherwise = P.xfail $ "BeamValue: " ++ s

-- | @clef-sign@ /(simple)/
--
-- The clef-sign element represents the different clef symbols.
data ClefSign = 
      ClefSignG -- ^ /G/
    | ClefSignF -- ^ /F/
    | ClefSignC -- ^ /C/
    | ClefSignPercussion -- ^ /percussion/
    | ClefSignTAB -- ^ /TAB/
    | ClefSignNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ClefSign where
    emitXml ClefSignG = XLit "G"
    emitXml ClefSignF = XLit "F"
    emitXml ClefSignC = XLit "C"
    emitXml ClefSignPercussion = XLit "percussion"
    emitXml ClefSignTAB = XLit "TAB"
    emitXml ClefSignNone = XLit "none"
parseClefSign :: String -> P.XParse ClefSign
parseClefSign s
        | s == "G" = return $ ClefSignG
        | s == "F" = return $ ClefSignF
        | s == "C" = return $ ClefSignC
        | s == "percussion" = return $ ClefSignPercussion
        | s == "TAB" = return $ ClefSignTAB
        | s == "none" = return $ ClefSignNone
        | otherwise = P.xfail $ "ClefSign: " ++ s

-- | @color@ /(simple)/
--
-- The color type indicates the color of an element. Color may be represented as hexadecimal RGB triples, as in HTML, or as hexadecimal ARGB tuples, with the A indicating alpha of transparency. An alpha value of 00 is totally transparent; FF is totally opaque. If RGB is used, the A value is assumed to be FF. 
-- 
-- For instance, the RGB value "#800080" represents purple. An ARGB value of "#40800080" would be a transparent purple.
-- 
-- As in SVG 1.1, colors are defined in terms of the sRGB color space (IEC 61966).
newtype Color = Color { color :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Color where show (Color a) = show a
instance Read Color where readsPrec i = map (A.first Color) . readsPrec i
instance EmitXml Color where
    emitXml = emitXml . color
parseColor :: String -> P.XParse Color
parseColor = return . fromString

-- | @comma-separated-text@ /(simple)/
--
-- The comma-separated-text type is used to specify a comma-separated list of text elements, as is used by the font-family attribute.
newtype CommaSeparatedText = CommaSeparatedText { commaSeparatedText :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show CommaSeparatedText where show (CommaSeparatedText a) = show a
instance Read CommaSeparatedText where readsPrec i = map (A.first CommaSeparatedText) . readsPrec i
instance EmitXml CommaSeparatedText where
    emitXml = emitXml . commaSeparatedText
parseCommaSeparatedText :: String -> P.XParse CommaSeparatedText
parseCommaSeparatedText = return . fromString

-- | @css-font-size@ /(simple)/
--
-- The css-font-size type includes the CSS font sizes used as an alternative to a numeric point size.
data CssFontSize = 
      CssFontSizeXxSmall -- ^ /xx-small/
    | CssFontSizeXSmall -- ^ /x-small/
    | CssFontSizeSmall -- ^ /small/
    | CssFontSizeMedium -- ^ /medium/
    | CssFontSizeLarge -- ^ /large/
    | CssFontSizeXLarge -- ^ /x-large/
    | CssFontSizeXxLarge -- ^ /xx-large/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml CssFontSize where
    emitXml CssFontSizeXxSmall = XLit "xx-small"
    emitXml CssFontSizeXSmall = XLit "x-small"
    emitXml CssFontSizeSmall = XLit "small"
    emitXml CssFontSizeMedium = XLit "medium"
    emitXml CssFontSizeLarge = XLit "large"
    emitXml CssFontSizeXLarge = XLit "x-large"
    emitXml CssFontSizeXxLarge = XLit "xx-large"
parseCssFontSize :: String -> P.XParse CssFontSize
parseCssFontSize s
        | s == "xx-small" = return $ CssFontSizeXxSmall
        | s == "x-small" = return $ CssFontSizeXSmall
        | s == "small" = return $ CssFontSizeSmall
        | s == "medium" = return $ CssFontSizeMedium
        | s == "large" = return $ CssFontSizeLarge
        | s == "x-large" = return $ CssFontSizeXLarge
        | s == "xx-large" = return $ CssFontSizeXxLarge
        | otherwise = P.xfail $ "CssFontSize: " ++ s

-- | @degree-type-value@ /(simple)/
--
-- The degree-type-value type indicates whether the current degree element is an addition, alteration, or subtraction to the kind of the current chord in the harmony element.
data DegreeTypeValue = 
      DegreeTypeValueAdd -- ^ /add/
    | DegreeTypeValueAlter -- ^ /alter/
    | DegreeTypeValueSubtract -- ^ /subtract/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml DegreeTypeValue where
    emitXml DegreeTypeValueAdd = XLit "add"
    emitXml DegreeTypeValueAlter = XLit "alter"
    emitXml DegreeTypeValueSubtract = XLit "subtract"
parseDegreeTypeValue :: String -> P.XParse DegreeTypeValue
parseDegreeTypeValue s
        | s == "add" = return $ DegreeTypeValueAdd
        | s == "alter" = return $ DegreeTypeValueAlter
        | s == "subtract" = return $ DegreeTypeValueSubtract
        | otherwise = P.xfail $ "DegreeTypeValue: " ++ s

-- | @divisions@ /(simple)/
--
-- The divisions type is used to express values in terms of the musical divisions defined by the divisions element. It is preferred that these be integer values both for MIDI interoperability and to avoid roundoff errors.
newtype Divisions = Divisions { divisions :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Divisions where show (Divisions a) = show a
instance Read Divisions where readsPrec i = map (A.first Divisions) . readsPrec i
instance EmitXml Divisions where
    emitXml = emitXml . divisions
parseDivisions :: String -> P.XParse Divisions
parseDivisions = P.xread "Divisions"

-- | @enclosure@ /(simple)/
--
-- The enclosure type describes the shape and presence / absence of an enclosure around text.
data Enclosure = 
      EnclosureRectangle -- ^ /rectangle/
    | EnclosureOval -- ^ /oval/
    | EnclosureNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Enclosure where
    emitXml EnclosureRectangle = XLit "rectangle"
    emitXml EnclosureOval = XLit "oval"
    emitXml EnclosureNone = XLit "none"
parseEnclosure :: String -> P.XParse Enclosure
parseEnclosure s
        | s == "rectangle" = return $ EnclosureRectangle
        | s == "oval" = return $ EnclosureOval
        | s == "none" = return $ EnclosureNone
        | otherwise = P.xfail $ "Enclosure: " ++ s

-- | @ending-number@ /(simple)/
--
-- The ending-number type is used to specify either a comma-separated list of positive integers without leading zeros, or a string of zero or more spaces. It is used for the number attribute of the ending element. The zero or more spaces version is used when software knows that an ending is present, but cannot determine the type of the ending.
newtype EndingNumber = EndingNumber { endingNumber :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show EndingNumber where show (EndingNumber a) = show a
instance Read EndingNumber where readsPrec i = map (A.first EndingNumber) . readsPrec i
instance EmitXml EndingNumber where
    emitXml = emitXml . endingNumber
parseEndingNumber :: String -> P.XParse EndingNumber
parseEndingNumber = return . fromString

-- | @fan@ /(simple)/
--
-- The fan type represents the type of beam fanning present on a note, used to represent accelerandos and ritardandos.
data Fan = 
      FanAccel -- ^ /accel/
    | FanRit -- ^ /rit/
    | FanNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Fan where
    emitXml FanAccel = XLit "accel"
    emitXml FanRit = XLit "rit"
    emitXml FanNone = XLit "none"
parseFan :: String -> P.XParse Fan
parseFan s
        | s == "accel" = return $ FanAccel
        | s == "rit" = return $ FanRit
        | s == "none" = return $ FanNone
        | otherwise = P.xfail $ "Fan: " ++ s

-- | @fermata-shape@ /(simple)/
--
-- The fermata-shape type represents the shape of the fermata sign. The empty value is equivalent to the normal value.
data FermataShape = 
      FermataShapeNormal -- ^ /normal/
    | FermataShapeAngled -- ^ /angled/
    | FermataShapeSquare -- ^ /square/
    | FermataShape -- ^ //
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml FermataShape where
    emitXml FermataShapeNormal = XLit "normal"
    emitXml FermataShapeAngled = XLit "angled"
    emitXml FermataShapeSquare = XLit "square"
    emitXml FermataShape = XLit ""
parseFermataShape :: String -> P.XParse FermataShape
parseFermataShape s
        | s == "normal" = return $ FermataShapeNormal
        | s == "angled" = return $ FermataShapeAngled
        | s == "square" = return $ FermataShapeSquare
        | s == "" = return $ FermataShape
        | otherwise = P.xfail $ "FermataShape: " ++ s

-- | @fifths@ /(simple)/
--
-- The fifths type represents the number of flats or sharps in a traditional key signature. Negative numbers are used for flats and positive numbers for sharps, reflecting the key's placement within the circle of fifths (hence the type name).
newtype Fifths = Fifths { fifths :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Fifths where show (Fifths a) = show a
instance Read Fifths where readsPrec i = map (A.first Fifths) . readsPrec i
instance EmitXml Fifths where
    emitXml = emitXml . fifths
parseFifths :: String -> P.XParse Fifths
parseFifths = P.xread "Fifths"

-- | @font-size@ /(simple)/
--
-- The font-size can be one of the CSS font sizes or a numeric point size.
data FontSize = 
      FontSizeDecimal {
          fontSize1 :: Decimal
       }
    | FontSizeCssFontSize {
          fontSize2 :: CssFontSize
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FontSize where
    emitXml (FontSizeDecimal a) = emitXml a
    emitXml (FontSizeCssFontSize a) = emitXml a
parseFontSize :: String -> P.XParse FontSize
parseFontSize s = 
      FontSizeDecimal
        <$> (P.xread "Decimal") s
      <|> FontSizeCssFontSize
        <$> parseCssFontSize s


-- | @font-style@ /(simple)/
--
-- The font-style type represents a simplified version of the CSS font-style property.
data FontStyle = 
      FontStyleNormal -- ^ /normal/
    | FontStyleItalic -- ^ /italic/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml FontStyle where
    emitXml FontStyleNormal = XLit "normal"
    emitXml FontStyleItalic = XLit "italic"
parseFontStyle :: String -> P.XParse FontStyle
parseFontStyle s
        | s == "normal" = return $ FontStyleNormal
        | s == "italic" = return $ FontStyleItalic
        | otherwise = P.xfail $ "FontStyle: " ++ s

-- | @font-weight@ /(simple)/
--
-- The font-weight type represents a simplified version of the CSS font-weight property.
data FontWeight = 
      FontWeightNormal -- ^ /normal/
    | FontWeightBold -- ^ /bold/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml FontWeight where
    emitXml FontWeightNormal = XLit "normal"
    emitXml FontWeightBold = XLit "bold"
parseFontWeight :: String -> P.XParse FontWeight
parseFontWeight s
        | s == "normal" = return $ FontWeightNormal
        | s == "bold" = return $ FontWeightBold
        | otherwise = P.xfail $ "FontWeight: " ++ s

-- | @group-barline-value@ /(simple)/
--
-- The group-barline-value type indicates if the group should have common barlines.
data GroupBarlineValue = 
      GroupBarlineValueYes -- ^ /yes/
    | GroupBarlineValueNo -- ^ /no/
    | GroupBarlineValueMensurstrich -- ^ /Mensurstrich/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml GroupBarlineValue where
    emitXml GroupBarlineValueYes = XLit "yes"
    emitXml GroupBarlineValueNo = XLit "no"
    emitXml GroupBarlineValueMensurstrich = XLit "Mensurstrich"
parseGroupBarlineValue :: String -> P.XParse GroupBarlineValue
parseGroupBarlineValue s
        | s == "yes" = return $ GroupBarlineValueYes
        | s == "no" = return $ GroupBarlineValueNo
        | s == "Mensurstrich" = return $ GroupBarlineValueMensurstrich
        | otherwise = P.xfail $ "GroupBarlineValue: " ++ s

-- | @group-symbol-value@ /(simple)/
--
-- The group-symbol-value type indicates how the symbol for a group is indicated in the score. The default value is none.
data GroupSymbolValue = 
      GroupSymbolValueNone -- ^ /none/
    | GroupSymbolValueBrace -- ^ /brace/
    | GroupSymbolValueLine -- ^ /line/
    | GroupSymbolValueBracket -- ^ /bracket/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml GroupSymbolValue where
    emitXml GroupSymbolValueNone = XLit "none"
    emitXml GroupSymbolValueBrace = XLit "brace"
    emitXml GroupSymbolValueLine = XLit "line"
    emitXml GroupSymbolValueBracket = XLit "bracket"
parseGroupSymbolValue :: String -> P.XParse GroupSymbolValue
parseGroupSymbolValue s
        | s == "none" = return $ GroupSymbolValueNone
        | s == "brace" = return $ GroupSymbolValueBrace
        | s == "line" = return $ GroupSymbolValueLine
        | s == "bracket" = return $ GroupSymbolValueBracket
        | otherwise = P.xfail $ "GroupSymbolValue: " ++ s

-- | @harmony-type@ /(simple)/
--
-- The harmony-type type differentiates different types of harmonies when alternate harmonies are possible. Explicit harmonies have all note present in the music; implied have some notes missing but implied; alternate represents alternate analyses.
data HarmonyType = 
      HarmonyTypeExplicit -- ^ /explicit/
    | HarmonyTypeImplied -- ^ /implied/
    | HarmonyTypeAlternate -- ^ /alternate/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml HarmonyType where
    emitXml HarmonyTypeExplicit = XLit "explicit"
    emitXml HarmonyTypeImplied = XLit "implied"
    emitXml HarmonyTypeAlternate = XLit "alternate"
parseHarmonyType :: String -> P.XParse HarmonyType
parseHarmonyType s
        | s == "explicit" = return $ HarmonyTypeExplicit
        | s == "implied" = return $ HarmonyTypeImplied
        | s == "alternate" = return $ HarmonyTypeAlternate
        | otherwise = P.xfail $ "HarmonyType: " ++ s

-- | @kind-value@ /(simple)/
--
-- A kind-value indicates the type of chord. Degree elements can then add, subtract, or alter from these starting points. Values include:
--
-- @
-- 	
-- Triads:
-- 	major (major third, perfect fifth)
-- 	minor (minor third, perfect fifth)
-- 	augmented (major third, augmented fifth)
-- 	diminished (minor third, diminished fifth)
-- Sevenths:
-- 	dominant (major triad, minor seventh)
-- 	major-seventh (major triad, major seventh)
-- 	minor-seventh (minor triad, minor seventh)
-- 	diminished-seventh (diminished triad, diminished seventh)
-- 	augmented-seventh (augmented triad, minor seventh)
-- 	half-diminished (diminished triad, minor seventh)
-- 	major-minor (minor triad, major seventh)
-- Sixths:
-- 	major-sixth (major triad, added sixth)
-- 	minor-sixth (minor triad, added sixth)
-- Ninths:
-- 	dominant-ninth (dominant-seventh, major ninth)
-- 	major-ninth (major-seventh, major ninth)
-- 	minor-ninth (minor-seventh, major ninth)
-- 11ths (usually as the basis for alteration):
-- 	dominant-11th (dominant-ninth, perfect 11th)
-- 	major-11th (major-ninth, perfect 11th)
-- 	minor-11th (minor-ninth, perfect 11th)
-- 13ths (usually as the basis for alteration):
-- 	dominant-13th (dominant-11th, major 13th)
-- 	major-13th (major-11th, major 13th)
-- 	minor-13th (minor-11th, major 13th)
-- Suspended:
-- 	suspended-second (major second, perfect fifth)
-- 	suspended-fourth (perfect fourth, perfect fifth)
-- Functional sixths:
-- 	Neapolitan
-- 	Italian
-- 	French
-- 	German
-- Other:
-- 	pedal (pedal-point bass)
-- 	power (perfect fifth)
-- 	Tristan
-- 	
-- The "other" kind is used when the harmony is entirely composed of add elements. The "none" kind is used to explicitly encode absence of chords or functional harmony.
-- @
data KindValue = 
      KindValueMajor -- ^ /major/
    | KindValueMinor -- ^ /minor/
    | KindValueAugmented -- ^ /augmented/
    | KindValueDiminished -- ^ /diminished/
    | KindValueDominant -- ^ /dominant/
    | KindValueMajorSeventh -- ^ /major-seventh/
    | KindValueMinorSeventh -- ^ /minor-seventh/
    | KindValueDiminishedSeventh -- ^ /diminished-seventh/
    | KindValueAugmentedSeventh -- ^ /augmented-seventh/
    | KindValueHalfDiminished -- ^ /half-diminished/
    | KindValueMajorMinor -- ^ /major-minor/
    | KindValueMajorSixth -- ^ /major-sixth/
    | KindValueMinorSixth -- ^ /minor-sixth/
    | KindValueDominantNinth -- ^ /dominant-ninth/
    | KindValueMajorNinth -- ^ /major-ninth/
    | KindValueMinorNinth -- ^ /minor-ninth/
    | KindValueDominant11th -- ^ /dominant-11th/
    | KindValueMajor11th -- ^ /major-11th/
    | KindValueMinor11th -- ^ /minor-11th/
    | KindValueDominant13th -- ^ /dominant-13th/
    | KindValueMajor13th -- ^ /major-13th/
    | KindValueMinor13th -- ^ /minor-13th/
    | KindValueSuspendedSecond -- ^ /suspended-second/
    | KindValueSuspendedFourth -- ^ /suspended-fourth/
    | KindValueNeapolitan -- ^ /Neapolitan/
    | KindValueItalian -- ^ /Italian/
    | KindValueFrench -- ^ /French/
    | KindValueGerman -- ^ /German/
    | KindValuePedal -- ^ /pedal/
    | KindValuePower -- ^ /power/
    | KindValueTristan -- ^ /Tristan/
    | KindValueOther -- ^ /other/
    | KindValueNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml KindValue where
    emitXml KindValueMajor = XLit "major"
    emitXml KindValueMinor = XLit "minor"
    emitXml KindValueAugmented = XLit "augmented"
    emitXml KindValueDiminished = XLit "diminished"
    emitXml KindValueDominant = XLit "dominant"
    emitXml KindValueMajorSeventh = XLit "major-seventh"
    emitXml KindValueMinorSeventh = XLit "minor-seventh"
    emitXml KindValueDiminishedSeventh = XLit "diminished-seventh"
    emitXml KindValueAugmentedSeventh = XLit "augmented-seventh"
    emitXml KindValueHalfDiminished = XLit "half-diminished"
    emitXml KindValueMajorMinor = XLit "major-minor"
    emitXml KindValueMajorSixth = XLit "major-sixth"
    emitXml KindValueMinorSixth = XLit "minor-sixth"
    emitXml KindValueDominantNinth = XLit "dominant-ninth"
    emitXml KindValueMajorNinth = XLit "major-ninth"
    emitXml KindValueMinorNinth = XLit "minor-ninth"
    emitXml KindValueDominant11th = XLit "dominant-11th"
    emitXml KindValueMajor11th = XLit "major-11th"
    emitXml KindValueMinor11th = XLit "minor-11th"
    emitXml KindValueDominant13th = XLit "dominant-13th"
    emitXml KindValueMajor13th = XLit "major-13th"
    emitXml KindValueMinor13th = XLit "minor-13th"
    emitXml KindValueSuspendedSecond = XLit "suspended-second"
    emitXml KindValueSuspendedFourth = XLit "suspended-fourth"
    emitXml KindValueNeapolitan = XLit "Neapolitan"
    emitXml KindValueItalian = XLit "Italian"
    emitXml KindValueFrench = XLit "French"
    emitXml KindValueGerman = XLit "German"
    emitXml KindValuePedal = XLit "pedal"
    emitXml KindValuePower = XLit "power"
    emitXml KindValueTristan = XLit "Tristan"
    emitXml KindValueOther = XLit "other"
    emitXml KindValueNone = XLit "none"
parseKindValue :: String -> P.XParse KindValue
parseKindValue s
        | s == "major" = return $ KindValueMajor
        | s == "minor" = return $ KindValueMinor
        | s == "augmented" = return $ KindValueAugmented
        | s == "diminished" = return $ KindValueDiminished
        | s == "dominant" = return $ KindValueDominant
        | s == "major-seventh" = return $ KindValueMajorSeventh
        | s == "minor-seventh" = return $ KindValueMinorSeventh
        | s == "diminished-seventh" = return $ KindValueDiminishedSeventh
        | s == "augmented-seventh" = return $ KindValueAugmentedSeventh
        | s == "half-diminished" = return $ KindValueHalfDiminished
        | s == "major-minor" = return $ KindValueMajorMinor
        | s == "major-sixth" = return $ KindValueMajorSixth
        | s == "minor-sixth" = return $ KindValueMinorSixth
        | s == "dominant-ninth" = return $ KindValueDominantNinth
        | s == "major-ninth" = return $ KindValueMajorNinth
        | s == "minor-ninth" = return $ KindValueMinorNinth
        | s == "dominant-11th" = return $ KindValueDominant11th
        | s == "major-11th" = return $ KindValueMajor11th
        | s == "minor-11th" = return $ KindValueMinor11th
        | s == "dominant-13th" = return $ KindValueDominant13th
        | s == "major-13th" = return $ KindValueMajor13th
        | s == "minor-13th" = return $ KindValueMinor13th
        | s == "suspended-second" = return $ KindValueSuspendedSecond
        | s == "suspended-fourth" = return $ KindValueSuspendedFourth
        | s == "Neapolitan" = return $ KindValueNeapolitan
        | s == "Italian" = return $ KindValueItalian
        | s == "French" = return $ KindValueFrench
        | s == "German" = return $ KindValueGerman
        | s == "pedal" = return $ KindValuePedal
        | s == "power" = return $ KindValuePower
        | s == "Tristan" = return $ KindValueTristan
        | s == "other" = return $ KindValueOther
        | s == "none" = return $ KindValueNone
        | otherwise = P.xfail $ "KindValue: " ++ s

-- | @xml:lang@ /(simple)/
data Lang = 
      LangLanguage {
          lang1 :: Language
       }
    | LangLang {
          lang2 :: SumLang
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Lang where
    emitXml (LangLanguage a) = emitXml a
    emitXml (LangLang a) = emitXml a
parseLang :: String -> P.XParse Lang
parseLang s = 
      LangLanguage
        <$> parseLanguage s
      <|> LangLang
        <$> parseSumLang s


-- | @xs:language@ /(simple)/
newtype Language = Language { language :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Language where show (Language a) = show a
instance Read Language where readsPrec i = map (A.first Language) . readsPrec i
instance EmitXml Language where
    emitXml = emitXml . language
parseLanguage :: String -> P.XParse Language
parseLanguage = return . fromString

-- | @left-center-right@ /(simple)/
--
-- The left-center-right type is used to define horizontal alignment and text justification.
data LeftCenterRight = 
      LeftCenterRightLeft -- ^ /left/
    | LeftCenterRightCenter -- ^ /center/
    | LeftCenterRightRight -- ^ /right/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LeftCenterRight where
    emitXml LeftCenterRightLeft = XLit "left"
    emitXml LeftCenterRightCenter = XLit "center"
    emitXml LeftCenterRightRight = XLit "right"
parseLeftCenterRight :: String -> P.XParse LeftCenterRight
parseLeftCenterRight s
        | s == "left" = return $ LeftCenterRightLeft
        | s == "center" = return $ LeftCenterRightCenter
        | s == "right" = return $ LeftCenterRightRight
        | otherwise = P.xfail $ "LeftCenterRight: " ++ s

-- | @left-right@ /(simple)/
--
-- The left-right type is used to indicate whether one element appears to the left or the right of another element.
data LeftRight = 
      LeftRightLeft -- ^ /left/
    | LeftRightRight -- ^ /right/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LeftRight where
    emitXml LeftRightLeft = XLit "left"
    emitXml LeftRightRight = XLit "right"
parseLeftRight :: String -> P.XParse LeftRight
parseLeftRight s
        | s == "left" = return $ LeftRightLeft
        | s == "right" = return $ LeftRightRight
        | otherwise = P.xfail $ "LeftRight: " ++ s

-- | @line-end@ /(simple)/
--
-- The line-end type specifies if there is a jog up or down (or both), an arrow, or nothing at the start or end of a bracket.
data LineEnd = 
      LineEndUp -- ^ /up/
    | LineEndDown -- ^ /down/
    | LineEndBoth -- ^ /both/
    | LineEndArrow -- ^ /arrow/
    | LineEndNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LineEnd where
    emitXml LineEndUp = XLit "up"
    emitXml LineEndDown = XLit "down"
    emitXml LineEndBoth = XLit "both"
    emitXml LineEndArrow = XLit "arrow"
    emitXml LineEndNone = XLit "none"
parseLineEnd :: String -> P.XParse LineEnd
parseLineEnd s
        | s == "up" = return $ LineEndUp
        | s == "down" = return $ LineEndDown
        | s == "both" = return $ LineEndBoth
        | s == "arrow" = return $ LineEndArrow
        | s == "none" = return $ LineEndNone
        | otherwise = P.xfail $ "LineEnd: " ++ s

-- | @line-shape@ /(simple)/
--
-- The line-shape type distinguishes between straight and curved lines.
data LineShape = 
      LineShapeStraight -- ^ /straight/
    | LineShapeCurved -- ^ /curved/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LineShape where
    emitXml LineShapeStraight = XLit "straight"
    emitXml LineShapeCurved = XLit "curved"
parseLineShape :: String -> P.XParse LineShape
parseLineShape s
        | s == "straight" = return $ LineShapeStraight
        | s == "curved" = return $ LineShapeCurved
        | otherwise = P.xfail $ "LineShape: " ++ s

-- | @line-type@ /(simple)/
--
-- The line-type type distinguishes between solid, dashed, dotted, and wavy lines.
data LineType = 
      LineTypeSolid -- ^ /solid/
    | LineTypeDashed -- ^ /dashed/
    | LineTypeDotted -- ^ /dotted/
    | LineTypeWavy -- ^ /wavy/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LineType where
    emitXml LineTypeSolid = XLit "solid"
    emitXml LineTypeDashed = XLit "dashed"
    emitXml LineTypeDotted = XLit "dotted"
    emitXml LineTypeWavy = XLit "wavy"
parseLineType :: String -> P.XParse LineType
parseLineType s
        | s == "solid" = return $ LineTypeSolid
        | s == "dashed" = return $ LineTypeDashed
        | s == "dotted" = return $ LineTypeDotted
        | s == "wavy" = return $ LineTypeWavy
        | otherwise = P.xfail $ "LineType: " ++ s

-- | @line-width-type@ /(simple)/
--
-- The line-width-type defines what type of line is being defined in a line-width element. Values include beam, bracket, dashes, enclosure, ending, extend, heavy barline, leger, light barline, octave shift, pedal, slur middle, slur tip, staff, stem, tie middle, tie tip, tuplet bracket, and wedge. This is left as a string so that other application-specific types can be defined, but it is made a separate type so that it can be redefined more strictly.
newtype LineWidthType = LineWidthType { lineWidthType :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show LineWidthType where show (LineWidthType a) = show a
instance Read LineWidthType where readsPrec i = map (A.first LineWidthType) . readsPrec i
instance EmitXml LineWidthType where
    emitXml = emitXml . lineWidthType
parseLineWidthType :: String -> P.XParse LineWidthType
parseLineWidthType = return . fromString

-- | @margin-type@ /(simple)/
--
-- The margin-type type specifies whether margins apply to even page, odd pages, or both.
data MarginType = 
      MarginTypeOdd -- ^ /odd/
    | MarginTypeEven -- ^ /even/
    | MarginTypeBoth -- ^ /both/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml MarginType where
    emitXml MarginTypeOdd = XLit "odd"
    emitXml MarginTypeEven = XLit "even"
    emitXml MarginTypeBoth = XLit "both"
parseMarginType :: String -> P.XParse MarginType
parseMarginType s
        | s == "odd" = return $ MarginTypeOdd
        | s == "even" = return $ MarginTypeEven
        | s == "both" = return $ MarginTypeBoth
        | otherwise = P.xfail $ "MarginType: " ++ s

-- | @measure-numbering-value@ /(simple)/
--
-- The measure-numbering-value type describes how measure numbers are displayed on this part: no numbers, numbers every measure, or numbers every system.
data MeasureNumberingValue = 
      MeasureNumberingValueNone -- ^ /none/
    | MeasureNumberingValueMeasure -- ^ /measure/
    | MeasureNumberingValueSystem -- ^ /system/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml MeasureNumberingValue where
    emitXml MeasureNumberingValueNone = XLit "none"
    emitXml MeasureNumberingValueMeasure = XLit "measure"
    emitXml MeasureNumberingValueSystem = XLit "system"
parseMeasureNumberingValue :: String -> P.XParse MeasureNumberingValue
parseMeasureNumberingValue s
        | s == "none" = return $ MeasureNumberingValueNone
        | s == "measure" = return $ MeasureNumberingValueMeasure
        | s == "system" = return $ MeasureNumberingValueSystem
        | otherwise = P.xfail $ "MeasureNumberingValue: " ++ s

-- | @midi-128@ /(simple)/
--
-- The midi-16 type is used to express MIDI 1.0 values that range from 1 to 128.
newtype Midi128 = Midi128 { midi128 :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Midi128 where show (Midi128 a) = show a
instance Read Midi128 where readsPrec i = map (A.first Midi128) . readsPrec i
instance EmitXml Midi128 where
    emitXml = emitXml . midi128
parseMidi128 :: String -> P.XParse Midi128
parseMidi128 = P.xread "Midi128"

-- | @midi-16@ /(simple)/
--
-- The midi-16 type is used to express MIDI 1.0 values that range from 1 to 16.
newtype Midi16 = Midi16 { midi16 :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Midi16 where show (Midi16 a) = show a
instance Read Midi16 where readsPrec i = map (A.first Midi16) . readsPrec i
instance EmitXml Midi16 where
    emitXml = emitXml . midi16
parseMidi16 :: String -> P.XParse Midi16
parseMidi16 = P.xread "Midi16"

-- | @midi-16384@ /(simple)/
--
-- The midi-16 type is used to express MIDI 1.0 values that range from 1 to 16,384.
newtype Midi16384 = Midi16384 { midi16384 :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Midi16384 where show (Midi16384 a) = show a
instance Read Midi16384 where readsPrec i = map (A.first Midi16384) . readsPrec i
instance EmitXml Midi16384 where
    emitXml = emitXml . midi16384
parseMidi16384 :: String -> P.XParse Midi16384
parseMidi16384 = P.xread "Midi16384"

-- | @millimeters@ /(simple)/
--
-- The millimeters type is a number representing millimeters. This is used in the scaling element to provide a default scaling from tenths to physical units.
newtype Millimeters = Millimeters { millimeters :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Millimeters where show (Millimeters a) = show a
instance Read Millimeters where readsPrec i = map (A.first Millimeters) . readsPrec i
instance EmitXml Millimeters where
    emitXml = emitXml . millimeters
parseMillimeters :: String -> P.XParse Millimeters
parseMillimeters = P.xread "Millimeters"

-- | @mode@ /(simple)/
--
-- The mode type is used to specify major/minor and other mode distinctions. Valid mode values include major, minor, dorian, phrygian, lydian, mixolydian, aeolian, ionian, and locrian.
newtype Mode = Mode { mode :: String }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Mode where show (Mode a) = show a
instance Read Mode where readsPrec i = map (A.first Mode) . readsPrec i
instance EmitXml Mode where
    emitXml = emitXml . mode
parseMode :: String -> P.XParse Mode
parseMode = return . fromString

-- | @non-negative-decimal@ /(simple)/
--
-- The non-negative-decimal type specifies a non-negative decimal value.
newtype NonNegativeDecimal = NonNegativeDecimal { nonNegativeDecimal :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show NonNegativeDecimal where show (NonNegativeDecimal a) = show a
instance Read NonNegativeDecimal where readsPrec i = map (A.first NonNegativeDecimal) . readsPrec i
instance EmitXml NonNegativeDecimal where
    emitXml = emitXml . nonNegativeDecimal
parseNonNegativeDecimal :: String -> P.XParse NonNegativeDecimal
parseNonNegativeDecimal = P.xread "NonNegativeDecimal"

-- | @xs:nonNegativeInteger@ /(simple)/
newtype NonNegativeInteger = NonNegativeInteger { nonNegativeInteger :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show NonNegativeInteger where show (NonNegativeInteger a) = show a
instance Read NonNegativeInteger where readsPrec i = map (A.first NonNegativeInteger) . readsPrec i
instance EmitXml NonNegativeInteger where
    emitXml = emitXml . nonNegativeInteger
parseNonNegativeInteger :: String -> P.XParse NonNegativeInteger
parseNonNegativeInteger = P.xread "NonNegativeInteger"

-- | @xs:normalizedString@ /(simple)/
newtype NormalizedString = NormalizedString { normalizedString :: String }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show NormalizedString where show (NormalizedString a) = show a
instance Read NormalizedString where readsPrec i = map (A.first NormalizedString) . readsPrec i
instance EmitXml NormalizedString where
    emitXml = emitXml . normalizedString
parseNormalizedString :: String -> P.XParse NormalizedString
parseNormalizedString = return . fromString

-- | @note-size-type@ /(simple)/
--
-- The note-size-type type indicates the type of note being defined by a note-size element. The grace type is used for notes of cue size that that include a grace element. The cue type is used for all other notes with cue size, whether defined explicitly or implicitly via a cue element. The large type is used for notes of large size.
data NoteSizeType = 
      NoteSizeTypeCue -- ^ /cue/
    | NoteSizeTypeGrace -- ^ /grace/
    | NoteSizeTypeLarge -- ^ /large/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml NoteSizeType where
    emitXml NoteSizeTypeCue = XLit "cue"
    emitXml NoteSizeTypeGrace = XLit "grace"
    emitXml NoteSizeTypeLarge = XLit "large"
parseNoteSizeType :: String -> P.XParse NoteSizeType
parseNoteSizeType s
        | s == "cue" = return $ NoteSizeTypeCue
        | s == "grace" = return $ NoteSizeTypeGrace
        | s == "large" = return $ NoteSizeTypeLarge
        | otherwise = P.xfail $ "NoteSizeType: " ++ s

-- | @note-type-value@ /(simple)/
--
-- The note-type type is used for the MusicXML type element and represents the graphic note type, from 256th (shortest) to long (longest).
data NoteTypeValue = 
      NoteTypeValue256th -- ^ /256th/
    | NoteTypeValue128th -- ^ /128th/
    | NoteTypeValue64th -- ^ /64th/
    | NoteTypeValue32nd -- ^ /32nd/
    | NoteTypeValue16th -- ^ /16th/
    | NoteTypeValueEighth -- ^ /eighth/
    | NoteTypeValueQuarter -- ^ /quarter/
    | NoteTypeValueHalf -- ^ /half/
    | NoteTypeValueWhole -- ^ /whole/
    | NoteTypeValueBreve -- ^ /breve/
    | NoteTypeValueLong -- ^ /long/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml NoteTypeValue where
    emitXml NoteTypeValue256th = XLit "256th"
    emitXml NoteTypeValue128th = XLit "128th"
    emitXml NoteTypeValue64th = XLit "64th"
    emitXml NoteTypeValue32nd = XLit "32nd"
    emitXml NoteTypeValue16th = XLit "16th"
    emitXml NoteTypeValueEighth = XLit "eighth"
    emitXml NoteTypeValueQuarter = XLit "quarter"
    emitXml NoteTypeValueHalf = XLit "half"
    emitXml NoteTypeValueWhole = XLit "whole"
    emitXml NoteTypeValueBreve = XLit "breve"
    emitXml NoteTypeValueLong = XLit "long"
parseNoteTypeValue :: String -> P.XParse NoteTypeValue
parseNoteTypeValue s
        | s == "256th" = return $ NoteTypeValue256th
        | s == "128th" = return $ NoteTypeValue128th
        | s == "64th" = return $ NoteTypeValue64th
        | s == "32nd" = return $ NoteTypeValue32nd
        | s == "16th" = return $ NoteTypeValue16th
        | s == "eighth" = return $ NoteTypeValueEighth
        | s == "quarter" = return $ NoteTypeValueQuarter
        | s == "half" = return $ NoteTypeValueHalf
        | s == "whole" = return $ NoteTypeValueWhole
        | s == "breve" = return $ NoteTypeValueBreve
        | s == "long" = return $ NoteTypeValueLong
        | otherwise = P.xfail $ "NoteTypeValue: " ++ s

-- | @notehead-value@ /(simple)/
--
-- 
-- The notehead type indicates shapes other than the open and closed ovals associated with note durations. The values do, re, mi, fa, so, la, and ti correspond to Aikin's 7-shape system.
-- 
-- The arrow shapes differ from triangle and inverted triangle by being centered on the stem. Slashed and back slashed notes include both the normal notehead and a slash. The triangle shape has the tip of the triangle pointing up; the inverted triangle shape has the tip of the triangle pointing down.
data NoteheadValue = 
      NoteheadValueSlash -- ^ /slash/
    | NoteheadValueTriangle -- ^ /triangle/
    | NoteheadValueDiamond -- ^ /diamond/
    | NoteheadValueSquare -- ^ /square/
    | NoteheadValueCross -- ^ /cross/
    | NoteheadValueX -- ^ /x/
    | NoteheadValueCircleX -- ^ /circle-x/
    | NoteheadValueInvertedTriangle -- ^ /inverted triangle/
    | NoteheadValueArrowDown -- ^ /arrow down/
    | NoteheadValueArrowUp -- ^ /arrow up/
    | NoteheadValueSlashed -- ^ /slashed/
    | NoteheadValueBackSlashed -- ^ /back slashed/
    | NoteheadValueNormal -- ^ /normal/
    | NoteheadValueCluster -- ^ /cluster/
    | NoteheadValueNone -- ^ /none/
    | NoteheadValueDo -- ^ /do/
    | NoteheadValueRe -- ^ /re/
    | NoteheadValueMi -- ^ /mi/
    | NoteheadValueFa -- ^ /fa/
    | NoteheadValueSo -- ^ /so/
    | NoteheadValueLa -- ^ /la/
    | NoteheadValueTi -- ^ /ti/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml NoteheadValue where
    emitXml NoteheadValueSlash = XLit "slash"
    emitXml NoteheadValueTriangle = XLit "triangle"
    emitXml NoteheadValueDiamond = XLit "diamond"
    emitXml NoteheadValueSquare = XLit "square"
    emitXml NoteheadValueCross = XLit "cross"
    emitXml NoteheadValueX = XLit "x"
    emitXml NoteheadValueCircleX = XLit "circle-x"
    emitXml NoteheadValueInvertedTriangle = XLit "inverted triangle"
    emitXml NoteheadValueArrowDown = XLit "arrow down"
    emitXml NoteheadValueArrowUp = XLit "arrow up"
    emitXml NoteheadValueSlashed = XLit "slashed"
    emitXml NoteheadValueBackSlashed = XLit "back slashed"
    emitXml NoteheadValueNormal = XLit "normal"
    emitXml NoteheadValueCluster = XLit "cluster"
    emitXml NoteheadValueNone = XLit "none"
    emitXml NoteheadValueDo = XLit "do"
    emitXml NoteheadValueRe = XLit "re"
    emitXml NoteheadValueMi = XLit "mi"
    emitXml NoteheadValueFa = XLit "fa"
    emitXml NoteheadValueSo = XLit "so"
    emitXml NoteheadValueLa = XLit "la"
    emitXml NoteheadValueTi = XLit "ti"
parseNoteheadValue :: String -> P.XParse NoteheadValue
parseNoteheadValue s
        | s == "slash" = return $ NoteheadValueSlash
        | s == "triangle" = return $ NoteheadValueTriangle
        | s == "diamond" = return $ NoteheadValueDiamond
        | s == "square" = return $ NoteheadValueSquare
        | s == "cross" = return $ NoteheadValueCross
        | s == "x" = return $ NoteheadValueX
        | s == "circle-x" = return $ NoteheadValueCircleX
        | s == "inverted triangle" = return $ NoteheadValueInvertedTriangle
        | s == "arrow down" = return $ NoteheadValueArrowDown
        | s == "arrow up" = return $ NoteheadValueArrowUp
        | s == "slashed" = return $ NoteheadValueSlashed
        | s == "back slashed" = return $ NoteheadValueBackSlashed
        | s == "normal" = return $ NoteheadValueNormal
        | s == "cluster" = return $ NoteheadValueCluster
        | s == "none" = return $ NoteheadValueNone
        | s == "do" = return $ NoteheadValueDo
        | s == "re" = return $ NoteheadValueRe
        | s == "mi" = return $ NoteheadValueMi
        | s == "fa" = return $ NoteheadValueFa
        | s == "so" = return $ NoteheadValueSo
        | s == "la" = return $ NoteheadValueLa
        | s == "ti" = return $ NoteheadValueTi
        | otherwise = P.xfail $ "NoteheadValue: " ++ s

-- | @number-level@ /(simple)/
--
-- Slurs, tuplets, and many other features can be concurrent and overlapping within a single musical part. The number-level type distinguishes up to six concurrent objects of the same type. A reading program should be prepared to handle cases where the number-levels stop in an arbitrary order. Different numbers are needed when the features overlap in MusicXML file order. When a number-level value is implied, the value is 1 by default.
newtype NumberLevel = NumberLevel { numberLevel :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show NumberLevel where show (NumberLevel a) = show a
instance Read NumberLevel where readsPrec i = map (A.first NumberLevel) . readsPrec i
instance EmitXml NumberLevel where
    emitXml = emitXml . numberLevel
parseNumberLevel :: String -> P.XParse NumberLevel
parseNumberLevel = P.xread "NumberLevel"

-- | @number-of-lines@ /(simple)/
--
-- The number-of-lines type is used to specify the number of lines in text decoration attributes.
newtype NumberOfLines = NumberOfLines { numberOfLines :: NonNegativeInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show NumberOfLines where show (NumberOfLines a) = show a
instance Read NumberOfLines where readsPrec i = map (A.first NumberOfLines) . readsPrec i
instance EmitXml NumberOfLines where
    emitXml = emitXml . numberOfLines
parseNumberOfLines :: String -> P.XParse NumberOfLines
parseNumberOfLines = P.xread "NumberOfLines"

-- | @number-or-normal@ /(simple)/
--
-- The number-or-normal values can be either a decimal number or the string "normal". This is used by the line-height and letter-spacing attributes.
data NumberOrNormal = 
      NumberOrNormalDecimal {
          numberOrNormal1 :: Decimal
       }
    | NumberOrNormalNumberOrNormal {
          numberOrNormal2 :: SumNumberOrNormal
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NumberOrNormal where
    emitXml (NumberOrNormalDecimal a) = emitXml a
    emitXml (NumberOrNormalNumberOrNormal a) = emitXml a
parseNumberOrNormal :: String -> P.XParse NumberOrNormal
parseNumberOrNormal s = 
      NumberOrNormalDecimal
        <$> (P.xread "Decimal") s
      <|> NumberOrNormalNumberOrNormal
        <$> parseSumNumberOrNormal s


-- | @octave@ /(simple)/
--
-- Octaves are represented by the numbers 0 to 9, where 4 indicates the octave started by middle C.
newtype Octave = Octave { octave :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Octave where show (Octave a) = show a
instance Read Octave where readsPrec i = map (A.first Octave) . readsPrec i
instance EmitXml Octave where
    emitXml = emitXml . octave
parseOctave :: String -> P.XParse Octave
parseOctave = P.xread "Octave"

-- | @over-under@ /(simple)/
--
-- The over-under type is used to indicate whether the tips of curved lines such as slurs and ties are overhand (tips down) or underhand (tips up).
data OverUnder = 
      OverUnderOver -- ^ /over/
    | OverUnderUnder -- ^ /under/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml OverUnder where
    emitXml OverUnderOver = XLit "over"
    emitXml OverUnderUnder = XLit "under"
parseOverUnder :: String -> P.XParse OverUnder
parseOverUnder s
        | s == "over" = return $ OverUnderOver
        | s == "under" = return $ OverUnderUnder
        | otherwise = P.xfail $ "OverUnder: " ++ s

-- | @percent@ /(simple)/
--
-- The percent type specifies a percentage from 0 to 100.
newtype Percent = Percent { percent :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Percent where show (Percent a) = show a
instance Read Percent where readsPrec i = map (A.first Percent) . readsPrec i
instance EmitXml Percent where
    emitXml = emitXml . percent
parsePercent :: String -> P.XParse Percent
parsePercent = P.xread "Percent"

-- | @positive-divisions@ /(simple)/
--
-- The positive-divisions type restricts divisions values to positive numbers.
newtype PositiveDivisions = PositiveDivisions { positiveDivisions :: Divisions }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show PositiveDivisions where show (PositiveDivisions a) = show a
instance Read PositiveDivisions where readsPrec i = map (A.first PositiveDivisions) . readsPrec i
instance EmitXml PositiveDivisions where
    emitXml = emitXml . positiveDivisions
parsePositiveDivisions :: String -> P.XParse PositiveDivisions
parsePositiveDivisions = P.xread "PositiveDivisions"

-- | @positive-integer-or-empty@ /(simple)/
--
-- The positive-integer-or-empty values can be either a positive integer or an empty string.
data PositiveIntegerOrEmpty = 
      PositiveIntegerOrEmptyPositiveInteger {
          positiveIntegerOrEmpty1 :: PositiveInteger
       }
    | PositiveIntegerOrEmptyPositiveIntegerOrEmpty {
          positiveIntegerOrEmpty2 :: SumPositiveIntegerOrEmpty
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PositiveIntegerOrEmpty where
    emitXml (PositiveIntegerOrEmptyPositiveInteger a) = emitXml a
    emitXml (PositiveIntegerOrEmptyPositiveIntegerOrEmpty a) = emitXml a
parsePositiveIntegerOrEmpty :: String -> P.XParse PositiveIntegerOrEmpty
parsePositiveIntegerOrEmpty s = 
      PositiveIntegerOrEmptyPositiveInteger
        <$> parsePositiveInteger s
      <|> PositiveIntegerOrEmptyPositiveIntegerOrEmpty
        <$> parseSumPositiveIntegerOrEmpty s


-- | @xs:positiveInteger@ /(simple)/
newtype PositiveInteger = PositiveInteger { positiveInteger :: NonNegativeInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show PositiveInteger where show (PositiveInteger a) = show a
instance Read PositiveInteger where readsPrec i = map (A.first PositiveInteger) . readsPrec i
instance EmitXml PositiveInteger where
    emitXml = emitXml . positiveInteger
parsePositiveInteger :: String -> P.XParse PositiveInteger
parsePositiveInteger = P.xread "PositiveInteger"

-- | @rehearsal-enclosure@ /(simple)/
--
-- The rehearsal-enclosure type describes the shape and presence / absence of an enclosure around rehearsal text.
data RehearsalEnclosure = 
      RehearsalEnclosureSquare -- ^ /square/
    | RehearsalEnclosureCircle -- ^ /circle/
    | RehearsalEnclosureNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml RehearsalEnclosure where
    emitXml RehearsalEnclosureSquare = XLit "square"
    emitXml RehearsalEnclosureCircle = XLit "circle"
    emitXml RehearsalEnclosureNone = XLit "none"
parseRehearsalEnclosure :: String -> P.XParse RehearsalEnclosure
parseRehearsalEnclosure s
        | s == "square" = return $ RehearsalEnclosureSquare
        | s == "circle" = return $ RehearsalEnclosureCircle
        | s == "none" = return $ RehearsalEnclosureNone
        | otherwise = P.xfail $ "RehearsalEnclosure: " ++ s

-- | @right-left-middle@ /(simple)/
--
-- The right-left-middle type is used to specify barline location.
data RightLeftMiddle = 
      RightLeftMiddleRight -- ^ /right/
    | RightLeftMiddleLeft -- ^ /left/
    | RightLeftMiddleMiddle -- ^ /middle/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml RightLeftMiddle where
    emitXml RightLeftMiddleRight = XLit "right"
    emitXml RightLeftMiddleLeft = XLit "left"
    emitXml RightLeftMiddleMiddle = XLit "middle"
parseRightLeftMiddle :: String -> P.XParse RightLeftMiddle
parseRightLeftMiddle s
        | s == "right" = return $ RightLeftMiddleRight
        | s == "left" = return $ RightLeftMiddleLeft
        | s == "middle" = return $ RightLeftMiddleMiddle
        | otherwise = P.xfail $ "RightLeftMiddle: " ++ s

-- | @rotation-degrees@ /(simple)/
--
-- The rotation-degrees type specifies rotation, pan, and elevation values in degrees. Values range from -180 to 180.
newtype RotationDegrees = RotationDegrees { rotationDegrees :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show RotationDegrees where show (RotationDegrees a) = show a
instance Read RotationDegrees where readsPrec i = map (A.first RotationDegrees) . readsPrec i
instance EmitXml RotationDegrees where
    emitXml = emitXml . rotationDegrees
parseRotationDegrees :: String -> P.XParse RotationDegrees
parseRotationDegrees = P.xread "RotationDegrees"

-- | @semitones@ /(simple)/
--
-- The semintones type is a number representing semitones, used for chromatic alteration. A value of -1 corresponds to a flat and a value of 1 to a sharp. Decimal values like 0.5 (quarter tone sharp) may be used for microtones.
newtype Semitones = Semitones { semitones :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Semitones where show (Semitones a) = show a
instance Read Semitones where readsPrec i = map (A.first Semitones) . readsPrec i
instance EmitXml Semitones where
    emitXml = emitXml . semitones
parseSemitones :: String -> P.XParse Semitones
parseSemitones = P.xread "Semitones"

-- | @xlink:show@ /(simple)/
data SmpShow = 
      ShowNew -- ^ /new/
    | ShowReplace -- ^ /replace/
    | ShowEmbed -- ^ /embed/
    | ShowOther -- ^ /other/
    | ShowNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SmpShow where
    emitXml ShowNew = XLit "new"
    emitXml ShowReplace = XLit "replace"
    emitXml ShowEmbed = XLit "embed"
    emitXml ShowOther = XLit "other"
    emitXml ShowNone = XLit "none"
parseSmpShow :: String -> P.XParse SmpShow
parseSmpShow s
        | s == "new" = return $ ShowNew
        | s == "replace" = return $ ShowReplace
        | s == "embed" = return $ ShowEmbed
        | s == "other" = return $ ShowOther
        | s == "none" = return $ ShowNone
        | otherwise = P.xfail $ "SmpShow: " ++ s

-- | @show-frets@ /(simple)/
--
-- The show-frets type indicates whether to show tablature frets as numbers (0, 1, 2) or letters (a, b, c). The default choice is numbers.
data ShowFrets = 
      ShowFretsNumbers -- ^ /numbers/
    | ShowFretsLetters -- ^ /letters/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ShowFrets where
    emitXml ShowFretsNumbers = XLit "numbers"
    emitXml ShowFretsLetters = XLit "letters"
parseShowFrets :: String -> P.XParse ShowFrets
parseShowFrets s
        | s == "numbers" = return $ ShowFretsNumbers
        | s == "letters" = return $ ShowFretsLetters
        | otherwise = P.xfail $ "ShowFrets: " ++ s

-- | @show-tuplet@ /(simple)/
--
-- The show-tuplet type indicates whether to show a part of a tuplet relating to the tuplet-actual element, both the tuplet-actual and tuplet-normal elements, or neither.
data ShowTuplet = 
      ShowTupletActual -- ^ /actual/
    | ShowTupletBoth -- ^ /both/
    | ShowTupletNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ShowTuplet where
    emitXml ShowTupletActual = XLit "actual"
    emitXml ShowTupletBoth = XLit "both"
    emitXml ShowTupletNone = XLit "none"
parseShowTuplet :: String -> P.XParse ShowTuplet
parseShowTuplet s
        | s == "actual" = return $ ShowTupletActual
        | s == "both" = return $ ShowTupletBoth
        | s == "none" = return $ ShowTupletNone
        | otherwise = P.xfail $ "ShowTuplet: " ++ s

-- | @staff-line@ /(simple)/
--
-- The staff-line type indicates the line on a given staff. Staff lines are numbered from bottom to top, with 1 being the bottom line on a staff. Staff line values can be used to specify positions outside the staff, such as a C clef positioned in the middle of a grand staff.
newtype StaffLine = StaffLine { staffLine :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show StaffLine where show (StaffLine a) = show a
instance Read StaffLine where readsPrec i = map (A.first StaffLine) . readsPrec i
instance EmitXml StaffLine where
    emitXml = emitXml . staffLine
parseStaffLine :: String -> P.XParse StaffLine
parseStaffLine = P.xread "StaffLine"

-- | @staff-number@ /(simple)/
--
-- The staff-number type indicates staff numbers within a multi-staff part. Staves are numbered from top to bottom, with 1 being the top staff on a part.
newtype StaffNumber = StaffNumber { staffNumber :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show StaffNumber where show (StaffNumber a) = show a
instance Read StaffNumber where readsPrec i = map (A.first StaffNumber) . readsPrec i
instance EmitXml StaffNumber where
    emitXml = emitXml . staffNumber
parseStaffNumber :: String -> P.XParse StaffNumber
parseStaffNumber = P.xread "StaffNumber"

-- | @staff-type@ /(simple)/
--
-- The staff-type value can be ossia, cue, editorial, regular, or alternate. An alternate staff indicates one that shares the same musical data as the prior staff, but displayed differently (e.g., treble and bass clef, standard notation and tab).
data StaffType = 
      StaffTypeOssia -- ^ /ossia/
    | StaffTypeCue -- ^ /cue/
    | StaffTypeEditorial -- ^ /editorial/
    | StaffTypeRegular -- ^ /regular/
    | StaffTypeAlternate -- ^ /alternate/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StaffType where
    emitXml StaffTypeOssia = XLit "ossia"
    emitXml StaffTypeCue = XLit "cue"
    emitXml StaffTypeEditorial = XLit "editorial"
    emitXml StaffTypeRegular = XLit "regular"
    emitXml StaffTypeAlternate = XLit "alternate"
parseStaffType :: String -> P.XParse StaffType
parseStaffType s
        | s == "ossia" = return $ StaffTypeOssia
        | s == "cue" = return $ StaffTypeCue
        | s == "editorial" = return $ StaffTypeEditorial
        | s == "regular" = return $ StaffTypeRegular
        | s == "alternate" = return $ StaffTypeAlternate
        | otherwise = P.xfail $ "StaffType: " ++ s

-- | @start-note@ /(simple)/
--
-- The start-note type describes the starting note of trills and mordents for playback, relative to the current note.
data StartNote = 
      StartNoteUpper -- ^ /upper/
    | StartNoteMain -- ^ /main/
    | StartNoteBelow -- ^ /below/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartNote where
    emitXml StartNoteUpper = XLit "upper"
    emitXml StartNoteMain = XLit "main"
    emitXml StartNoteBelow = XLit "below"
parseStartNote :: String -> P.XParse StartNote
parseStartNote s
        | s == "upper" = return $ StartNoteUpper
        | s == "main" = return $ StartNoteMain
        | s == "below" = return $ StartNoteBelow
        | otherwise = P.xfail $ "StartNote: " ++ s

-- | @start-stop@ /(simple)/
--
-- The start-stop type is used for an attribute of musical elements that can either start or stop, such as tuplets, wedges, and lines.
data StartStop = 
      StartStopStart -- ^ /start/
    | StartStopStop -- ^ /stop/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStop where
    emitXml StartStopStart = XLit "start"
    emitXml StartStopStop = XLit "stop"
parseStartStop :: String -> P.XParse StartStop
parseStartStop s
        | s == "start" = return $ StartStopStart
        | s == "stop" = return $ StartStopStop
        | otherwise = P.xfail $ "StartStop: " ++ s

-- | @start-stop-change@ /(simple)/
--
-- The start-stop-change type is used to distinguish types of pedal directions.
data StartStopChange = 
      StartStopChangeStart -- ^ /start/
    | StartStopChangeStop -- ^ /stop/
    | StartStopChangeChange -- ^ /change/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopChange where
    emitXml StartStopChangeStart = XLit "start"
    emitXml StartStopChangeStop = XLit "stop"
    emitXml StartStopChangeChange = XLit "change"
parseStartStopChange :: String -> P.XParse StartStopChange
parseStartStopChange s
        | s == "start" = return $ StartStopChangeStart
        | s == "stop" = return $ StartStopChangeStop
        | s == "change" = return $ StartStopChangeChange
        | otherwise = P.xfail $ "StartStopChange: " ++ s

-- | @start-stop-continue@ /(simple)/
--
-- The start-stop-continue type is used for an attribute of musical elements that can either start or stop, but also need to refer to an intermediate point in the symbol, as for complex slurs.
data StartStopContinue = 
      StartStopContinueStart -- ^ /start/
    | StartStopContinueStop -- ^ /stop/
    | StartStopContinueContinue -- ^ /continue/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopContinue where
    emitXml StartStopContinueStart = XLit "start"
    emitXml StartStopContinueStop = XLit "stop"
    emitXml StartStopContinueContinue = XLit "continue"
parseStartStopContinue :: String -> P.XParse StartStopContinue
parseStartStopContinue s
        | s == "start" = return $ StartStopContinueStart
        | s == "stop" = return $ StartStopContinueStop
        | s == "continue" = return $ StartStopContinueContinue
        | otherwise = P.xfail $ "StartStopContinue: " ++ s

-- | @start-stop-discontinue@ /(simple)/
--
-- The start-stop-discontinue type is used to specify ending types. Typically, the start type is associated with the left barline of the first measure in an ending. The stop and discontinue types are associated with the right barline of the last measure in an ending. Stop is used when the ending mark concludes with a downward jog, as is typical for first endings. Discontinue is used when there is no downward jog, as is typical for second endings that do not conclude a piece.
data StartStopDiscontinue = 
      StartStopDiscontinueStart -- ^ /start/
    | StartStopDiscontinueStop -- ^ /stop/
    | StartStopDiscontinueDiscontinue -- ^ /discontinue/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopDiscontinue where
    emitXml StartStopDiscontinueStart = XLit "start"
    emitXml StartStopDiscontinueStop = XLit "stop"
    emitXml StartStopDiscontinueDiscontinue = XLit "discontinue"
parseStartStopDiscontinue :: String -> P.XParse StartStopDiscontinue
parseStartStopDiscontinue s
        | s == "start" = return $ StartStopDiscontinueStart
        | s == "stop" = return $ StartStopDiscontinueStop
        | s == "discontinue" = return $ StartStopDiscontinueDiscontinue
        | otherwise = P.xfail $ "StartStopDiscontinue: " ++ s

-- | @start-stop-single@ /(simple)/
--
-- The start-stop-single type is used for an attribute of musical elements that can be used for either multi-note or single-note musical elements, as for tremolos.
data StartStopSingle = 
      StartStopSingleStart -- ^ /start/
    | StartStopSingleStop -- ^ /stop/
    | StartStopSingleSingle -- ^ /single/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopSingle where
    emitXml StartStopSingleStart = XLit "start"
    emitXml StartStopSingleStop = XLit "stop"
    emitXml StartStopSingleSingle = XLit "single"
parseStartStopSingle :: String -> P.XParse StartStopSingle
parseStartStopSingle s
        | s == "start" = return $ StartStopSingleStart
        | s == "stop" = return $ StartStopSingleStop
        | s == "single" = return $ StartStopSingleSingle
        | otherwise = P.xfail $ "StartStopSingle: " ++ s

-- | @stem-value@ /(simple)/
--
-- The stem type represents the notated stem direction.
data StemValue = 
      StemValueDown -- ^ /down/
    | StemValueUp -- ^ /up/
    | StemValueDouble -- ^ /double/
    | StemValueNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StemValue where
    emitXml StemValueDown = XLit "down"
    emitXml StemValueUp = XLit "up"
    emitXml StemValueDouble = XLit "double"
    emitXml StemValueNone = XLit "none"
parseStemValue :: String -> P.XParse StemValue
parseStemValue s
        | s == "down" = return $ StemValueDown
        | s == "up" = return $ StemValueUp
        | s == "double" = return $ StemValueDouble
        | s == "none" = return $ StemValueNone
        | otherwise = P.xfail $ "StemValue: " ++ s

-- | @step@ /(simple)/
--
-- The step type represents a step of the diatonic scale, represented using the English letters A through G.
data Step = 
      StepA -- ^ /A/
    | StepB -- ^ /B/
    | StepC -- ^ /C/
    | StepD -- ^ /D/
    | StepE -- ^ /E/
    | StepF -- ^ /F/
    | StepG -- ^ /G/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Step where
    emitXml StepA = XLit "A"
    emitXml StepB = XLit "B"
    emitXml StepC = XLit "C"
    emitXml StepD = XLit "D"
    emitXml StepE = XLit "E"
    emitXml StepF = XLit "F"
    emitXml StepG = XLit "G"
parseStep :: String -> P.XParse Step
parseStep s
        | s == "A" = return $ StepA
        | s == "B" = return $ StepB
        | s == "C" = return $ StepC
        | s == "D" = return $ StepD
        | s == "E" = return $ StepE
        | s == "F" = return $ StepF
        | s == "G" = return $ StepG
        | otherwise = P.xfail $ "Step: " ++ s

-- | @string-number@ /(simple)/
--
-- The string-number type indicates a string number. Strings are numbered from high to low, with 1 being the highest pitched string.
newtype StringNumber = StringNumber { stringNumber :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show StringNumber where show (StringNumber a) = show a
instance Read StringNumber where readsPrec i = map (A.first StringNumber) . readsPrec i
instance EmitXml StringNumber where
    emitXml = emitXml . stringNumber
parseStringNumber :: String -> P.XParse StringNumber
parseStringNumber = P.xread "StringNumber"

-- | @syllabic@ /(simple)/
--
-- Lyric hyphenation is indicated by the syllabic type. The single, begin, end, and middle values represent single-syllable words, word-beginning syllables, word-ending syllables, and mid-word syllables, respectively.
data Syllabic = 
      SyllabicSingle -- ^ /single/
    | SyllabicBegin -- ^ /begin/
    | SyllabicEnd -- ^ /end/
    | SyllabicMiddle -- ^ /middle/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Syllabic where
    emitXml SyllabicSingle = XLit "single"
    emitXml SyllabicBegin = XLit "begin"
    emitXml SyllabicEnd = XLit "end"
    emitXml SyllabicMiddle = XLit "middle"
parseSyllabic :: String -> P.XParse Syllabic
parseSyllabic s
        | s == "single" = return $ SyllabicSingle
        | s == "begin" = return $ SyllabicBegin
        | s == "end" = return $ SyllabicEnd
        | s == "middle" = return $ SyllabicMiddle
        | otherwise = P.xfail $ "Syllabic: " ++ s

-- | @symbol-size@ /(simple)/
--
-- The symbol-size type is used to indicate full vs. cue-sized vs. oversized symbols. The large value for oversized symbols was added in version 1.1.
data SymbolSize = 
      SymbolSizeFull -- ^ /full/
    | SymbolSizeCue -- ^ /cue/
    | SymbolSizeLarge -- ^ /large/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SymbolSize where
    emitXml SymbolSizeFull = XLit "full"
    emitXml SymbolSizeCue = XLit "cue"
    emitXml SymbolSizeLarge = XLit "large"
parseSymbolSize :: String -> P.XParse SymbolSize
parseSymbolSize s
        | s == "full" = return $ SymbolSizeFull
        | s == "cue" = return $ SymbolSizeCue
        | s == "large" = return $ SymbolSizeLarge
        | otherwise = P.xfail $ "SymbolSize: " ++ s

-- | @tenths@ /(simple)/
--
-- The tenths type is a number representing tenths of interline staff space (positive or negative). Both integer and decimal values are allowed, such as 5 for a half space and 2.5 for a quarter space. Interline space is measured from the middle of a staff line.
-- 
-- Distances in a MusicXML file are measured in tenths of staff space. Tenths are then scaled to millimeters within the scaling element, used in the defaults element at the start of a score. Individual staves can apply a scaling factor to adjust staff size. When a MusicXML element or attribute refers to tenths, it means the global tenths defined by the scaling element, not the local tenths as adjusted by the staff-size element.
newtype Tenths = Tenths { tenths :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Tenths where show (Tenths a) = show a
instance Read Tenths where readsPrec i = map (A.first Tenths) . readsPrec i
instance EmitXml Tenths where
    emitXml = emitXml . tenths
parseTenths :: String -> P.XParse Tenths
parseTenths = P.xread "Tenths"

-- | @text-direction@ /(simple)/
--
-- The text-direction type is used to adjust and override the Unicode bidirectional text algorithm, similar to the W3C Internationalization Tag Set recommendation. Values are ltr (left-to-right embed), rtl (right-to-left embed), lro (left-to-right bidi-override), and rlo (right-to-left bidi-override). The default value is ltr. This type is typically used by applications that store text in left-to-right visual order rather than logical order. Such applications can use the lro value to better communicate with other applications that more fully support bidirectional text.
data TextDirection = 
      TextDirectionLtr -- ^ /ltr/
    | TextDirectionRtl -- ^ /rtl/
    | TextDirectionLro -- ^ /lro/
    | TextDirectionRlo -- ^ /rlo/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TextDirection where
    emitXml TextDirectionLtr = XLit "ltr"
    emitXml TextDirectionRtl = XLit "rtl"
    emitXml TextDirectionLro = XLit "lro"
    emitXml TextDirectionRlo = XLit "rlo"
parseTextDirection :: String -> P.XParse TextDirection
parseTextDirection s
        | s == "ltr" = return $ TextDirectionLtr
        | s == "rtl" = return $ TextDirectionRtl
        | s == "lro" = return $ TextDirectionLro
        | s == "rlo" = return $ TextDirectionRlo
        | otherwise = P.xfail $ "TextDirection: " ++ s

-- | @time-symbol@ /(simple)/
--
-- The time-symbol type indicates how to display a time signature. The normal value is the usual fractional display, and is the implied symbol type if none is specified. Other options are the common and cut time symbols, as well as a single number with an implied denominator.
data TimeSymbol = 
      TimeSymbolCommon -- ^ /common/
    | TimeSymbolCut -- ^ /cut/
    | TimeSymbolSingleNumber -- ^ /single-number/
    | TimeSymbolNormal -- ^ /normal/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TimeSymbol where
    emitXml TimeSymbolCommon = XLit "common"
    emitXml TimeSymbolCut = XLit "cut"
    emitXml TimeSymbolSingleNumber = XLit "single-number"
    emitXml TimeSymbolNormal = XLit "normal"
parseTimeSymbol :: String -> P.XParse TimeSymbol
parseTimeSymbol s
        | s == "common" = return $ TimeSymbolCommon
        | s == "cut" = return $ TimeSymbolCut
        | s == "single-number" = return $ TimeSymbolSingleNumber
        | s == "normal" = return $ TimeSymbolNormal
        | otherwise = P.xfail $ "TimeSymbol: " ++ s

-- | @xs:token@ /(simple)/
newtype Token = Token { token :: NormalizedString }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Token where show (Token a) = show a
instance Read Token where readsPrec i = map (A.first Token) . readsPrec i
instance EmitXml Token where
    emitXml = emitXml . token
parseToken :: String -> P.XParse Token
parseToken = return . fromString

-- | @top-bottom@ /(simple)/
--
-- The top-bottom type is used to indicate the top or bottom part of a vertical shape like non-arpeggiate.
data TopBottom = 
      TopBottomTop -- ^ /top/
    | TopBottomBottom -- ^ /bottom/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TopBottom where
    emitXml TopBottomTop = XLit "top"
    emitXml TopBottomBottom = XLit "bottom"
parseTopBottom :: String -> P.XParse TopBottom
parseTopBottom s
        | s == "top" = return $ TopBottomTop
        | s == "bottom" = return $ TopBottomBottom
        | otherwise = P.xfail $ "TopBottom: " ++ s

-- | @tremolo-marks@ /(simple)/
--
-- The number of tremolo marks is represented by a number from 0 to 6: the same as beam-level with 0 added.
newtype TremoloMarks = TremoloMarks { tremoloMarks :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show TremoloMarks where show (TremoloMarks a) = show a
instance Read TremoloMarks where readsPrec i = map (A.first TremoloMarks) . readsPrec i
instance EmitXml TremoloMarks where
    emitXml = emitXml . tremoloMarks
parseTremoloMarks :: String -> P.XParse TremoloMarks
parseTremoloMarks = P.xread "TremoloMarks"

-- | @trill-beats@ /(simple)/
--
-- The trill-beats type specifies the beats used in a trill-sound or bend-sound attribute group. It is a decimal value with a minimum value of 2.
newtype TrillBeats = TrillBeats { trillBeats :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show TrillBeats where show (TrillBeats a) = show a
instance Read TrillBeats where readsPrec i = map (A.first TrillBeats) . readsPrec i
instance EmitXml TrillBeats where
    emitXml = emitXml . trillBeats
parseTrillBeats :: String -> P.XParse TrillBeats
parseTrillBeats = P.xread "TrillBeats"

-- | @trill-step@ /(simple)/
--
-- The trill-step type describes the alternating note of trills and mordents for playback, relative to the current note.
data TrillStep = 
      TrillStepWhole -- ^ /whole/
    | TrillStepHalf -- ^ /half/
    | TrillStepUnison -- ^ /unison/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TrillStep where
    emitXml TrillStepWhole = XLit "whole"
    emitXml TrillStepHalf = XLit "half"
    emitXml TrillStepUnison = XLit "unison"
parseTrillStep :: String -> P.XParse TrillStep
parseTrillStep s
        | s == "whole" = return $ TrillStepWhole
        | s == "half" = return $ TrillStepHalf
        | s == "unison" = return $ TrillStepUnison
        | otherwise = P.xfail $ "TrillStep: " ++ s

-- | @two-note-turn@ /(simple)/
--
-- The two-note-turn type describes the ending notes of trills and mordents for playback, relative to the current note.
data TwoNoteTurn = 
      TwoNoteTurnWhole -- ^ /whole/
    | TwoNoteTurnHalf -- ^ /half/
    | TwoNoteTurnNone -- ^ /none/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TwoNoteTurn where
    emitXml TwoNoteTurnWhole = XLit "whole"
    emitXml TwoNoteTurnHalf = XLit "half"
    emitXml TwoNoteTurnNone = XLit "none"
parseTwoNoteTurn :: String -> P.XParse TwoNoteTurn
parseTwoNoteTurn s
        | s == "whole" = return $ TwoNoteTurnWhole
        | s == "half" = return $ TwoNoteTurnHalf
        | s == "none" = return $ TwoNoteTurnNone
        | otherwise = P.xfail $ "TwoNoteTurn: " ++ s

-- | @xlink:type@ /(simple)/
data Type = 
      TypeSimple -- ^ /simple/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Type where
    emitXml TypeSimple = XLit "simple"
parseType :: String -> P.XParse Type
parseType s
        | s == "simple" = return $ TypeSimple
        | otherwise = P.xfail $ "Type: " ++ s

-- | @up-down@ /(simple)/
--
-- The up-down type is used for arrow direction, indicating which way the tip is pointing.
data UpDown = 
      UpDownUp -- ^ /up/
    | UpDownDown -- ^ /down/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml UpDown where
    emitXml UpDownUp = XLit "up"
    emitXml UpDownDown = XLit "down"
parseUpDown :: String -> P.XParse UpDown
parseUpDown s
        | s == "up" = return $ UpDownUp
        | s == "down" = return $ UpDownDown
        | otherwise = P.xfail $ "UpDown: " ++ s

-- | @up-down-stop@ /(simple)/
--
-- The up-down-stop type is used for octave-shift elements, indicating the direction of the shift from their true pitched values because of printing difficulty.
data UpDownStop = 
      UpDownStopUp -- ^ /up/
    | UpDownStopDown -- ^ /down/
    | UpDownStopStop -- ^ /stop/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml UpDownStop where
    emitXml UpDownStopUp = XLit "up"
    emitXml UpDownStopDown = XLit "down"
    emitXml UpDownStopStop = XLit "stop"
parseUpDownStop :: String -> P.XParse UpDownStop
parseUpDownStop s
        | s == "up" = return $ UpDownStopUp
        | s == "down" = return $ UpDownStopDown
        | s == "stop" = return $ UpDownStopStop
        | otherwise = P.xfail $ "UpDownStop: " ++ s

-- | @upright-inverted@ /(simple)/
--
-- The upright-inverted type describes the appearance of a fermata element. The value is upright if not specified.
data UprightInverted = 
      UprightInvertedUpright -- ^ /upright/
    | UprightInvertedInverted -- ^ /inverted/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml UprightInverted where
    emitXml UprightInvertedUpright = XLit "upright"
    emitXml UprightInvertedInverted = XLit "inverted"
parseUprightInverted :: String -> P.XParse UprightInverted
parseUprightInverted s
        | s == "upright" = return $ UprightInvertedUpright
        | s == "inverted" = return $ UprightInvertedInverted
        | otherwise = P.xfail $ "UprightInverted: " ++ s

-- | @valign@ /(simple)/
--
-- The valign type is used to indicate vertical alignment to the top, middle, bottom, or baseline of the text. Defaults are implementation-dependent.
data Valign = 
      ValignTop -- ^ /top/
    | ValignMiddle -- ^ /middle/
    | ValignBottom -- ^ /bottom/
    | ValignBaseline -- ^ /baseline/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Valign where
    emitXml ValignTop = XLit "top"
    emitXml ValignMiddle = XLit "middle"
    emitXml ValignBottom = XLit "bottom"
    emitXml ValignBaseline = XLit "baseline"
parseValign :: String -> P.XParse Valign
parseValign s
        | s == "top" = return $ ValignTop
        | s == "middle" = return $ ValignMiddle
        | s == "bottom" = return $ ValignBottom
        | s == "baseline" = return $ ValignBaseline
        | otherwise = P.xfail $ "Valign: " ++ s

-- | @valign-image@ /(simple)/
--
-- The valign-image type is used to indicate vertical alignment for images and graphics, so it does not include a baseline value. Defaults are implementation-dependent.
data ValignImage = 
      ValignImageTop -- ^ /top/
    | ValignImageMiddle -- ^ /middle/
    | ValignImageBottom -- ^ /bottom/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ValignImage where
    emitXml ValignImageTop = XLit "top"
    emitXml ValignImageMiddle = XLit "middle"
    emitXml ValignImageBottom = XLit "bottom"
parseValignImage :: String -> P.XParse ValignImage
parseValignImage s
        | s == "top" = return $ ValignImageTop
        | s == "middle" = return $ ValignImageMiddle
        | s == "bottom" = return $ ValignImageBottom
        | otherwise = P.xfail $ "ValignImage: " ++ s

-- | @wedge-type@ /(simple)/
--
-- The wedge type is crescendo for the start of a wedge that is closed at the left side, diminuendo for the start of a wedge that is closed on the right side, and stop for the end of a wedge.
data WedgeType = 
      WedgeTypeCrescendo -- ^ /crescendo/
    | WedgeTypeDiminuendo -- ^ /diminuendo/
    | WedgeTypeStop -- ^ /stop/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml WedgeType where
    emitXml WedgeTypeCrescendo = XLit "crescendo"
    emitXml WedgeTypeDiminuendo = XLit "diminuendo"
    emitXml WedgeTypeStop = XLit "stop"
parseWedgeType :: String -> P.XParse WedgeType
parseWedgeType s
        | s == "crescendo" = return $ WedgeTypeCrescendo
        | s == "diminuendo" = return $ WedgeTypeDiminuendo
        | s == "stop" = return $ WedgeTypeStop
        | otherwise = P.xfail $ "WedgeType: " ++ s

-- | @yes-no@ /(simple)/
--
-- The yes-no type is used for boolean-like attributes. We cannot use W3C XML Schema booleans due to their restrictions on expression of boolean values.
data YesNo = 
      YesNoYes -- ^ /yes/
    | YesNoNo -- ^ /no/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml YesNo where
    emitXml YesNoYes = XLit "yes"
    emitXml YesNoNo = XLit "no"
parseYesNo :: String -> P.XParse YesNo
parseYesNo s
        | s == "yes" = return $ YesNoYes
        | s == "no" = return $ YesNoNo
        | otherwise = P.xfail $ "YesNo: " ++ s

-- | @yes-no-number@ /(simple)/
--
-- The yes-no-number type is used for attributes that can be either boolean or numeric values.
data YesNoNumber = 
      YesNoNumberYesNo {
          yesNoNumber1 :: YesNo
       }
    | YesNoNumberDecimal {
          yesNoNumber2 :: Decimal
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml YesNoNumber where
    emitXml (YesNoNumberYesNo a) = emitXml a
    emitXml (YesNoNumberDecimal a) = emitXml a
parseYesNoNumber :: String -> P.XParse YesNoNumber
parseYesNoNumber s = 
      YesNoNumberYesNo
        <$> parseYesNo s
      <|> YesNoNumberDecimal
        <$> (P.xread "Decimal") s


-- | @yyyy-mm-dd@ /(simple)/
--
-- Calendar dates are represented yyyy-mm-dd format, following ISO 8601. This is a W3C XML Schema date type, but without the optional timezone data.
newtype YyyyMmDd = YyyyMmDd { yyyyMmDd :: String }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show YyyyMmDd where show (YyyyMmDd a) = show a
instance Read YyyyMmDd where readsPrec i = map (A.first YyyyMmDd) . readsPrec i
instance EmitXml YyyyMmDd where
    emitXml = emitXml . yyyyMmDd
parseYyyyMmDd :: String -> P.XParse YyyyMmDd
parseYyyyMmDd = return . fromString

-- | @xml:lang@ /(union)/
data SumLang = 
      SumLang -- ^ //
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SumLang where
    emitXml SumLang = XLit ""
parseSumLang :: String -> P.XParse SumLang
parseSumLang s
        | s == "" = return $ SumLang
        | otherwise = P.xfail $ "SumLang: " ++ s

-- | @number-or-normal@ /(union)/
data SumNumberOrNormal = 
      NumberOrNormalNormal -- ^ /normal/
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SumNumberOrNormal where
    emitXml NumberOrNormalNormal = XLit "normal"
parseSumNumberOrNormal :: String -> P.XParse SumNumberOrNormal
parseSumNumberOrNormal s
        | s == "normal" = return $ NumberOrNormalNormal
        | otherwise = P.xfail $ "SumNumberOrNormal: " ++ s

-- | @positive-integer-or-empty@ /(union)/
data SumPositiveIntegerOrEmpty = 
      SumPositiveIntegerOrEmpty -- ^ //
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SumPositiveIntegerOrEmpty where
    emitXml SumPositiveIntegerOrEmpty = XLit ""
parseSumPositiveIntegerOrEmpty :: String -> P.XParse SumPositiveIntegerOrEmpty
parseSumPositiveIntegerOrEmpty s
        | s == "" = return $ SumPositiveIntegerOrEmpty
        | otherwise = P.xfail $ "SumPositiveIntegerOrEmpty: " ++ s

-- | @accidental@ /(complex)/
--
-- The accidental type represents actual notated accidentals. Editorial and cautionary indications are indicated by attributes. Values for these attributes are "no" if not present. Specific graphic display such as parentheses, brackets, and size are controlled by the level-display attribute group.
data Accidental = 
      Accidental {
          accidentalAccidentalValue :: AccidentalValue -- ^ text content
        , accidentalCautionary :: (Maybe YesNo) -- ^ /cautionary/ attribute
        , accidentalEditorial :: (Maybe YesNo) -- ^ /editorial/ attribute
        , accidentalParentheses :: (Maybe YesNo) -- ^ /parentheses/ attribute
        , accidentalBracket :: (Maybe YesNo) -- ^ /bracket/ attribute
        , accidentalSize :: (Maybe SymbolSize) -- ^ /size/ attribute
        , accidentalDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , accidentalDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , accidentalRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , accidentalRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , accidentalFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , accidentalFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , accidentalFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , accidentalFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , accidentalColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Accidental where
    emitXml (Accidental a b c d e f g h i j k l m n o) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "cautionary" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "editorial" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o])
        []
parseAccidental :: P.XParse Accidental
parseAccidental = 
      Accidental
        <$> (P.xtext >>= parseAccidentalValue)
        <*> P.optional (P.xattr (P.name "cautionary") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "editorial") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "parentheses") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "bracket") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "size") >>= parseSymbolSize)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Accidental'
mkAccidental :: AccidentalValue -> Accidental
mkAccidental a = Accidental a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @accidental-mark@ /(complex)/
--
-- An accidental-mark can be used as a separate notation or as part of an ornament. When used in an ornament, position and placement are relative to the ornament, not relative to the note.
data AccidentalMark = 
      AccidentalMark {
          accidentalMarkAccidentalValue :: AccidentalValue -- ^ text content
        , accidentalMarkDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , accidentalMarkDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , accidentalMarkRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , accidentalMarkRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , accidentalMarkFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , accidentalMarkFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , accidentalMarkFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , accidentalMarkFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , accidentalMarkColor :: (Maybe Color) -- ^ /color/ attribute
        , accidentalMarkPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AccidentalMark where
    emitXml (AccidentalMark a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        []
parseAccidentalMark :: P.XParse AccidentalMark
parseAccidentalMark = 
      AccidentalMark
        <$> (P.xtext >>= parseAccidentalValue)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'AccidentalMark'
mkAccidentalMark :: AccidentalValue -> AccidentalMark
mkAccidentalMark a = AccidentalMark a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @accidental-text@ /(complex)/
--
-- The accidental-text type represents an element with an accidental value and text-formatting attributes.
data AccidentalText = 
      AccidentalText {
          accidentalTextAccidentalValue :: AccidentalValue -- ^ text content
        , accidentalTextLang :: (Maybe Lang) -- ^ /xml:lang/ attribute
        , accidentalTextEnclosure :: (Maybe Enclosure) -- ^ /enclosure/ attribute
        , accidentalTextJustify :: (Maybe LeftCenterRight) -- ^ /justify/ attribute
        , accidentalTextHalign :: (Maybe LeftCenterRight) -- ^ /halign/ attribute
        , accidentalTextValign :: (Maybe Valign) -- ^ /valign/ attribute
        , accidentalTextDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , accidentalTextDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , accidentalTextRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , accidentalTextRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , accidentalTextFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , accidentalTextFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , accidentalTextFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , accidentalTextFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , accidentalTextColor :: (Maybe Color) -- ^ /color/ attribute
        , accidentalTextUnderline :: (Maybe NumberOfLines) -- ^ /underline/ attribute
        , accidentalTextOverline :: (Maybe NumberOfLines) -- ^ /overline/ attribute
        , accidentalTextLineThrough :: (Maybe NumberOfLines) -- ^ /line-through/ attribute
        , accidentalTextRotation :: (Maybe RotationDegrees) -- ^ /rotation/ attribute
        , accidentalTextLetterSpacing :: (Maybe NumberOrNormal) -- ^ /letter-spacing/ attribute
        , accidentalTextLineHeight :: (Maybe NumberOrNormal) -- ^ /line-height/ attribute
        , accidentalTextDir :: (Maybe TextDirection) -- ^ /dir/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AccidentalText where
    emitXml (AccidentalText a b c d e f g h i j k l m n o p q r s t u v) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "enclosure" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) q]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) r]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) s]++[maybe XEmpty (XAttr (QN "letter-spacing" Nothing).emitXml) t]++[maybe XEmpty (XAttr (QN "line-height" Nothing).emitXml) u]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) v])
        []
parseAccidentalText :: P.XParse AccidentalText
parseAccidentalText = 
      AccidentalText
        <$> (P.xtext >>= parseAccidentalValue)
        <*> P.optional (P.xattr (P.name "xml:lang") >>= parseLang)
        <*> P.optional (P.xattr (P.name "enclosure") >>= parseEnclosure)
        <*> P.optional (P.xattr (P.name "justify") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "halign") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "valign") >>= parseValign)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "underline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "overline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "line-through") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "rotation") >>= parseRotationDegrees)
        <*> P.optional (P.xattr (P.name "letter-spacing") >>= parseNumberOrNormal)
        <*> P.optional (P.xattr (P.name "line-height") >>= parseNumberOrNormal)
        <*> P.optional (P.xattr (P.name "dir") >>= parseTextDirection)

-- | Smart constructor for 'AccidentalText'
mkAccidentalText :: AccidentalValue -> AccidentalText
mkAccidentalText a = AccidentalText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @accord@ /(complex)/
--
-- The accord type represents the tuning of a single string in the scordatura element. It uses the same group of elements as the staff-tuning element. Strings are numbered from high to low.
data Accord = 
      Accord {
          accordString :: (Maybe StringNumber) -- ^ /string/ attribute
        , accordTuning :: Tuning
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Accord where
    emitXml (Accord a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "string" Nothing).emitXml) a])
        ([emitXml b])
parseAccord :: P.XParse Accord
parseAccord = 
      Accord
        <$> P.optional (P.xattr (P.name "string") >>= parseStringNumber)
        <*> parseTuning

-- | Smart constructor for 'Accord'
mkAccord :: Tuning -> Accord
mkAccord b = Accord Nothing b

-- | @accordion-registration@ /(complex)/
--
-- The accordion-registration type is use for accordion registration symbols. These are circular symbols divided horizontally into high, middle, and low sections that correspond to 4', 8', and 16' pipes. Each accordion-high, accordion-middle, and accordion-low element represents the presence of one or more dots in the registration diagram. An accordion-registration element needs to have at least one of the child elements present.
data AccordionRegistration = 
      AccordionRegistration {
          accordionRegistrationDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , accordionRegistrationDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , accordionRegistrationRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , accordionRegistrationRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , accordionRegistrationFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , accordionRegistrationFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , accordionRegistrationFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , accordionRegistrationFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , accordionRegistrationColor :: (Maybe Color) -- ^ /color/ attribute
        , accordionRegistrationAccordionHigh :: (Maybe Empty) -- ^ /accordion-high/ child element
        , accordionRegistrationAccordionMiddle :: (Maybe AccordionMiddle) -- ^ /accordion-middle/ child element
        , accordionRegistrationAccordionLow :: (Maybe Empty) -- ^ /accordion-low/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AccordionRegistration where
    emitXml (AccordionRegistration a b c d e f g h i j k l) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        ([maybe XEmpty (XElement (QN "accordion-high" Nothing).emitXml) j]++[maybe XEmpty (XElement (QN "accordion-middle" Nothing).emitXml) k]++[maybe XEmpty (XElement (QN "accordion-low" Nothing).emitXml) l])
parseAccordionRegistration :: P.XParse AccordionRegistration
parseAccordionRegistration = 
      AccordionRegistration
        <$> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xchild (P.name "accordion-high") (parseEmpty))
        <*> P.optional (P.xchild (P.name "accordion-middle") (P.xtext >>= parseAccordionMiddle))
        <*> P.optional (P.xchild (P.name "accordion-low") (parseEmpty))

-- | Smart constructor for 'AccordionRegistration'
mkAccordionRegistration :: AccordionRegistration
mkAccordionRegistration = AccordionRegistration Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @appearance@ /(complex)/
--
-- The appearance type controls general graphical settings for the music's final form appearance on a printed page of display. Currently this includes support for line widths and definitions for note sizes, plus an extension element for other aspects of appearance.
data Appearance = 
      Appearance {
          appearanceLineWidth :: [LineWidth] -- ^ /line-width/ child element
        , appearanceNoteSize :: [NoteSize] -- ^ /note-size/ child element
        , appearanceOtherAppearance :: [OtherAppearance] -- ^ /other-appearance/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Appearance where
    emitXml (Appearance a b c) =
      XContent XEmpty
        []
        (map (XElement (QN "line-width" Nothing).emitXml) a++map (XElement (QN "note-size" Nothing).emitXml) b++map (XElement (QN "other-appearance" Nothing).emitXml) c)
parseAppearance :: P.XParse Appearance
parseAppearance = 
      Appearance
        <$> P.many (P.xchild (P.name "line-width") (parseLineWidth))
        <*> P.many (P.xchild (P.name "note-size") (parseNoteSize))
        <*> P.many (P.xchild (P.name "other-appearance") (parseOtherAppearance))

-- | Smart constructor for 'Appearance'
mkAppearance :: Appearance
mkAppearance = Appearance [] [] []

-- | @arpeggiate@ /(complex)/
--
-- The arpeggiate type indicates that this note is part of an arpeggiated chord. The number attribute can be used to distinguish between two simultaneous chords arpeggiated separately (different numbers) or together (same number). The up-down attribute is used if there is an arrow on the arpeggio sign. By default, arpeggios go from the lowest to highest note.
data Arpeggiate = 
      Arpeggiate {
          arpeggiateNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , arpeggiateDirection :: (Maybe UpDown) -- ^ /direction/ attribute
        , arpeggiateDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , arpeggiateDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , arpeggiateRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , arpeggiateRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , arpeggiatePlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , arpeggiateColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Arpeggiate where
    emitXml (Arpeggiate a b c d e f g h) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "direction" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
parseArpeggiate :: P.XParse Arpeggiate
parseArpeggiate = 
      Arpeggiate
        <$> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "direction") >>= parseUpDown)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Arpeggiate'
mkArpeggiate :: Arpeggiate
mkArpeggiate = Arpeggiate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @articulations@ /(complex)/
--
-- Articulations and accents are grouped together here.
data Articulations = 
      Articulations {
          articulationsArticulations :: [ChxArticulations]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Articulations where
    emitXml (Articulations a) =
      XReps [emitXml a]
parseArticulations :: P.XParse Articulations
parseArticulations = 
      Articulations
        <$> P.many (parseChxArticulations)

-- | Smart constructor for 'Articulations'
mkArticulations :: Articulations
mkArticulations = Articulations []

-- | @attributes@ /(complex)/
--
-- The attributes element contains musical information that typically changes on measure boundaries. This includes key and time signatures, clefs, transpositions, and staving.
data Attributes = 
      Attributes {
          attributesEditorial :: Editorial
        , attributesDivisions :: (Maybe PositiveDivisions) -- ^ /divisions/ child element
        , attributesKey :: [Key] -- ^ /key/ child element
        , attributesTime :: [Time] -- ^ /time/ child element
        , attributesStaves :: (Maybe NonNegativeInteger) -- ^ /staves/ child element
        , attributesPartSymbol :: (Maybe PartSymbol) -- ^ /part-symbol/ child element
        , attributesInstruments :: (Maybe NonNegativeInteger) -- ^ /instruments/ child element
        , attributesClef :: [Clef] -- ^ /clef/ child element
        , attributesStaffDetails :: [StaffDetails] -- ^ /staff-details/ child element
        , attributesTranspose :: (Maybe Transpose) -- ^ /transpose/ child element
        , attributesDirective :: [Directive] -- ^ /directive/ child element
        , attributesMeasureStyle :: [MeasureStyle] -- ^ /measure-style/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Attributes where
    emitXml (Attributes a b c d e f g h i j k l) =
      XContent XEmpty
        []
        ([emitXml a]++[maybe XEmpty (XElement (QN "divisions" Nothing).emitXml) b]++map (XElement (QN "key" Nothing).emitXml) c++map (XElement (QN "time" Nothing).emitXml) d++[maybe XEmpty (XElement (QN "staves" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "part-symbol" Nothing).emitXml) f]++[maybe XEmpty (XElement (QN "instruments" Nothing).emitXml) g]++map (XElement (QN "clef" Nothing).emitXml) h++map (XElement (QN "staff-details" Nothing).emitXml) i++[maybe XEmpty (XElement (QN "transpose" Nothing).emitXml) j]++map (XElement (QN "directive" Nothing).emitXml) k++map (XElement (QN "measure-style" Nothing).emitXml) l)
parseAttributes :: P.XParse Attributes
parseAttributes = 
      Attributes
        <$> parseEditorial
        <*> P.optional (P.xchild (P.name "divisions") (P.xtext >>= parsePositiveDivisions))
        <*> P.many (P.xchild (P.name "key") (parseKey))
        <*> P.many (P.xchild (P.name "time") (parseTime))
        <*> P.optional (P.xchild (P.name "staves") (P.xtext >>= parseNonNegativeInteger))
        <*> P.optional (P.xchild (P.name "part-symbol") (parsePartSymbol))
        <*> P.optional (P.xchild (P.name "instruments") (P.xtext >>= parseNonNegativeInteger))
        <*> P.many (P.xchild (P.name "clef") (parseClef))
        <*> P.many (P.xchild (P.name "staff-details") (parseStaffDetails))
        <*> P.optional (P.xchild (P.name "transpose") (parseTranspose))
        <*> P.many (P.xchild (P.name "directive") (parseDirective))
        <*> P.many (P.xchild (P.name "measure-style") (parseMeasureStyle))

-- | Smart constructor for 'Attributes'
mkAttributes :: Editorial -> Attributes
mkAttributes a = Attributes a Nothing [] [] Nothing Nothing Nothing [] [] Nothing [] []

-- | @backup@ /(complex)/
--
-- The backup and forward elements are required to coordinate multiple voices in one part, including music on multiple staves. The backup type is generally used to move between voices and staves. Thus the backup element does not include voice or staff elements. Duration values should always be positive, and should not cross measure boundaries.
data Backup = 
      Backup {
          backupDuration :: Duration
        , backupEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Backup where
    emitXml (Backup a b) =
      XReps [emitXml a,emitXml b]
parseBackup :: P.XParse Backup
parseBackup = 
      Backup
        <$> parseDuration
        <*> parseEditorial

-- | Smart constructor for 'Backup'
mkBackup :: Duration -> Editorial -> Backup
mkBackup a b = Backup a b

-- | @bar-style-color@ /(complex)/
--
-- The bar-style-color type contains barline style and color information.
data BarStyleColor = 
      BarStyleColor {
          barStyleColorBarStyle :: BarStyle -- ^ text content
        , barStyleColorColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BarStyleColor where
    emitXml (BarStyleColor a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "color" Nothing).emitXml) b])
        []
parseBarStyleColor :: P.XParse BarStyleColor
parseBarStyleColor = 
      BarStyleColor
        <$> (P.xtext >>= parseBarStyle)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'BarStyleColor'
mkBarStyleColor :: BarStyle -> BarStyleColor
mkBarStyleColor a = BarStyleColor a Nothing

-- | @barline@ /(complex)/
--
-- If a barline is other than a normal single barline, it should be represented by a barline type that describes it. This includes information about repeats and multiple endings, as well as line style. Barline data is on the same level as the other musical data in a score - a child of a measure in a partwise score, or a part in a timewise score. This allows for barlines within measures, as in dotted barlines that subdivide measures in complex meters. The two fermata elements allow for fermatas on both sides of the barline (the lower one inverted).
-- 	
-- Barlines have a location attribute to make it easier to process barlines independently of the other musical data in a score. It is often easier to set up measures separately from entering notes. The location attribute must match where the barline element occurs within the rest of the musical data in the score. If location is left, it should be the first element in the measure, aside from the print, bookmark, and link elements. If location is right, it should be the last element, again with the possible exception of the print, bookmark, and link elements. If no location is specified, the right barline is the default. The segno, coda, and divisions attributes work the same way as in the sound element. They are used for playback when barline elements contain segno or coda child elements.
data Barline = 
      Barline {
          barlineLocation :: (Maybe RightLeftMiddle) -- ^ /location/ attribute
        , barlineSegno :: (Maybe Token) -- ^ /segno/ attribute
        , barlineCoda :: (Maybe Token) -- ^ /coda/ attribute
        , barlineDivisions :: (Maybe Divisions) -- ^ /divisions/ attribute
        , barlineBarStyle :: (Maybe BarStyleColor) -- ^ /bar-style/ child element
        , barlineEditorial :: Editorial
        , barlineWavyLine :: (Maybe WavyLine) -- ^ /wavy-line/ child element
        , barlineSegno1 :: (Maybe EmptyPrintStyle) -- ^ /segno/ child element
        , barlineCoda1 :: (Maybe EmptyPrintStyle) -- ^ /coda/ child element
        , barlineFermata :: [Fermata] -- ^ /fermata/ child element
        , barlineEnding :: (Maybe Ending) -- ^ /ending/ child element
        , barlineRepeat :: (Maybe Repeat) -- ^ /repeat/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Barline where
    emitXml (Barline a b c d e f g h i j k l) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "segno" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "coda" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "divisions" Nothing).emitXml) d])
        ([maybe XEmpty (XElement (QN "bar-style" Nothing).emitXml) e]++[emitXml f]++[maybe XEmpty (XElement (QN "wavy-line" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "segno" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "coda" Nothing).emitXml) i]++map (XElement (QN "fermata" Nothing).emitXml) j++[maybe XEmpty (XElement (QN "ending" Nothing).emitXml) k]++[maybe XEmpty (XElement (QN "repeat" Nothing).emitXml) l])
parseBarline :: P.XParse Barline
parseBarline = 
      Barline
        <$> P.optional (P.xattr (P.name "location") >>= parseRightLeftMiddle)
        <*> P.optional (P.xattr (P.name "segno") >>= parseToken)
        <*> P.optional (P.xattr (P.name "coda") >>= parseToken)
        <*> P.optional (P.xattr (P.name "divisions") >>= parseDivisions)
        <*> P.optional (P.xchild (P.name "bar-style") (parseBarStyleColor))
        <*> parseEditorial
        <*> P.optional (P.xchild (P.name "wavy-line") (parseWavyLine))
        <*> P.optional (P.xchild (P.name "segno") (parseEmptyPrintStyle))
        <*> P.optional (P.xchild (P.name "coda") (parseEmptyPrintStyle))
        <*> P.many (P.xchild (P.name "fermata") (parseFermata))
        <*> P.optional (P.xchild (P.name "ending") (parseEnding))
        <*> P.optional (P.xchild (P.name "repeat") (parseRepeat))

-- | Smart constructor for 'Barline'
mkBarline :: Editorial -> Barline
mkBarline f = Barline Nothing Nothing Nothing Nothing Nothing f Nothing Nothing Nothing [] Nothing Nothing

-- | @barre@ /(complex)/
--
-- The barre element indicates placing a finger over multiple strings on a single fret. The type is "start" for the lowest pitched string (e.g., the string with the highest MusicXML number) and is "stop" for the highest pitched string.
data Barre = 
      Barre {
          barreType :: StartStop -- ^ /type/ attribute
        , barreColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Barre where
    emitXml (Barre a b) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) b])
        []
parseBarre :: P.XParse Barre
parseBarre = 
      Barre
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Barre'
mkBarre :: StartStop -> Barre
mkBarre a = Barre a Nothing

-- | @bass@ /(complex)/
--
-- The bass type is used to indicate a bass note in popular music chord symbols, e.g. G/C. It is generally not used in functional harmony, as inversion is generally not used in pop chord symbols. As with root, it is divided into step and alter elements, similar to pitches.
data Bass = 
      Bass {
          bassBassStep :: BassStep -- ^ /bass-step/ child element
        , bassBassAlter :: (Maybe BassAlter) -- ^ /bass-alter/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bass where
    emitXml (Bass a b) =
      XContent XEmpty
        []
        ([XElement (QN "bass-step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "bass-alter" Nothing).emitXml) b])
parseBass :: P.XParse Bass
parseBass = 
      Bass
        <$> (P.xchild (P.name "bass-step") (parseBassStep))
        <*> P.optional (P.xchild (P.name "bass-alter") (parseBassAlter))

-- | Smart constructor for 'Bass'
mkBass :: BassStep -> Bass
mkBass a = Bass a Nothing

-- | @bass-alter@ /(complex)/
--
-- The bass-alter type represents the chromatic alteration of the bass of the current chord within the harmony element. In some chord styles, the text for the bass-step element may include bass-alter information. In that case, the print-object attribute of the bass-alter element can be set to no. The location attribute indicates whether the alteration should appear to the left or the right of the bass-step; it is right by default.
data BassAlter = 
      BassAlter {
          bassAlterSemitones :: Semitones -- ^ text content
        , bassAlterLocation :: (Maybe LeftRight) -- ^ /location/ attribute
        , bassAlterPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , bassAlterDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , bassAlterDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , bassAlterRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , bassAlterRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , bassAlterFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , bassAlterFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , bassAlterFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , bassAlterFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , bassAlterColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BassAlter where
    emitXml (BassAlter a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l])
        []
parseBassAlter :: P.XParse BassAlter
parseBassAlter = 
      BassAlter
        <$> (P.xtext >>= parseSemitones)
        <*> P.optional (P.xattr (P.name "location") >>= parseLeftRight)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'BassAlter'
mkBassAlter :: Semitones -> BassAlter
mkBassAlter a = BassAlter a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @bass-step@ /(complex)/
--
-- The bass-step type represents the pitch step of the bass of the current chord within the harmony element. The text attribute indicates how the bass should appear on the page if not using the element contents.
data BassStep = 
      BassStep {
          bassStepStep :: Step -- ^ text content
        , bassStepText :: (Maybe Token) -- ^ /text/ attribute
        , bassStepDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , bassStepDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , bassStepRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , bassStepRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , bassStepFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , bassStepFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , bassStepFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , bassStepFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , bassStepColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BassStep where
    emitXml (BassStep a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseBassStep :: P.XParse BassStep
parseBassStep = 
      BassStep
        <$> (P.xtext >>= parseStep)
        <*> P.optional (P.xattr (P.name "text") >>= parseToken)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'BassStep'
mkBassStep :: Step -> BassStep
mkBassStep a = BassStep a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @beam@ /(complex)/
--
-- Beam values include begin, continue, end, forward hook, and backward hook. Up to six concurrent beam levels are available to cover up to 256th notes. The repeater attribute, used for tremolos, needs to be specified with a "yes" value for each beam using it. Beams that have a begin value can also have a fan attribute to indicate accelerandos and ritardandos using fanned beams. The fan attribute may also be used with a continue value if the fanning direction changes on that note. The value is "none" if not specified.
-- 	
-- Note that the beam number does not distinguish sets of beams that overlap, as it does for slur and other elements. Beaming groups are distinguished by being in different voices and/or the presence or absence of grace and cue elements.
data Beam = 
      Beam {
          beamBeamValue :: BeamValue -- ^ text content
        , beamNumber :: (Maybe BeamLevel) -- ^ /number/ attribute
        , beamRepeater :: (Maybe YesNo) -- ^ /repeater/ attribute
        , beamFan :: (Maybe Fan) -- ^ /fan/ attribute
        , beamColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Beam where
    emitXml (Beam a b c d e) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "repeater" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "fan" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) e])
        []
parseBeam :: P.XParse Beam
parseBeam = 
      Beam
        <$> (P.xtext >>= parseBeamValue)
        <*> P.optional (P.xattr (P.name "number") >>= parseBeamLevel)
        <*> P.optional (P.xattr (P.name "repeater") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "fan") >>= parseFan)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Beam'
mkBeam :: BeamValue -> Beam
mkBeam a = Beam a Nothing Nothing Nothing Nothing

-- | @beat-repeat@ /(complex)/
--
-- The beat-repeat type is used to indicate that a single beat (but possibly many notes) is repeated. Both the start and stop of the beat being repeated should be specified. The slashes attribute specifies the number of slashes to use in the symbol. The use-dots attribute indicates whether or not to use dots as well (for instance, with mixed rhythm patterns). By default, the value for slashes is 1 and the value for use-dots is no.
-- 	
-- The beat-repeat element specifies a notation style for repetitions. The actual music being repeated needs to be repeated within the MusicXML file. This element specifies the notation that indicates the repeat.
data BeatRepeat = 
      BeatRepeat {
          beatRepeatType :: StartStop -- ^ /type/ attribute
        , beatRepeatSlashes :: (Maybe PositiveInteger) -- ^ /slashes/ attribute
        , beatRepeatUseDots :: (Maybe YesNo) -- ^ /use-dots/ attribute
        , beatRepeatSlash :: (Maybe Slash)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BeatRepeat where
    emitXml (BeatRepeat a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "slashes" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "use-dots" Nothing).emitXml) c])
        ([emitXml d])
parseBeatRepeat :: P.XParse BeatRepeat
parseBeatRepeat = 
      BeatRepeat
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "slashes") >>= parsePositiveInteger)
        <*> P.optional (P.xattr (P.name "use-dots") >>= parseYesNo)
        <*> P.optional (parseSlash)

-- | Smart constructor for 'BeatRepeat'
mkBeatRepeat :: StartStop -> BeatRepeat
mkBeatRepeat a = BeatRepeat a Nothing Nothing Nothing

-- | @bend@ /(complex)/
--
-- The bend type is used in guitar and tablature. The bend-alter element indicates the number of steps in the bend, similar to the alter element. As with the alter element, numbers like 0.5 can be used to indicate microtones. Negative numbers indicate pre-bends or releases; the pre-bend and release elements are used to distinguish what is intended. A with-bar element indicates that the bend is to be done at the bridge with a whammy or vibrato bar. The content of the element indicates how this should be notated.
data Bend = 
      Bend {
          bendDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , bendDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , bendRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , bendRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , bendFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , bendFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , bendFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , bendFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , bendColor :: (Maybe Color) -- ^ /color/ attribute
        , bendAccelerate :: (Maybe YesNo) -- ^ /accelerate/ attribute
        , bendBeats :: (Maybe TrillBeats) -- ^ /beats/ attribute
        , bendFirstBeat :: (Maybe Percent) -- ^ /first-beat/ attribute
        , bendLastBeat :: (Maybe Percent) -- ^ /last-beat/ attribute
        , bendBendAlter :: Semitones -- ^ /bend-alter/ child element
        , bendBend :: (Maybe ChxBend)
        , bendWithBar :: (Maybe PlacementText) -- ^ /with-bar/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bend where
    emitXml (Bend a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "first-beat" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) m])
        ([XElement (QN "bend-alter" Nothing) (emitXml n)]++[emitXml o]++[maybe XEmpty (XElement (QN "with-bar" Nothing).emitXml) p])
parseBend :: P.XParse Bend
parseBend = 
      Bend
        <$> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "accelerate") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "beats") >>= parseTrillBeats)
        <*> P.optional (P.xattr (P.name "first-beat") >>= parsePercent)
        <*> P.optional (P.xattr (P.name "last-beat") >>= parsePercent)
        <*> (P.xchild (P.name "bend-alter") (P.xtext >>= parseSemitones))
        <*> P.optional (parseChxBend)
        <*> P.optional (P.xchild (P.name "with-bar") (parsePlacementText))

-- | Smart constructor for 'Bend'
mkBend :: Semitones -> Bend
mkBend n = Bend Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing n Nothing Nothing

-- | @bookmark@ /(complex)/
--
-- The bookmark type serves as a well-defined target for an incoming simple XLink.
data Bookmark = 
      Bookmark {
          bookmarkId :: ID -- ^ /id/ attribute
        , bookmarkName :: (Maybe Token) -- ^ /name/ attribute
        , bookmarkElement :: (Maybe NMTOKEN) -- ^ /element/ attribute
        , bookmarkPosition :: (Maybe PositiveInteger) -- ^ /position/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bookmark where
    emitXml (Bookmark a b c d) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "element" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "position" Nothing).emitXml) d])
        []
parseBookmark :: P.XParse Bookmark
parseBookmark = 
      Bookmark
        <$> (P.xattr (P.name "id") >>= parseID)
        <*> P.optional (P.xattr (P.name "name") >>= parseToken)
        <*> P.optional (P.xattr (P.name "element") >>= parseNMTOKEN)
        <*> P.optional (P.xattr (P.name "position") >>= parsePositiveInteger)

-- | Smart constructor for 'Bookmark'
mkBookmark :: ID -> Bookmark
mkBookmark a = Bookmark a Nothing Nothing Nothing

-- | @bracket@ /(complex)/
--
-- Brackets are combined with words in a variety of modern directions. The line-end attribute specifies if there is a jog up or down (or both), an arrow, or nothing at the start or end of the bracket. If the line-end is up or down, the length of the jog can be specified using the end-length attribute. The line-type is solid by default.
data Bracket = 
      Bracket {
          bracketType :: StartStop -- ^ /type/ attribute
        , bracketNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , bracketLineEnd :: LineEnd -- ^ /line-end/ attribute
        , bracketEndLength :: (Maybe Tenths) -- ^ /end-length/ attribute
        , bracketLineType :: (Maybe LineType) -- ^ /line-type/ attribute
        , bracketDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , bracketDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , bracketRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , bracketRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , bracketColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bracket where
    emitXml (Bracket a b c d e f g h i j) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[XAttr (QN "line-end" Nothing) (emitXml c)]++[maybe XEmpty (XAttr (QN "end-length" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
parseBracket :: P.XParse Bracket
parseBracket = 
      Bracket
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> (P.xattr (P.name "line-end") >>= parseLineEnd)
        <*> P.optional (P.xattr (P.name "end-length") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "line-type") >>= parseLineType)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Bracket'
mkBracket :: StartStop -> LineEnd -> Bracket
mkBracket a c = Bracket a Nothing c Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @cancel@ /(complex)/
--
-- A cancel element indicates that the old key signature should be cancelled before the new one appears. This will always happen when changing to C major or A minor and need not be specified then. The cancel value matches the fifths value of the cancelled key signature (e.g., a cancel of -2 will provide an explicit cancellation for changing from B flat major to F major). The optional location attribute indicates whether the cancellation appears to the left or the right of the new key signature. It is left by default.
data Cancel = 
      Cancel {
          cancelFifths :: Fifths -- ^ text content
        , cancelLocation :: (Maybe LeftRight) -- ^ /location/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Cancel where
    emitXml (Cancel a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) b])
        []
parseCancel :: P.XParse Cancel
parseCancel = 
      Cancel
        <$> (P.xtext >>= parseFifths)
        <*> P.optional (P.xattr (P.name "location") >>= parseLeftRight)

-- | Smart constructor for 'Cancel'
mkCancel :: Fifths -> Cancel
mkCancel a = Cancel a Nothing

-- | @clef@ /(complex)/
--
-- Clefs are represented by a combination of sign, line, and clef-octave-change elements. The optional number attribute refers to staff numbers within the part. A value of 1 is assumed if not present.
-- 
-- Sometimes clefs are added to the staff in non-standard line positions, either to indicate cue passages, or when there are multiple clefs present simultaneously on one staff. In this situation, the additional attribute is set to "yes" and the line value is ignored. The size attribute is used for clefs where the additional attribute is "yes". It is typically used to indicate cue clefs.
data Clef = 
      Clef {
          clefNumber :: (Maybe StaffNumber) -- ^ /number/ attribute
        , clefAdditional :: (Maybe YesNo) -- ^ /additional/ attribute
        , clefSize :: (Maybe SymbolSize) -- ^ /size/ attribute
        , clefDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , clefDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , clefRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , clefRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , clefFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , clefFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , clefFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , clefFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , clefColor :: (Maybe Color) -- ^ /color/ attribute
        , clefPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , clefSign :: ClefSign -- ^ /sign/ child element
        , clefLine :: (Maybe StaffLine) -- ^ /line/ child element
        , clefClefOctaveChange :: (Maybe Int) -- ^ /clef-octave-change/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Clef where
    emitXml (Clef a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "additional" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) m])
        ([XElement (QN "sign" Nothing) (emitXml n)]++[maybe XEmpty (XElement (QN "line" Nothing).emitXml) o]++[maybe XEmpty (XElement (QN "clef-octave-change" Nothing).emitXml) p])
parseClef :: P.XParse Clef
parseClef = 
      Clef
        <$> P.optional (P.xattr (P.name "number") >>= parseStaffNumber)
        <*> P.optional (P.xattr (P.name "additional") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "size") >>= parseSymbolSize)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> (P.xchild (P.name "sign") (P.xtext >>= parseClefSign))
        <*> P.optional (P.xchild (P.name "line") (P.xtext >>= parseStaffLine))
        <*> P.optional (P.xchild (P.name "clef-octave-change") (P.xtext >>= (P.xread "Integer")))

-- | Smart constructor for 'Clef'
mkClef :: ClefSign -> Clef
mkClef n = Clef Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing n Nothing Nothing

-- | @credit@ /(complex)/
--
-- The credit type represents the appearance of the title, composer, arranger, lyricist, copyright, dedication, and other text and graphics that commonly appears on the first page of a score. The credit-words and credit-image elements are similar to the words and image elements for directions. However, since the credit is not part of a measure, the default-x and default-y attributes adjust the origin relative to the bottom left-hand corner of the first page. The enclosure for credit-words is none by default.
-- 	
-- By default, a series of credit-words elements within a single credit element follow one another in sequence visually. Non-positional formatting attributes are carried over from the previous element by default.
-- 	
-- The page attribute for the credit element, new in Version 2.0, specifies the page number where the credit should appear. This is an integer value that starts with 1 for the first page. Its value is 1 by default. Since credits occur before the music, these page numbers do not refer to the page numbering specified by the print element's page-number attribute.
data Credit = 
      Credit {
          creditPage :: (Maybe PositiveInteger) -- ^ /page/ attribute
        , creditLink :: [Link] -- ^ /link/ child element
        , creditBookmark :: [Bookmark] -- ^ /bookmark/ child element
        , creditCredit :: ChxCredit
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Credit where
    emitXml (Credit a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "page" Nothing).emitXml) a])
        (map (XElement (QN "link" Nothing).emitXml) b++map (XElement (QN "bookmark" Nothing).emitXml) c++[emitXml d])
parseCredit :: P.XParse Credit
parseCredit = 
      Credit
        <$> P.optional (P.xattr (P.name "page") >>= parsePositiveInteger)
        <*> P.many (P.xchild (P.name "link") (parseLink))
        <*> P.many (P.xchild (P.name "bookmark") (parseBookmark))
        <*> parseChxCredit

-- | Smart constructor for 'Credit'
mkCredit :: ChxCredit -> Credit
mkCredit d = Credit Nothing [] [] d

-- | @dashes@ /(complex)/
--
-- The dashes type represents dashes, used for instance with cresc. and dim. marks.
data Dashes = 
      Dashes {
          dashesType :: StartStop -- ^ /type/ attribute
        , dashesNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , dashesDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , dashesDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , dashesRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , dashesRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , dashesColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Dashes where
    emitXml (Dashes a b c d e f g) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) g])
        []
parseDashes :: P.XParse Dashes
parseDashes = 
      Dashes
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Dashes'
mkDashes :: StartStop -> Dashes
mkDashes a = Dashes a Nothing Nothing Nothing Nothing Nothing Nothing

-- | @defaults@ /(complex)/
--
-- The defaults type specifies score-wide defaults for scaling, layout, and appearance.
data Defaults = 
      Defaults {
          defaultsScaling :: (Maybe Scaling) -- ^ /scaling/ child element
        , defaultsLayout :: Layout
        , defaultsAppearance :: (Maybe Appearance) -- ^ /appearance/ child element
        , defaultsMusicFont :: (Maybe EmptyFont) -- ^ /music-font/ child element
        , defaultsWordFont :: (Maybe EmptyFont) -- ^ /word-font/ child element
        , defaultsLyricFont :: [LyricFont] -- ^ /lyric-font/ child element
        , defaultsLyricLanguage :: [LyricLanguage] -- ^ /lyric-language/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Defaults where
    emitXml (Defaults a b c d e f g) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "scaling" Nothing).emitXml) a]++[emitXml b]++[maybe XEmpty (XElement (QN "appearance" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "music-font" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "word-font" Nothing).emitXml) e]++map (XElement (QN "lyric-font" Nothing).emitXml) f++map (XElement (QN "lyric-language" Nothing).emitXml) g)
parseDefaults :: P.XParse Defaults
parseDefaults = 
      Defaults
        <$> P.optional (P.xchild (P.name "scaling") (parseScaling))
        <*> parseLayout
        <*> P.optional (P.xchild (P.name "appearance") (parseAppearance))
        <*> P.optional (P.xchild (P.name "music-font") (parseEmptyFont))
        <*> P.optional (P.xchild (P.name "word-font") (parseEmptyFont))
        <*> P.many (P.xchild (P.name "lyric-font") (parseLyricFont))
        <*> P.many (P.xchild (P.name "lyric-language") (parseLyricLanguage))

-- | Smart constructor for 'Defaults'
mkDefaults :: Layout -> Defaults
mkDefaults b = Defaults Nothing b Nothing Nothing Nothing [] []

-- | @degree@ /(complex)/
--
-- The degree type is used to add, alter, or subtract individual notes in the chord. The print-object attribute can be used to keep the degree from printing separately when it has already taken into account in the text attribute of the kind element. The degree-value and degree-type text attributes specify how the value and type of the degree should be displayed.
-- 	
-- A harmony of kind "other" can be spelled explicitly by using a series of degree elements together with a root.
data Degree = 
      Degree {
          degreePrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , degreeDegreeValue :: DegreeValue -- ^ /degree-value/ child element
        , degreeDegreeAlter :: DegreeAlter -- ^ /degree-alter/ child element
        , degreeDegreeType :: DegreeType -- ^ /degree-type/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Degree where
    emitXml (Degree a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) a])
        ([XElement (QN "degree-value" Nothing) (emitXml b)]++[XElement (QN "degree-alter" Nothing) (emitXml c)]++[XElement (QN "degree-type" Nothing) (emitXml d)])
parseDegree :: P.XParse Degree
parseDegree = 
      Degree
        <$> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> (P.xchild (P.name "degree-value") (parseDegreeValue))
        <*> (P.xchild (P.name "degree-alter") (parseDegreeAlter))
        <*> (P.xchild (P.name "degree-type") (parseDegreeType))

-- | Smart constructor for 'Degree'
mkDegree :: DegreeValue -> DegreeAlter -> DegreeType -> Degree
mkDegree b c d = Degree Nothing b c d

-- | @degree-alter@ /(complex)/
--
-- The degree-alter type represents the chromatic alteration for the current degree. If the degree-type value is alter or subtract, the degree-alter value is relative to the degree already in the chord based on its kind element. If the degree-type value is add, the degree-alter is relative to a dominant chord (major and perfect intervals except for a minor seventh). The plus-minus attribute is used to indicate if plus and minus symbols should be used instead of sharp and flat symbols to display the degree alteration; it is no by default.
data DegreeAlter = 
      DegreeAlter {
          degreeAlterSemitones :: Semitones -- ^ text content
        , degreeAlterPlusMinus :: (Maybe YesNo) -- ^ /plus-minus/ attribute
        , degreeAlterDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , degreeAlterDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , degreeAlterRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , degreeAlterRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , degreeAlterFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , degreeAlterFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , degreeAlterFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , degreeAlterFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , degreeAlterColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DegreeAlter where
    emitXml (DegreeAlter a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "plus-minus" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseDegreeAlter :: P.XParse DegreeAlter
parseDegreeAlter = 
      DegreeAlter
        <$> (P.xtext >>= parseSemitones)
        <*> P.optional (P.xattr (P.name "plus-minus") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'DegreeAlter'
mkDegreeAlter :: Semitones -> DegreeAlter
mkDegreeAlter a = DegreeAlter a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @degree-type@ /(complex)/
--
-- The degree-type type indicates if this degree is an addition, alteration, or subtraction relative to the kind of the current chord. The value of the degree-type element affects the interpretation of the value of the degree-alter element. The text attribute specifies how the type of the degree should be displayed.
data DegreeType = 
      DegreeType {
          degreeTypeDegreeTypeValue :: DegreeTypeValue -- ^ text content
        , degreeTypeText :: (Maybe Token) -- ^ /text/ attribute
        , degreeTypeDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , degreeTypeDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , degreeTypeRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , degreeTypeRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , degreeTypeFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , degreeTypeFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , degreeTypeFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , degreeTypeFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , degreeTypeColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DegreeType where
    emitXml (DegreeType a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseDegreeType :: P.XParse DegreeType
parseDegreeType = 
      DegreeType
        <$> (P.xtext >>= parseDegreeTypeValue)
        <*> P.optional (P.xattr (P.name "text") >>= parseToken)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'DegreeType'
mkDegreeType :: DegreeTypeValue -> DegreeType
mkDegreeType a = DegreeType a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @degree-value@ /(complex)/
--
-- The content of the degree-value type is a number indicating the degree of the chord (1 for the root, 3 for third, etc). The text attribute specifies how the type of the degree should be displayed.
data DegreeValue = 
      DegreeValue {
          degreeValuePositiveInteger :: PositiveInteger -- ^ text content
        , degreeValueText :: (Maybe Token) -- ^ /text/ attribute
        , degreeValueDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , degreeValueDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , degreeValueRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , degreeValueRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , degreeValueFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , degreeValueFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , degreeValueFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , degreeValueFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , degreeValueColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DegreeValue where
    emitXml (DegreeValue a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseDegreeValue :: P.XParse DegreeValue
parseDegreeValue = 
      DegreeValue
        <$> (P.xtext >>= parsePositiveInteger)
        <*> P.optional (P.xattr (P.name "text") >>= parseToken)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'DegreeValue'
mkDegreeValue :: PositiveInteger -> DegreeValue
mkDegreeValue a = DegreeValue a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @direction@ /(complex)/
--
-- A direction is a musical indication that is not attached to a specific note. Two or more may be combined to indicate starts and stops of wedges, dashes, etc.
-- 	
-- By default, a series of direction-type elements and a series of child elements of a direction-type within a single direction element follow one another in sequence visually. For a series of direction-type children, non-positional formatting attributes are carried over from the previous element by default.
data Direction = 
      Direction {
          directionPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , directionDirective :: (Maybe YesNo) -- ^ /directive/ attribute
        , directionDirectionType :: [DirectionType] -- ^ /direction-type/ child element
        , directionOffset :: (Maybe Offset) -- ^ /offset/ child element
        , directionEditorialVoiceDirection :: EditorialVoiceDirection
        , directionStaff :: (Maybe Staff)
        , directionSound :: (Maybe Sound) -- ^ /sound/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Direction where
    emitXml (Direction a b c d e f g) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "directive" Nothing).emitXml) b])
        (map (XElement (QN "direction-type" Nothing).emitXml) c++[maybe XEmpty (XElement (QN "offset" Nothing).emitXml) d]++[emitXml e]++[emitXml f]++[maybe XEmpty (XElement (QN "sound" Nothing).emitXml) g])
parseDirection :: P.XParse Direction
parseDirection = 
      Direction
        <$> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "directive") >>= parseYesNo)
        <*> P.many (P.xchild (P.name "direction-type") (parseDirectionType))
        <*> P.optional (P.xchild (P.name "offset") (parseOffset))
        <*> parseEditorialVoiceDirection
        <*> P.optional (parseStaff)
        <*> P.optional (P.xchild (P.name "sound") (parseSound))

-- | Smart constructor for 'Direction'
mkDirection :: EditorialVoiceDirection -> Direction
mkDirection e = Direction Nothing Nothing [] Nothing e Nothing Nothing

-- | @direction-type@ /(complex)/
--
-- Textual direction types may have more than 1 component due to multiple fonts. The dynamics element may also be used in the notations element. Attribute groups related to print suggestions apply to the individual direction-type, not to the overall direction.
data DirectionType = 
      DirectionType {
          directionTypeDirectionType :: ChxDirectionType
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DirectionType where
    emitXml (DirectionType a) =
      XReps [emitXml a]
parseDirectionType :: P.XParse DirectionType
parseDirectionType = 
      DirectionType
        <$> parseChxDirectionType

-- | Smart constructor for 'DirectionType'
mkDirectionType :: ChxDirectionType -> DirectionType
mkDirectionType a = DirectionType a

-- | @directive@ /(complex)/
data Directive = 
      Directive {
          directiveString :: String -- ^ text content
        , directiveLang :: (Maybe Lang) -- ^ /xml:lang/ attribute
        , directiveDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , directiveDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , directiveRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , directiveRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , directiveFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , directiveFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , directiveFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , directiveFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , directiveColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Directive where
    emitXml (Directive a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseDirective :: P.XParse Directive
parseDirective = 
      Directive
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "xml:lang") >>= parseLang)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Directive'
mkDirective :: String -> Directive
mkDirective a = Directive a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @display-step-octave@ /(complex)/
--
-- The display-step-octave type contains the sequence of elements used by both the rest and unpitched elements. This group is used to place rests and unpitched elements on the staff without implying that these elements have pitch. Positioning follows the current clef. If percussion clef is used, the display-step and display-octave elements are interpreted as if in treble clef, with a G in octave 4 on line 2. If not present, the note is placed on the middle line of the staff, generally used for one-line staffs.
data DisplayStepOctave = 
      DisplayStepOctave {
          displayStepOctaveDisplayStepOctave :: (Maybe SeqDisplayStepOctave)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DisplayStepOctave where
    emitXml (DisplayStepOctave a) =
      XReps [emitXml a]
parseDisplayStepOctave :: P.XParse DisplayStepOctave
parseDisplayStepOctave = 
      DisplayStepOctave
        <$> P.optional (parseSeqDisplayStepOctave)

-- | Smart constructor for 'DisplayStepOctave'
mkDisplayStepOctave :: DisplayStepOctave
mkDisplayStepOctave = DisplayStepOctave Nothing

-- | @dynamics@ /(complex)/
--
-- Dynamics can be associated either with a note or a general musical direction. To avoid inconsistencies between and amongst the letter abbreviations for dynamics (what is sf vs. sfz, standing alone or with a trailing dynamic that is not always piano), we use the actual letters as the names of these dynamic elements. The other-dynamics element allows other dynamic marks that are not covered here, but many of those should perhaps be included in a more general musical direction element. Dynamics elements may also be combined to create marks not covered by a single element, such as sfmp.
-- 	
-- These letter dynamic symbols are separated from crescendo, decrescendo, and wedge indications. Dynamic representation is inconsistent in scores. Many things are assumed by the composer and left out, such as returns to original dynamics. Systematic representations are quite complex: for example, Humdrum has at least 3 representation formats related to dynamics. The MusicXML format captures what is in the score, but does not try to be optimal for analysis or synthesis of dynamics.
data Dynamics = 
      Dynamics {
          dynamicsDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , dynamicsDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , dynamicsRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , dynamicsRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , dynamicsFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , dynamicsFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , dynamicsFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , dynamicsFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , dynamicsColor :: (Maybe Color) -- ^ /color/ attribute
        , dynamicsPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , dynamicsDynamics :: [ChxDynamics]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Dynamics where
    emitXml (Dynamics a b c d e f g h i j k) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) j])
        ([emitXml k])
parseDynamics :: P.XParse Dynamics
parseDynamics = 
      Dynamics
        <$> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.many (parseChxDynamics)

-- | Smart constructor for 'Dynamics'
mkDynamics :: Dynamics
mkDynamics = Dynamics Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

-- | @elision@ /(complex)/
--
-- In Version 2.0, the content of the elision type is used to specify the symbol used to display the elision. Common values are a no-break space (Unicode 00A0), an underscore (Unicode 005F), or an undertie (Unicode 203F).
data Elision = 
      Elision {
          elisionString :: String -- ^ text content
        , elisionFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , elisionFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , elisionFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , elisionFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , elisionColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Elision where
    emitXml (Elision a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
parseElision :: P.XParse Elision
parseElision = 
      Elision
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Elision'
mkElision :: String -> Elision
mkElision a = Elision a Nothing Nothing Nothing Nothing Nothing

-- | @empty@ /(complex)/
--
-- The empty type represents an empty element with no attributes.
data Empty = 
      Empty
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Empty where
    emitXml (Empty) =
      XReps []
parseEmpty :: P.XParse Empty
parseEmpty = 
      return Empty

-- | Smart constructor for 'Empty'
mkEmpty :: Empty
mkEmpty = Empty 

-- | @empty-font@ /(complex)/
--
-- The empty-font type represents an empty element with font attributes.
data EmptyFont = 
      EmptyFont {
          emptyFontFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , emptyFontFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , emptyFontFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , emptyFontFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyFont where
    emitXml (EmptyFont a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) d])
        []
parseEmptyFont :: P.XParse EmptyFont
parseEmptyFont = 
      EmptyFont
        <$> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)

-- | Smart constructor for 'EmptyFont'
mkEmptyFont :: EmptyFont
mkEmptyFont = EmptyFont Nothing Nothing Nothing Nothing

-- | @empty-line@ /(complex)/
--
-- The empty-line type represents an empty element with line-shape, line-type, print-style and placement attributes.
data EmptyLine = 
      EmptyLine {
          emptyLineLineShape :: (Maybe LineShape) -- ^ /line-shape/ attribute
        , emptyLineLineType :: (Maybe LineType) -- ^ /line-type/ attribute
        , emptyLineDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , emptyLineDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , emptyLineRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , emptyLineRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , emptyLineFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , emptyLineFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , emptyLineFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , emptyLineFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , emptyLineColor :: (Maybe Color) -- ^ /color/ attribute
        , emptyLinePlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyLine where
    emitXml (EmptyLine a b c d e f g h i j k l) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "line-shape" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) l])
        []
parseEmptyLine :: P.XParse EmptyLine
parseEmptyLine = 
      EmptyLine
        <$> P.optional (P.xattr (P.name "line-shape") >>= parseLineShape)
        <*> P.optional (P.xattr (P.name "line-type") >>= parseLineType)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'EmptyLine'
mkEmptyLine :: EmptyLine
mkEmptyLine = EmptyLine Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @empty-placement@ /(complex)/
--
-- The empty-placement type represents an empty element with print-style and placement attributes.
data EmptyPlacement = 
      EmptyPlacement {
          emptyPlacementDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , emptyPlacementDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , emptyPlacementRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , emptyPlacementRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , emptyPlacementFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , emptyPlacementFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , emptyPlacementFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , emptyPlacementFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , emptyPlacementColor :: (Maybe Color) -- ^ /color/ attribute
        , emptyPlacementPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyPlacement where
    emitXml (EmptyPlacement a b c d e f g h i j) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) j])
        []
parseEmptyPlacement :: P.XParse EmptyPlacement
parseEmptyPlacement = 
      EmptyPlacement
        <$> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'EmptyPlacement'
mkEmptyPlacement :: EmptyPlacement
mkEmptyPlacement = EmptyPlacement Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @empty-print-style@ /(complex)/
--
-- The empty-print-style type represents an empty element with print-style attributes.
data EmptyPrintStyle = 
      EmptyPrintStyle {
          emptyPrintStyleDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , emptyPrintStyleDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , emptyPrintStyleRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , emptyPrintStyleRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , emptyPrintStyleFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , emptyPrintStyleFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , emptyPrintStyleFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , emptyPrintStyleFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , emptyPrintStyleColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyPrintStyle where
    emitXml (EmptyPrintStyle a b c d e f g h i) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        []
parseEmptyPrintStyle :: P.XParse EmptyPrintStyle
parseEmptyPrintStyle = 
      EmptyPrintStyle
        <$> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'EmptyPrintStyle'
mkEmptyPrintStyle :: EmptyPrintStyle
mkEmptyPrintStyle = EmptyPrintStyle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @empty-trill-sound@ /(complex)/
--
-- The empty-trill-sound type represents an empty element with print-style, placement, and trill-sound attributes.
data EmptyTrillSound = 
      EmptyTrillSound {
          emptyTrillSoundDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , emptyTrillSoundDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , emptyTrillSoundRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , emptyTrillSoundRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , emptyTrillSoundFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , emptyTrillSoundFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , emptyTrillSoundFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , emptyTrillSoundFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , emptyTrillSoundColor :: (Maybe Color) -- ^ /color/ attribute
        , emptyTrillSoundPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , emptyTrillSoundStartNote :: (Maybe StartNote) -- ^ /start-note/ attribute
        , emptyTrillSoundTrillStep :: (Maybe TrillStep) -- ^ /trill-step/ attribute
        , emptyTrillSoundTwoNoteTurn :: (Maybe TwoNoteTurn) -- ^ /two-note-turn/ attribute
        , emptyTrillSoundAccelerate :: (Maybe YesNo) -- ^ /accelerate/ attribute
        , emptyTrillSoundBeats :: (Maybe TrillBeats) -- ^ /beats/ attribute
        , emptyTrillSoundSecondBeat :: (Maybe Percent) -- ^ /second-beat/ attribute
        , emptyTrillSoundLastBeat :: (Maybe Percent) -- ^ /last-beat/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyTrillSound where
    emitXml (EmptyTrillSound a b c d e f g h i j k l m n o p q) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "start-note" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "trill-step" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "two-note-turn" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "second-beat" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) q])
        []
parseEmptyTrillSound :: P.XParse EmptyTrillSound
parseEmptyTrillSound = 
      EmptyTrillSound
        <$> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "start-note") >>= parseStartNote)
        <*> P.optional (P.xattr (P.name "trill-step") >>= parseTrillStep)
        <*> P.optional (P.xattr (P.name "two-note-turn") >>= parseTwoNoteTurn)
        <*> P.optional (P.xattr (P.name "accelerate") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "beats") >>= parseTrillBeats)
        <*> P.optional (P.xattr (P.name "second-beat") >>= parsePercent)
        <*> P.optional (P.xattr (P.name "last-beat") >>= parsePercent)

-- | Smart constructor for 'EmptyTrillSound'
mkEmptyTrillSound :: EmptyTrillSound
mkEmptyTrillSound = EmptyTrillSound Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @encoding@ /(complex)/
--
-- The encoding element contains information about who did the digital encoding, when, with what software, and in what aspects. Standard type values for the encoder element are music, words, and arrangement, but other types may be used. The type attribute is only needed when there are multiple encoder elements.
data Encoding = 
      Encoding {
          encodingEncoding :: [ChxEncoding]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Encoding where
    emitXml (Encoding a) =
      XReps [emitXml a]
parseEncoding :: P.XParse Encoding
parseEncoding = 
      Encoding
        <$> P.many (parseChxEncoding)

-- | Smart constructor for 'Encoding'
mkEncoding :: Encoding
mkEncoding = Encoding []

-- | @ending@ /(complex)/
--
-- The ending type represents multiple (e.g. first and second) endings. Typically, the start type is associated with the left barline of the first measure in an ending. The stop and discontinue types are associated with the right barline of the last measure in an ending. Stop is used when the ending mark concludes with a downward jog, as is typical for first endings. Discontinue is used when there is no downward jog, as is typical for second endings that do not conclude a piece. The length of the jog can be specified using the end-length attribute. The text-x and text-y attributes are offsets that specify where the baseline of the start of the ending text appears, relative to the start of the ending line.
-- 	
-- The number attribute reflects the numeric values of what is under the ending line. Single endings such as "1" or comma-separated multiple endings such as "1,2" may be used. The ending element text is used when the text displayed in the ending is different than what appears in the number attribute. The print-object element is used to indicate when an ending is present but not printed, as is often the case for many parts in a full score.
data Ending = 
      Ending {
          endingString :: String -- ^ text content
        , cmpendingNumber :: EndingNumber -- ^ /number/ attribute
        , endingType :: StartStopDiscontinue -- ^ /type/ attribute
        , endingEndLength :: (Maybe Tenths) -- ^ /end-length/ attribute
        , endingTextX :: (Maybe Tenths) -- ^ /text-x/ attribute
        , endingTextY :: (Maybe Tenths) -- ^ /text-y/ attribute
        , endingPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , endingDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , endingDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , endingRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , endingRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , endingFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , endingFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , endingFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , endingFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , endingColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Ending where
    emitXml (Ending a b c d e f g h i j k l m n o p) =
      XContent (emitXml a)
        ([XAttr (QN "number" Nothing) (emitXml b)]++[XAttr (QN "type" Nothing) (emitXml c)]++[maybe XEmpty (XAttr (QN "end-length" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "text-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "text-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) p])
        []
parseEnding :: P.XParse Ending
parseEnding = 
      Ending
        <$> (P.xtext >>= return)
        <*> (P.xattr (P.name "number") >>= parseEndingNumber)
        <*> (P.xattr (P.name "type") >>= parseStartStopDiscontinue)
        <*> P.optional (P.xattr (P.name "end-length") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "text-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "text-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Ending'
mkEnding :: String -> EndingNumber -> StartStopDiscontinue -> Ending
mkEnding a b c = Ending a b c Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @extend@ /(complex)/
--
-- The extend type represents word extensions for lyrics.
data Extend = 
      Extend {
          extendFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , extendFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , extendFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , extendFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , extendColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Extend where
    emitXml (Extend a b c d e) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) e])
        []
parseExtend :: P.XParse Extend
parseExtend = 
      Extend
        <$> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Extend'
mkExtend :: Extend
mkExtend = Extend Nothing Nothing Nothing Nothing Nothing

-- | @feature@ /(complex)/
--
-- The feature type is a part of the grouping element used for musical analysis. The type attribute represents the type of the feature and the element content represents its value. This type is flexible to allow for different analyses.
data Feature = 
      Feature {
          featureString :: String -- ^ text content
        , featureType :: (Maybe Token) -- ^ /type/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Feature where
    emitXml (Feature a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b])
        []
parseFeature :: P.XParse Feature
parseFeature = 
      Feature
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "type") >>= parseToken)

-- | Smart constructor for 'Feature'
mkFeature :: String -> Feature
mkFeature a = Feature a Nothing

-- | @fermata@ /(complex)/
--
-- The fermata text content represents the shape of the fermata sign. An empty fermata element represents a normal fermata. The fermata type is upright if not specified.
data Fermata = 
      Fermata {
          fermataFermataShape :: FermataShape -- ^ text content
        , fermataType :: (Maybe UprightInverted) -- ^ /type/ attribute
        , fermataDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , fermataDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , fermataRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , fermataRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , fermataFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , fermataFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , fermataFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , fermataFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , fermataColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Fermata where
    emitXml (Fermata a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseFermata :: P.XParse Fermata
parseFermata = 
      Fermata
        <$> (P.xtext >>= parseFermataShape)
        <*> P.optional (P.xattr (P.name "type") >>= parseUprightInverted)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Fermata'
mkFermata :: FermataShape -> Fermata
mkFermata a = Fermata a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @figure@ /(complex)/
--
-- The figure type represents a single figure within a figured-bass element.
data Figure = 
      Figure {
          figurePrefix :: (Maybe StyleText) -- ^ /prefix/ child element
        , figureFigureNumber :: (Maybe StyleText) -- ^ /figure-number/ child element
        , figureSuffix :: (Maybe StyleText) -- ^ /suffix/ child element
        , figureExtend :: (Maybe Extend) -- ^ /extend/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Figure where
    emitXml (Figure a b c d) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "prefix" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "figure-number" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "suffix" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "extend" Nothing).emitXml) d])
parseFigure :: P.XParse Figure
parseFigure = 
      Figure
        <$> P.optional (P.xchild (P.name "prefix") (parseStyleText))
        <*> P.optional (P.xchild (P.name "figure-number") (parseStyleText))
        <*> P.optional (P.xchild (P.name "suffix") (parseStyleText))
        <*> P.optional (P.xchild (P.name "extend") (parseExtend))

-- | Smart constructor for 'Figure'
mkFigure :: Figure
mkFigure = Figure Nothing Nothing Nothing Nothing

-- | @figured-bass@ /(complex)/
--
-- The figured-bass element represents figured bass notation. Figured bass elements take their position from the first regular note that follows. Figures are ordered from top to bottom. The value of parentheses is "no" if not present.
data FiguredBass = 
      FiguredBass {
          figuredBassParentheses :: (Maybe YesNo) -- ^ /parentheses/ attribute
        , figuredBassDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , figuredBassDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , figuredBassRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , figuredBassRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , figuredBassFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , figuredBassFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , figuredBassFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , figuredBassFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , figuredBassColor :: (Maybe Color) -- ^ /color/ attribute
        , figuredBassPrintDot :: (Maybe YesNo) -- ^ /print-dot/ attribute
        , figuredBassPrintLyric :: (Maybe YesNo) -- ^ /print-lyric/ attribute
        , figuredBassPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , figuredBassPrintSpacing :: (Maybe YesNo) -- ^ /print-spacing/ attribute
        , figuredBassFigure :: [Figure] -- ^ /figure/ child element
        , figuredBassDuration :: (Maybe Duration)
        , figuredBassEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FiguredBass where
    emitXml (FiguredBass a b c d e f g h i j k l m n o p q) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "print-dot" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "print-lyric" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "print-spacing" Nothing).emitXml) n])
        (map (XElement (QN "figure" Nothing).emitXml) o++[emitXml p]++[emitXml q])
parseFiguredBass :: P.XParse FiguredBass
parseFiguredBass = 
      FiguredBass
        <$> P.optional (P.xattr (P.name "parentheses") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "print-dot") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-lyric") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-spacing") >>= parseYesNo)
        <*> P.many (P.xchild (P.name "figure") (parseFigure))
        <*> P.optional (parseDuration)
        <*> parseEditorial

-- | Smart constructor for 'FiguredBass'
mkFiguredBass :: Editorial -> FiguredBass
mkFiguredBass q = FiguredBass Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing q

-- | @fingering@ /(complex)/
--
-- Fingering is typically indicated 1,2,3,4,5. Multiple fingerings may be given, typically to substitute fingerings in the middle of a note. The substitution and alternate values are "no" if the attribute is not present. For guitar and other fretted instruments, the fingering element represents the fretting finger; the pluck element represents the plucking finger.
data Fingering = 
      Fingering {
          fingeringString :: String -- ^ text content
        , fingeringSubstitution :: (Maybe YesNo) -- ^ /substitution/ attribute
        , fingeringAlternate :: (Maybe YesNo) -- ^ /alternate/ attribute
        , fingeringDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , fingeringDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , fingeringRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , fingeringRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , fingeringFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , fingeringFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , fingeringFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , fingeringFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , fingeringColor :: (Maybe Color) -- ^ /color/ attribute
        , fingeringPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Fingering where
    emitXml (Fingering a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "substitution" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "alternate" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) m])
        []
parseFingering :: P.XParse Fingering
parseFingering = 
      Fingering
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "substitution") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "alternate") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'Fingering'
mkFingering :: String -> Fingering
mkFingering a = Fingering a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @first-fret@ /(complex)/
--
-- The first-fret type indicates which fret is shown in the top space of the frame; it is fret 1 if the element is not present. The optional text attribute indicates how this is represented in the fret diagram, while the location attribute indicates whether the text appears to the left or right of the frame.
data FirstFret = 
      FirstFret {
          firstFretPositiveInteger :: PositiveInteger -- ^ text content
        , firstFretText :: (Maybe Token) -- ^ /text/ attribute
        , firstFretLocation :: (Maybe LeftRight) -- ^ /location/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FirstFret where
    emitXml (FirstFret a b c) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "location" Nothing).emitXml) c])
        []
parseFirstFret :: P.XParse FirstFret
parseFirstFret = 
      FirstFret
        <$> (P.xtext >>= parsePositiveInteger)
        <*> P.optional (P.xattr (P.name "text") >>= parseToken)
        <*> P.optional (P.xattr (P.name "location") >>= parseLeftRight)

-- | Smart constructor for 'FirstFret'
mkFirstFret :: PositiveInteger -> FirstFret
mkFirstFret a = FirstFret a Nothing Nothing

-- | @formatted-text@ /(complex)/
--
-- The formatted-text type represents a text element with text-formatting attributes.
data FormattedText = 
      FormattedText {
          formattedTextString :: String -- ^ text content
        , formattedTextLang :: (Maybe Lang) -- ^ /xml:lang/ attribute
        , formattedTextEnclosure :: (Maybe Enclosure) -- ^ /enclosure/ attribute
        , formattedTextJustify :: (Maybe LeftCenterRight) -- ^ /justify/ attribute
        , formattedTextHalign :: (Maybe LeftCenterRight) -- ^ /halign/ attribute
        , formattedTextValign :: (Maybe Valign) -- ^ /valign/ attribute
        , formattedTextDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , formattedTextDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , formattedTextRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , formattedTextRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , formattedTextFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , formattedTextFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , formattedTextFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , formattedTextFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , formattedTextColor :: (Maybe Color) -- ^ /color/ attribute
        , formattedTextUnderline :: (Maybe NumberOfLines) -- ^ /underline/ attribute
        , formattedTextOverline :: (Maybe NumberOfLines) -- ^ /overline/ attribute
        , formattedTextLineThrough :: (Maybe NumberOfLines) -- ^ /line-through/ attribute
        , formattedTextRotation :: (Maybe RotationDegrees) -- ^ /rotation/ attribute
        , formattedTextLetterSpacing :: (Maybe NumberOrNormal) -- ^ /letter-spacing/ attribute
        , formattedTextLineHeight :: (Maybe NumberOrNormal) -- ^ /line-height/ attribute
        , formattedTextDir :: (Maybe TextDirection) -- ^ /dir/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FormattedText where
    emitXml (FormattedText a b c d e f g h i j k l m n o p q r s t u v) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "enclosure" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) q]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) r]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) s]++[maybe XEmpty (XAttr (QN "letter-spacing" Nothing).emitXml) t]++[maybe XEmpty (XAttr (QN "line-height" Nothing).emitXml) u]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) v])
        []
parseFormattedText :: P.XParse FormattedText
parseFormattedText = 
      FormattedText
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "xml:lang") >>= parseLang)
        <*> P.optional (P.xattr (P.name "enclosure") >>= parseEnclosure)
        <*> P.optional (P.xattr (P.name "justify") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "halign") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "valign") >>= parseValign)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "underline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "overline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "line-through") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "rotation") >>= parseRotationDegrees)
        <*> P.optional (P.xattr (P.name "letter-spacing") >>= parseNumberOrNormal)
        <*> P.optional (P.xattr (P.name "line-height") >>= parseNumberOrNormal)
        <*> P.optional (P.xattr (P.name "dir") >>= parseTextDirection)

-- | Smart constructor for 'FormattedText'
mkFormattedText :: String -> FormattedText
mkFormattedText a = FormattedText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @forward@ /(complex)/
--
-- The backup and forward elements are required to coordinate multiple voices in one part, including music on multiple staves. The forward element is generally used within voices and staves. Duration values should always be positive, and should not cross measure boundaries.
data Forward = 
      Forward {
          forwardDuration :: Duration
        , forwardEditorialVoice :: EditorialVoice
        , forwardStaff :: (Maybe Staff)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Forward where
    emitXml (Forward a b c) =
      XReps [emitXml a,emitXml b,emitXml c]
parseForward :: P.XParse Forward
parseForward = 
      Forward
        <$> parseDuration
        <*> parseEditorialVoice
        <*> P.optional (parseStaff)

-- | Smart constructor for 'Forward'
mkForward :: Duration -> EditorialVoice -> Forward
mkForward a b = Forward a b Nothing

-- | @frame@ /(complex)/
--
-- The frame type represents a frame or fretboard diagram used together with a chord symbol. The representation is based on the NIFF guitar grid with additional information.
data Frame = 
      Frame {
          frameHeight :: (Maybe Tenths) -- ^ /height/ attribute
        , frameWidth :: (Maybe Tenths) -- ^ /width/ attribute
        , frameDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , frameDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , frameRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , frameRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , frameColor :: (Maybe Color) -- ^ /color/ attribute
        , frameHalign :: (Maybe LeftCenterRight) -- ^ /halign/ attribute
        , frameValign :: (Maybe Valign) -- ^ /valign/ attribute
        , frameFrameStrings :: PositiveInteger -- ^ /frame-strings/ child element
        , frameFrameFrets :: PositiveInteger -- ^ /frame-frets/ child element
        , frameFirstFret :: (Maybe FirstFret) -- ^ /first-fret/ child element
        , frameFrameNote :: [FrameNote] -- ^ /frame-note/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Frame where
    emitXml (Frame a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "height" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "width" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) i])
        ([XElement (QN "frame-strings" Nothing) (emitXml j)]++[XElement (QN "frame-frets" Nothing) (emitXml k)]++[maybe XEmpty (XElement (QN "first-fret" Nothing).emitXml) l]++map (XElement (QN "frame-note" Nothing).emitXml) m)
parseFrame :: P.XParse Frame
parseFrame = 
      Frame
        <$> P.optional (P.xattr (P.name "height") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "width") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "halign") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "valign") >>= parseValign)
        <*> (P.xchild (P.name "frame-strings") (P.xtext >>= parsePositiveInteger))
        <*> (P.xchild (P.name "frame-frets") (P.xtext >>= parsePositiveInteger))
        <*> P.optional (P.xchild (P.name "first-fret") (parseFirstFret))
        <*> P.many (P.xchild (P.name "frame-note") (parseFrameNote))

-- | Smart constructor for 'Frame'
mkFrame :: PositiveInteger -> PositiveInteger -> Frame
mkFrame j k = Frame Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing j k Nothing []

-- | @frame-note@ /(complex)/
--
-- The frame-note type represents each note included in the frame. An open string will have a fret value of 0, while a muted string will not be associated with a frame-note element.
data FrameNote = 
      FrameNote {
          frameNoteString :: CmpString -- ^ /string/ child element
        , frameNoteFret :: Fret -- ^ /fret/ child element
        , frameNoteFingering :: (Maybe Fingering) -- ^ /fingering/ child element
        , frameNoteBarre :: (Maybe Barre) -- ^ /barre/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FrameNote where
    emitXml (FrameNote a b c d) =
      XContent XEmpty
        []
        ([XElement (QN "string" Nothing) (emitXml a)]++[XElement (QN "fret" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "fingering" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "barre" Nothing).emitXml) d])
parseFrameNote :: P.XParse FrameNote
parseFrameNote = 
      FrameNote
        <$> (P.xchild (P.name "string") (parseCmpString))
        <*> (P.xchild (P.name "fret") (parseFret))
        <*> P.optional (P.xchild (P.name "fingering") (parseFingering))
        <*> P.optional (P.xchild (P.name "barre") (parseBarre))

-- | Smart constructor for 'FrameNote'
mkFrameNote :: CmpString -> Fret -> FrameNote
mkFrameNote a b = FrameNote a b Nothing Nothing

-- | @fret@ /(complex)/
--
-- The fret element is used with tablature notation and chord diagrams. Fret numbers start with 0 for an open string and 1 for the first fret.
data Fret = 
      Fret {
          fretNonNegativeInteger :: NonNegativeInteger -- ^ text content
        , fretFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , fretFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , fretFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , fretFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , fretColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Fret where
    emitXml (Fret a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
parseFret :: P.XParse Fret
parseFret = 
      Fret
        <$> (P.xtext >>= parseNonNegativeInteger)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Fret'
mkFret :: NonNegativeInteger -> Fret
mkFret a = Fret a Nothing Nothing Nothing Nothing Nothing

-- | @glissando@ /(complex)/
--
-- Glissando and slide types both indicate rapidly moving from one pitch to the other so that individual notes are not discerned. The distinction is similar to that between NIFF's glissando and portamento elements. A glissando sounds the half notes in between the slide and defaults to a wavy line. The optional text is printed alongside the line.
data Glissando = 
      Glissando {
          glissandoString :: String -- ^ text content
        , glissandoType :: StartStop -- ^ /type/ attribute
        , glissandoNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , glissandoLineType :: (Maybe LineType) -- ^ /line-type/ attribute
        , glissandoDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , glissandoDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , glissandoRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , glissandoRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , glissandoFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , glissandoFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , glissandoFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , glissandoFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , glissandoColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Glissando where
    emitXml (Glissando a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) m])
        []
parseGlissando :: P.XParse Glissando
parseGlissando = 
      Glissando
        <$> (P.xtext >>= return)
        <*> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "line-type") >>= parseLineType)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Glissando'
mkGlissando :: String -> StartStop -> Glissando
mkGlissando a b = Glissando a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @grace@ /(complex)/
--
-- The grace type indicates the presence of a grace note. The slash attribute for a grace note is yes for slashed eighth notes. The other grace note attributes come from MuseData sound suggestions. Steal-time-previous indicates the percentage of time to steal from the previous note for the grace note. Steal-time-following indicates the percentage of time to steal from the following note for the grace note. Make-time indicates to make time, not steal time; the units are in real-time divisions for the grace note.
data Grace = 
      Grace {
          graceStealTimePrevious :: (Maybe Percent) -- ^ /steal-time-previous/ attribute
        , graceStealTimeFollowing :: (Maybe Percent) -- ^ /steal-time-following/ attribute
        , graceMakeTime :: (Maybe Divisions) -- ^ /make-time/ attribute
        , graceSlash :: (Maybe YesNo) -- ^ /slash/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Grace where
    emitXml (Grace a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "steal-time-previous" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "steal-time-following" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "make-time" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "slash" Nothing).emitXml) d])
        []
parseGrace :: P.XParse Grace
parseGrace = 
      Grace
        <$> P.optional (P.xattr (P.name "steal-time-previous") >>= parsePercent)
        <*> P.optional (P.xattr (P.name "steal-time-following") >>= parsePercent)
        <*> P.optional (P.xattr (P.name "make-time") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "slash") >>= parseYesNo)

-- | Smart constructor for 'Grace'
mkGrace :: Grace
mkGrace = Grace Nothing Nothing Nothing Nothing

-- | @group-barline@ /(complex)/
--
-- The group-barline type indicates if the group should have common barlines.
data GroupBarline = 
      GroupBarline {
          groupBarlineGroupBarlineValue :: GroupBarlineValue -- ^ text content
        , groupBarlineColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GroupBarline where
    emitXml (GroupBarline a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "color" Nothing).emitXml) b])
        []
parseGroupBarline :: P.XParse GroupBarline
parseGroupBarline = 
      GroupBarline
        <$> (P.xtext >>= parseGroupBarlineValue)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'GroupBarline'
mkGroupBarline :: GroupBarlineValue -> GroupBarline
mkGroupBarline a = GroupBarline a Nothing

-- | @group-name@ /(complex)/
--
-- The group-name type describes the name or abbreviation of a part-group element. Formatting attributes in the group-name type are deprecated in Version 2.0 in favor of the new group-name-display and group-abbreviation-display elements.
data GroupName = 
      GroupName {
          groupNameString :: String -- ^ text content
        , groupNameDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , groupNameDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , groupNameRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , groupNameRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , groupNameFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , groupNameFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , groupNameFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , groupNameFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , groupNameColor :: (Maybe Color) -- ^ /color/ attribute
        , groupNameJustify :: (Maybe LeftCenterRight) -- ^ /justify/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GroupName where
    emitXml (GroupName a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) k])
        []
parseGroupName :: P.XParse GroupName
parseGroupName = 
      GroupName
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "justify") >>= parseLeftCenterRight)

-- | Smart constructor for 'GroupName'
mkGroupName :: String -> GroupName
mkGroupName a = GroupName a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @group-symbol@ /(complex)/
--
-- The group-symbol type indicates how the symbol for a group is indicated in the score.
data GroupSymbol = 
      GroupSymbol {
          groupSymbolGroupSymbolValue :: GroupSymbolValue -- ^ text content
        , groupSymbolDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , groupSymbolDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , groupSymbolRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , groupSymbolRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , groupSymbolColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GroupSymbol where
    emitXml (GroupSymbol a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
parseGroupSymbol :: P.XParse GroupSymbol
parseGroupSymbol = 
      GroupSymbol
        <$> (P.xtext >>= parseGroupSymbolValue)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'GroupSymbol'
mkGroupSymbol :: GroupSymbolValue -> GroupSymbol
mkGroupSymbol a = GroupSymbol a Nothing Nothing Nothing Nothing Nothing

-- | @grouping@ /(complex)/
--
-- The grouping type is used for musical analysis. When the type attribute is "start" or "single", it usually contains one or more feature elements. The number attribute is used for distinguishing between overlapping and hierarchical groupings. The member-of attribute allows for easy distinguishing of what grouping elements are in what hierarchy. Feature elements contained within a "stop" type of grouping may be ignored.
-- 	
-- This element is flexible to allow for different types of analyses. Future versions of the MusicXML format may add elements that can represent more standardized categories of analysis data, allowing for easier data sharing.
data Grouping = 
      Grouping {
          groupingType :: StartStopSingle -- ^ /type/ attribute
        , groupingNumber :: (Maybe Token) -- ^ /number/ attribute
        , groupingMemberOf :: (Maybe Token) -- ^ /member-of/ attribute
        , groupingFeature :: [Feature] -- ^ /feature/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Grouping where
    emitXml (Grouping a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "member-of" Nothing).emitXml) c])
        (map (XElement (QN "feature" Nothing).emitXml) d)
parseGrouping :: P.XParse Grouping
parseGrouping = 
      Grouping
        <$> (P.xattr (P.name "type") >>= parseStartStopSingle)
        <*> P.optional (P.xattr (P.name "number") >>= parseToken)
        <*> P.optional (P.xattr (P.name "member-of") >>= parseToken)
        <*> P.many (P.xchild (P.name "feature") (parseFeature))

-- | Smart constructor for 'Grouping'
mkGrouping :: StartStopSingle -> Grouping
mkGrouping a = Grouping a Nothing Nothing []

-- | @hammer-on-pull-off@ /(complex)/
--
-- The hammer-on and pull-off elements are used in guitar and fretted instrument notation. Since a single slur can be marked over many notes, the hammer-on and pull-off elements are separate so the individual pair of notes can be specified. The element content can be used to specify how the hammer-on or pull-off should be notated. An empty element leaves this choice up to the application.
data HammerOnPullOff = 
      HammerOnPullOff {
          hammerOnPullOffString :: String -- ^ text content
        , hammerOnPullOffType :: StartStop -- ^ /type/ attribute
        , hammerOnPullOffNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , hammerOnPullOffDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , hammerOnPullOffDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , hammerOnPullOffRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , hammerOnPullOffRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , hammerOnPullOffFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , hammerOnPullOffFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , hammerOnPullOffFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , hammerOnPullOffFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , hammerOnPullOffColor :: (Maybe Color) -- ^ /color/ attribute
        , hammerOnPullOffPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HammerOnPullOff where
    emitXml (HammerOnPullOff a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) m])
        []
parseHammerOnPullOff :: P.XParse HammerOnPullOff
parseHammerOnPullOff = 
      HammerOnPullOff
        <$> (P.xtext >>= return)
        <*> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'HammerOnPullOff'
mkHammerOnPullOff :: String -> StartStop -> HammerOnPullOff
mkHammerOnPullOff a b = HammerOnPullOff a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @harmonic@ /(complex)/
--
-- The harmonic type indicates natural and artificial harmonics. Allowing the type of pitch to be specified, combined with controls for appearance/playback differences, allows both the notation and the sound to be represented. Artificial harmonics can add a notated touching-pitch; artificial pinch harmonics will usually not notate a touching pitch. The attributes for the harmonic element refer to the use of the circular harmonic symbol, typically but not always used with natural harmonics.
data Harmonic = 
      Harmonic {
          harmonicPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , harmonicDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , harmonicDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , harmonicRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , harmonicRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , harmonicFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , harmonicFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , harmonicFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , harmonicFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , harmonicColor :: (Maybe Color) -- ^ /color/ attribute
        , harmonicPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , harmonicHarmonic :: (Maybe ChxHarmonic)
        , harmonicHarmonic1 :: (Maybe ChxHarmonic1)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Harmonic where
    emitXml (Harmonic a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        ([emitXml l]++[emitXml m])
parseHarmonic :: P.XParse Harmonic
parseHarmonic = 
      Harmonic
        <$> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (parseChxHarmonic)
        <*> P.optional (parseChxHarmonic1)

-- | Smart constructor for 'Harmonic'
mkHarmonic :: Harmonic
mkHarmonic = Harmonic Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @harmony@ /(complex)/
--
-- The harmony type is based on Humdrum's **harm encoding, extended to support chord symbols in popular music as well as functional harmony analysis in classical music.
-- 	
-- If there are alternate harmonies possible, this can be specified using multiple harmony elements differentiated by type. Explicit harmonies have all note present in the music; implied have some notes missing but implied; alternate represents alternate analyses. 
-- 	
-- The harmony object may be used for analysis or for chord symbols. The print-object attribute controls whether or not anything is printed due to the harmony element. The print-frame attribute controls printing of a frame or fretboard diagram. The print-style attribute group sets the default for the harmony, but individual elements can override this with their own print-style values.
data Harmony = 
      Harmony {
          harmonyType :: (Maybe HarmonyType) -- ^ /type/ attribute
        , harmonyPrintFrame :: (Maybe YesNo) -- ^ /print-frame/ attribute
        , harmonyPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , harmonyDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , harmonyDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , harmonyRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , harmonyRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , harmonyFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , harmonyFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , harmonyFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , harmonyFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , harmonyColor :: (Maybe Color) -- ^ /color/ attribute
        , harmonyPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , harmonyHarmonyChord :: [HarmonyChord]
        , harmonyFrame :: (Maybe Frame) -- ^ /frame/ child element
        , harmonyOffset :: (Maybe Offset) -- ^ /offset/ child element
        , harmonyEditorial :: Editorial
        , harmonyStaff :: (Maybe Staff)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Harmony where
    emitXml (Harmony a b c d e f g h i j k l m n o p q r) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "print-frame" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) m])
        ([emitXml n]++[maybe XEmpty (XElement (QN "frame" Nothing).emitXml) o]++[maybe XEmpty (XElement (QN "offset" Nothing).emitXml) p]++[emitXml q]++[emitXml r])
parseHarmony :: P.XParse Harmony
parseHarmony = 
      Harmony
        <$> P.optional (P.xattr (P.name "type") >>= parseHarmonyType)
        <*> P.optional (P.xattr (P.name "print-frame") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.many (parseHarmonyChord)
        <*> P.optional (P.xchild (P.name "frame") (parseFrame))
        <*> P.optional (P.xchild (P.name "offset") (parseOffset))
        <*> parseEditorial
        <*> P.optional (parseStaff)

-- | Smart constructor for 'Harmony'
mkHarmony :: Editorial -> Harmony
mkHarmony q = Harmony Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing q Nothing

-- | @harp-pedals@ /(complex)/
--
-- The harp-pedals type is used to create harp pedal diagrams. The pedal-step and pedal-alter elements use the same values as the step and alter elements. For easiest reading, the pedal-tuning elements should follow standard harp pedal order, with pedal-step values of D, C, B, E, F, G, and A.
data HarpPedals = 
      HarpPedals {
          harpPedalsDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , harpPedalsDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , harpPedalsRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , harpPedalsRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , harpPedalsFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , harpPedalsFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , harpPedalsFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , harpPedalsFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , harpPedalsColor :: (Maybe Color) -- ^ /color/ attribute
        , harpPedalsPedalTuning :: [PedalTuning] -- ^ /pedal-tuning/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HarpPedals where
    emitXml (HarpPedals a b c d e f g h i j) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        (map (XElement (QN "pedal-tuning" Nothing).emitXml) j)
parseHarpPedals :: P.XParse HarpPedals
parseHarpPedals = 
      HarpPedals
        <$> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.many (P.xchild (P.name "pedal-tuning") (parsePedalTuning))

-- | Smart constructor for 'HarpPedals'
mkHarpPedals :: HarpPedals
mkHarpPedals = HarpPedals Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

-- | @heel-toe@ /(complex)/
--
-- The heel and toe elements are used with organ pedals. The substitution value is "no" if the attribute is not present.
data HeelToe = 
      HeelToe {
          heelToeEmptyPlacement :: HeelToe
        , heelToeSubstitution :: (Maybe YesNo) -- ^ /substitution/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HeelToe where
    emitXml (HeelToe a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "substitution" Nothing).emitXml) b])
        ([emitXml a])
parseHeelToe :: P.XParse HeelToe
parseHeelToe = 
      HeelToe
        <$> parseHeelToe
        <*> P.optional (P.xattr (P.name "substitution") >>= parseYesNo)

-- | Smart constructor for 'HeelToe'
mkHeelToe :: HeelToe -> HeelToe
mkHeelToe a = HeelToe a Nothing

-- | @identification@ /(complex)/
--
-- Identification contains basic metadata about the score. It includes the information in MuseData headers that may apply at a score-wide, movement-wide, or part-wide level. The creator, rights, source, and relation elements are based on Dublin Core.
data Identification = 
      Identification {
          identificationCreator :: [TypedText] -- ^ /creator/ child element
        , identificationRights :: [TypedText] -- ^ /rights/ child element
        , identificationEncoding :: (Maybe Encoding) -- ^ /encoding/ child element
        , identificationSource :: (Maybe String) -- ^ /source/ child element
        , identificationRelation :: [TypedText] -- ^ /relation/ child element
        , identificationMiscellaneous :: (Maybe Miscellaneous) -- ^ /miscellaneous/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Identification where
    emitXml (Identification a b c d e f) =
      XContent XEmpty
        []
        (map (XElement (QN "creator" Nothing).emitXml) a++map (XElement (QN "rights" Nothing).emitXml) b++[maybe XEmpty (XElement (QN "encoding" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "source" Nothing).emitXml) d]++map (XElement (QN "relation" Nothing).emitXml) e++[maybe XEmpty (XElement (QN "miscellaneous" Nothing).emitXml) f])
parseIdentification :: P.XParse Identification
parseIdentification = 
      Identification
        <$> P.many (P.xchild (P.name "creator") (parseTypedText))
        <*> P.many (P.xchild (P.name "rights") (parseTypedText))
        <*> P.optional (P.xchild (P.name "encoding") (parseEncoding))
        <*> P.optional (P.xchild (P.name "source") (P.xtext >>= return))
        <*> P.many (P.xchild (P.name "relation") (parseTypedText))
        <*> P.optional (P.xchild (P.name "miscellaneous") (parseMiscellaneous))

-- | Smart constructor for 'Identification'
mkIdentification :: Identification
mkIdentification = Identification [] [] Nothing Nothing [] Nothing

-- | @image@ /(complex)/
--
-- The image type is used to include graphical images in a score.
data Image = 
      Image {
          imageSource :: String -- ^ /source/ attribute
        , imageType :: Token -- ^ /type/ attribute
        , imageDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , imageDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , imageRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , imageRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , imageHalign :: (Maybe LeftCenterRight) -- ^ /halign/ attribute
        , imageValign :: (Maybe ValignImage) -- ^ /valign/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Image where
    emitXml (Image a b c d e f g h) =
      XContent XEmpty
        ([XAttr (QN "source" Nothing) (emitXml a)]++[XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) h])
        []
parseImage :: P.XParse Image
parseImage = 
      Image
        <$> (P.xattr (P.name "source") >>= return)
        <*> (P.xattr (P.name "type") >>= parseToken)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "halign") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "valign") >>= parseValignImage)

-- | Smart constructor for 'Image'
mkImage :: String -> Token -> Image
mkImage a b = Image a b Nothing Nothing Nothing Nothing Nothing Nothing

-- | @instrument@ /(complex)/
--
-- The instrument type distinguishes between score-instrument elements in a score-part. The id attribute is an IDREF back to the score-instrument ID. If multiple score-instruments are specified on a score-part, there should be an instrument element for each note in the part.
data Instrument = 
      Instrument {
          instrumentId :: IDREF -- ^ /id/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Instrument where
    emitXml (Instrument a) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        []
parseInstrument :: P.XParse Instrument
parseInstrument = 
      Instrument
        <$> (P.xattr (P.name "id") >>= parseIDREF)

-- | Smart constructor for 'Instrument'
mkInstrument :: IDREF -> Instrument
mkInstrument a = Instrument a

-- | @inversion@ /(complex)/
--
-- The inversion type represents harmony inversions. The value is a number indicating which inversion is used: 0 for root position, 1 for first inversion, etc.
data Inversion = 
      Inversion {
          inversionNonNegativeInteger :: NonNegativeInteger -- ^ text content
        , inversionDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , inversionDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , inversionRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , inversionRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , inversionFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , inversionFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , inversionFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , inversionFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , inversionColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Inversion where
    emitXml (Inversion a b c d e f g h i j) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
parseInversion :: P.XParse Inversion
parseInversion = 
      Inversion
        <$> (P.xtext >>= parseNonNegativeInteger)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Inversion'
mkInversion :: NonNegativeInteger -> Inversion
mkInversion a = Inversion a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @key@ /(complex)/
--
-- The key type represents a key signature. Both traditional and non-traditional key signatures are supported. The optional number attribute refers to staff numbers. If absent, the key signature applies to all staves in the part.
data Key = 
      Key {
          keyNumber :: (Maybe StaffNumber) -- ^ /number/ attribute
        , keyDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , keyDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , keyRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , keyRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , keyFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , keyFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , keyFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , keyFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , keyColor :: (Maybe Color) -- ^ /color/ attribute
        , keyPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , keyKey :: ChxKey
        , keyKeyOctave :: [KeyOctave] -- ^ /key-octave/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Key where
    emitXml (Key a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) k])
        ([emitXml l]++map (XElement (QN "key-octave" Nothing).emitXml) m)
parseKey :: P.XParse Key
parseKey = 
      Key
        <$> P.optional (P.xattr (P.name "number") >>= parseStaffNumber)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> parseChxKey
        <*> P.many (P.xchild (P.name "key-octave") (parseKeyOctave))

-- | Smart constructor for 'Key'
mkKey :: ChxKey -> Key
mkKey l = Key Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing l []

-- | @key-octave@ /(complex)/
--
-- The key-octave element specifies in which octave an element of a key signature appears. The content specifies the octave value using the same values as the display-octave element. The number attribute is a positive integer that refers to the key signature element in left-to-right order. If the cancel attribute is set to yes, then this number refers to an element specified by the cancel element. It is no by default.
data KeyOctave = 
      KeyOctave {
          keyOctaveOctave :: Octave -- ^ text content
        , keyOctaveNumber :: PositiveInteger -- ^ /number/ attribute
        , keyOctaveCancel :: (Maybe YesNo) -- ^ /cancel/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml KeyOctave where
    emitXml (KeyOctave a b c) =
      XContent (emitXml a)
        ([XAttr (QN "number" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "cancel" Nothing).emitXml) c])
        []
parseKeyOctave :: P.XParse KeyOctave
parseKeyOctave = 
      KeyOctave
        <$> (P.xtext >>= parseOctave)
        <*> (P.xattr (P.name "number") >>= parsePositiveInteger)
        <*> P.optional (P.xattr (P.name "cancel") >>= parseYesNo)

-- | Smart constructor for 'KeyOctave'
mkKeyOctave :: Octave -> PositiveInteger -> KeyOctave
mkKeyOctave a b = KeyOctave a b Nothing

-- | @kind@ /(complex)/
--
-- Kind indicates the type of chord. Degree elements can then add, subtract, or alter from these starting points
--
-- @
-- 	
-- The attributes are used to indicate the formatting of the symbol. Since the kind element is the constant in all the harmony-chord groups that can make up a polychord, many formatting attributes are here.
-- 	
-- The use-symbols attribute is yes if the kind should be represented when possible with harmony symbols rather than letters and numbers. These symbols include:
-- 	
-- 	major: a triangle, like Unicode 25B3
-- 	minor: -, like Unicode 002D
-- 	augmented: +, like Unicode 002B
-- 	diminished: °, like Unicode 00B0
-- 	half-diminished: ø, like Unicode 00F8
-- 	
-- The text attribute describes how the kind should be spelled if not using symbols; it is ignored if use-symbols is yes. The stack-degrees attribute is yes if the degree elements should be stacked above each other. The parentheses-degrees attribute is yes if all the degrees should be in parentheses. The bracket-degrees attribute is yes if all the degrees should be in a bracket. If not specified, these values are implementation-specific. The alignment attributes are for the entire harmony-chord group of which this kind element is a part.
-- @
data Kind = 
      Kind {
          kindKindValue :: KindValue -- ^ text content
        , kindUseSymbols :: (Maybe YesNo) -- ^ /use-symbols/ attribute
        , kindText :: (Maybe Token) -- ^ /text/ attribute
        , kindStackDegrees :: (Maybe YesNo) -- ^ /stack-degrees/ attribute
        , kindParenthesesDegrees :: (Maybe YesNo) -- ^ /parentheses-degrees/ attribute
        , kindBracketDegrees :: (Maybe YesNo) -- ^ /bracket-degrees/ attribute
        , kindDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , kindDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , kindRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , kindRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , kindFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , kindFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , kindFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , kindFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , kindColor :: (Maybe Color) -- ^ /color/ attribute
        , kindHalign :: (Maybe LeftCenterRight) -- ^ /halign/ attribute
        , kindValign :: (Maybe Valign) -- ^ /valign/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Kind where
    emitXml (Kind a b c d e f g h i j k l m n o p q) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "use-symbols" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "text" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "stack-degrees" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "parentheses-degrees" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "bracket-degrees" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) q])
        []
parseKind :: P.XParse Kind
parseKind = 
      Kind
        <$> (P.xtext >>= parseKindValue)
        <*> P.optional (P.xattr (P.name "use-symbols") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "text") >>= parseToken)
        <*> P.optional (P.xattr (P.name "stack-degrees") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "parentheses-degrees") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "bracket-degrees") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "halign") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "valign") >>= parseValign)

-- | Smart constructor for 'Kind'
mkKind :: KindValue -> Kind
mkKind a = Kind a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @level@ /(complex)/
--
-- The level type is used to specify editorial information for different MusicXML elements. If the reference attribute for the level element is yes, this indicates editorial information that is for display only and should not affect playback. For instance, a modern edition of older music may set reference="yes" on the attributes containing the music's original clef, key, and time signature. It is no by default.
data Level = 
      Level {
          levelString :: String -- ^ text content
        , levelReference :: (Maybe YesNo) -- ^ /reference/ attribute
        , levelParentheses :: (Maybe YesNo) -- ^ /parentheses/ attribute
        , levelBracket :: (Maybe YesNo) -- ^ /bracket/ attribute
        , levelSize :: (Maybe SymbolSize) -- ^ /size/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Level where
    emitXml (Level a b c d e) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "reference" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) e])
        []
parseLevel :: P.XParse Level
parseLevel = 
      Level
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "reference") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "parentheses") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "bracket") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "size") >>= parseSymbolSize)

-- | Smart constructor for 'Level'
mkLevel :: String -> Level
mkLevel a = Level a Nothing Nothing Nothing Nothing

-- | @line-width@ /(complex)/
--
-- The line-width type indicates the width of a line type in tenths. The type attribute defines what type of line is being defined. Values include beam, bracket, dashes, enclosure, ending, extend, heavy barline, leger, light barline, octave shift, pedal, slur middle, slur tip, staff, stem, tie middle, tie tip, tuplet bracket, and wedge. The text content is expressed in tenths.
data LineWidth = 
      LineWidth {
          lineWidthTenths :: Tenths -- ^ text content
        , cmplineWidthType :: LineWidthType -- ^ /type/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LineWidth where
    emitXml (LineWidth a b) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)])
        []
parseLineWidth :: P.XParse LineWidth
parseLineWidth = 
      LineWidth
        <$> (P.xtext >>= parseTenths)
        <*> (P.xattr (P.name "type") >>= parseLineWidthType)

-- | Smart constructor for 'LineWidth'
mkLineWidth :: Tenths -> LineWidthType -> LineWidth
mkLineWidth a b = LineWidth a b

-- | @link@ /(complex)/
--
-- The link type serves as an outgoing simple XLink. It is also used to connect a MusicXML score with a MusicXML opus.
data Link = 
      Link {
          linkName :: (Maybe Token) -- ^ /name/ attribute
        , linkHref :: String -- ^ /xlink:href/ attribute
        , linkType :: (Maybe Type) -- ^ /xlink:type/ attribute
        , linkRole :: (Maybe Token) -- ^ /xlink:role/ attribute
        , linkTitle :: (Maybe Token) -- ^ /xlink:title/ attribute
        , linkShow :: (Maybe SmpShow) -- ^ /xlink:show/ attribute
        , linkActuate :: (Maybe Actuate) -- ^ /xlink:actuate/ attribute
        , linkElement :: (Maybe NMTOKEN) -- ^ /element/ attribute
        , linkPosition :: (Maybe PositiveInteger) -- ^ /position/ attribute
        , linkDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , linkDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , linkRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , linkRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Link where
    emitXml (Link a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "name" Nothing).emitXml) a]++[XAttr (QN "href" (Just "xlink")) (emitXml b)]++[maybe XEmpty (XAttr (QN "type" (Just "xlink")).emitXml) c]++[maybe XEmpty (XAttr (QN "role" (Just "xlink")).emitXml) d]++[maybe XEmpty (XAttr (QN "title" (Just "xlink")).emitXml) e]++[maybe XEmpty (XAttr (QN "show" (Just "xlink")).emitXml) f]++[maybe XEmpty (XAttr (QN "actuate" (Just "xlink")).emitXml) g]++[maybe XEmpty (XAttr (QN "element" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "position" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) m])
        []
parseLink :: P.XParse Link
parseLink = 
      Link
        <$> P.optional (P.xattr (P.name "name") >>= parseToken)
        <*> (P.xattr (P.name "xlink:href") >>= return)
        <*> P.optional (P.xattr (P.name "xlink:type") >>= parseType)
        <*> P.optional (P.xattr (P.name "xlink:role") >>= parseToken)
        <*> P.optional (P.xattr (P.name "xlink:title") >>= parseToken)
        <*> P.optional (P.xattr (P.name "xlink:show") >>= parseSmpShow)
        <*> P.optional (P.xattr (P.name "xlink:actuate") >>= parseActuate)
        <*> P.optional (P.xattr (P.name "element") >>= parseNMTOKEN)
        <*> P.optional (P.xattr (P.name "position") >>= parsePositiveInteger)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)

-- | Smart constructor for 'Link'
mkLink :: String -> Link
mkLink b = Link Nothing b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @lyric@ /(complex)/
--
-- The lyric type represents text underlays for lyrics, based on Humdrum with support for other formats. Two text elements that are not separated by an elision element are part of the same syllable, but may have different text formatting. The MusicXML 2.0 XSD is more strict than the 2.0 DTD in enforcing this by disallowing a second syllabic element unless preceded by an elision element. The lyric number indicates multiple lines, though a name can be used as well (as in Finale's verse / chorus / section specification). Justification is center by default; placement is below by default.
data Lyric = 
      Lyric {
          lyricNumber :: (Maybe NMTOKEN) -- ^ /number/ attribute
        , lyricName :: (Maybe Token) -- ^ /name/ attribute
        , lyricJustify :: (Maybe LeftCenterRight) -- ^ /justify/ attribute
        , lyricDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , lyricDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , lyricRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , lyricRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , lyricPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , lyricColor :: (Maybe Color) -- ^ /color/ attribute
        , lyricLyric :: ChxLyric
        , lyricEndLine :: (Maybe Empty) -- ^ /end-line/ child element
        , lyricEndParagraph :: (Maybe Empty) -- ^ /end-paragraph/ child element
        , lyricEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Lyric where
    emitXml (Lyric a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        ([emitXml j]++[maybe XEmpty (XElement (QN "end-line" Nothing).emitXml) k]++[maybe XEmpty (XElement (QN "end-paragraph" Nothing).emitXml) l]++[emitXml m])
parseLyric :: P.XParse Lyric
parseLyric = 
      Lyric
        <$> P.optional (P.xattr (P.name "number") >>= parseNMTOKEN)
        <*> P.optional (P.xattr (P.name "name") >>= parseToken)
        <*> P.optional (P.xattr (P.name "justify") >>= parseLeftCenterRight)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> parseChxLyric
        <*> P.optional (P.xchild (P.name "end-line") (parseEmpty))
        <*> P.optional (P.xchild (P.name "end-paragraph") (parseEmpty))
        <*> parseEditorial

-- | Smart constructor for 'Lyric'
mkLyric :: ChxLyric -> Editorial -> Lyric
mkLyric j m = Lyric Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing j Nothing Nothing m

-- | @lyric-font@ /(complex)/
--
-- The lyric-font type specifies the default font for a particular name and number of lyric.
data LyricFont = 
      LyricFont {
          lyricFontNumber :: (Maybe NMTOKEN) -- ^ /number/ attribute
        , lyricFontName :: (Maybe Token) -- ^ /name/ attribute
        , lyricFontFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , lyricFontFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , lyricFontFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , lyricFontFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LyricFont where
    emitXml (LyricFont a b c d e f) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) f])
        []
parseLyricFont :: P.XParse LyricFont
parseLyricFont = 
      LyricFont
        <$> P.optional (P.xattr (P.name "number") >>= parseNMTOKEN)
        <*> P.optional (P.xattr (P.name "name") >>= parseToken)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)

-- | Smart constructor for 'LyricFont'
mkLyricFont :: LyricFont
mkLyricFont = LyricFont Nothing Nothing Nothing Nothing Nothing Nothing

-- | @lyric-language@ /(complex)/
--
-- The lyric-language type specifies the default language for a particular name and number of lyric.
data LyricLanguage = 
      LyricLanguage {
          lyricLanguageNumber :: (Maybe NMTOKEN) -- ^ /number/ attribute
        , lyricLanguageName :: (Maybe Token) -- ^ /name/ attribute
        , lyricLanguageLang :: Lang -- ^ /xml:lang/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LyricLanguage where
    emitXml (LyricLanguage a b c) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[XAttr (QN "lang" (Just "xml")) (emitXml c)])
        []
parseLyricLanguage :: P.XParse LyricLanguage
parseLyricLanguage = 
      LyricLanguage
        <$> P.optional (P.xattr (P.name "number") >>= parseNMTOKEN)
        <*> P.optional (P.xattr (P.name "name") >>= parseToken)
        <*> (P.xattr (P.name "xml:lang") >>= parseLang)

-- | Smart constructor for 'LyricLanguage'
mkLyricLanguage :: Lang -> LyricLanguage
mkLyricLanguage c = LyricLanguage Nothing Nothing c

-- | @measure@ /(complex)/
data Measure = 
      Measure {
          measureNumber :: Token -- ^ /number/ attribute
        , measureImplicit :: (Maybe YesNo) -- ^ /implicit/ attribute
        , measureNonControlling :: (Maybe YesNo) -- ^ /non-controlling/ attribute
        , measureWidth :: (Maybe Tenths) -- ^ /width/ attribute
        , measureMusicData :: MusicData
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Measure where
    emitXml (Measure a b c d e) =
      XContent XEmpty
        ([XAttr (QN "number" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "implicit" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "non-controlling" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "width" Nothing).emitXml) d])
        ([emitXml e])
parseMeasure :: P.XParse Measure
parseMeasure = 
      Measure
        <$> (P.xattr (P.name "number") >>= parseToken)
        <*> P.optional (P.xattr (P.name "implicit") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "non-controlling") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "width") >>= parseTenths)
        <*> parseMusicData

-- | Smart constructor for 'Measure'
mkMeasure :: Token -> MusicData -> Measure
mkMeasure a e = Measure a Nothing Nothing Nothing e

-- | @measure@ /(complex)/

-- mangled: 1
data CmpMeasure = 
      CmpMeasure {
          cmpmeasureNumber :: Token -- ^ /number/ attribute
        , cmpmeasureImplicit :: (Maybe YesNo) -- ^ /implicit/ attribute
        , cmpmeasureNonControlling :: (Maybe YesNo) -- ^ /non-controlling/ attribute
        , cmpmeasureWidth :: (Maybe Tenths) -- ^ /width/ attribute
        , measurePart :: [Part] -- ^ /part/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpMeasure where
    emitXml (CmpMeasure a b c d e) =
      XContent XEmpty
        ([XAttr (QN "number" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "implicit" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "non-controlling" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "width" Nothing).emitXml) d])
        (map (XElement (QN "part" Nothing).emitXml) e)
parseCmpMeasure :: P.XParse CmpMeasure
parseCmpMeasure = 
      CmpMeasure
        <$> (P.xattr (P.name "number") >>= parseToken)
        <*> P.optional (P.xattr (P.name "implicit") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "non-controlling") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "width") >>= parseTenths)
        <*> P.many (P.xchild (P.name "part") (parsePart))

-- | Smart constructor for 'CmpMeasure'
mkCmpMeasure :: Token -> CmpMeasure
mkCmpMeasure a = CmpMeasure a Nothing Nothing Nothing []

-- | @measure-layout@ /(complex)/
--
-- The measure-layout type includes the horizontal distance from the previous measure.
data MeasureLayout = 
      MeasureLayout {
          measureLayoutMeasureDistance :: (Maybe Tenths) -- ^ /measure-distance/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureLayout where
    emitXml (MeasureLayout a) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "measure-distance" Nothing).emitXml) a])
parseMeasureLayout :: P.XParse MeasureLayout
parseMeasureLayout = 
      MeasureLayout
        <$> P.optional (P.xchild (P.name "measure-distance") (P.xtext >>= parseTenths))

-- | Smart constructor for 'MeasureLayout'
mkMeasureLayout :: MeasureLayout
mkMeasureLayout = MeasureLayout Nothing

-- | @measure-numbering@ /(complex)/
--
-- The measure-numbering type describes how frequently measure numbers are displayed on this part. The number attribute from the measure element is used for printing. Measures with an implicit attribute set to "yes" never display a measure number, regardless of the measure-numbering setting.
data MeasureNumbering = 
      MeasureNumbering {
          measureNumberingMeasureNumberingValue :: MeasureNumberingValue -- ^ text content
        , measureNumberingDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , measureNumberingDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , measureNumberingRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , measureNumberingRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , measureNumberingFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , measureNumberingFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , measureNumberingFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , measureNumberingFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , measureNumberingColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureNumbering where
    emitXml (MeasureNumbering a b c d e f g h i j) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
parseMeasureNumbering :: P.XParse MeasureNumbering
parseMeasureNumbering = 
      MeasureNumbering
        <$> (P.xtext >>= parseMeasureNumberingValue)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'MeasureNumbering'
mkMeasureNumbering :: MeasureNumberingValue -> MeasureNumbering
mkMeasureNumbering a = MeasureNumbering a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @measure-repeat@ /(complex)/
--
-- The measure-repeat type is used for both single and multiple measure repeats. The text of the element indicates the number of measures to be repeated in a single pattern. The slashes attribute specifies the number of slashes to use in the repeat sign. It is 1 if not specified. Both the start and the stop of the measure-repeat must be specified. The text of the element is ignored when the type is stop.
-- 	
-- The measure-repeat element specifies a notation style for repetitions. The actual music being repeated needs to be repeated within the MusicXML file. This element specifies the notation that indicates the repeat.
data MeasureRepeat = 
      MeasureRepeat {
          measureRepeatPositiveIntegerOrEmpty :: PositiveIntegerOrEmpty -- ^ text content
        , measureRepeatType :: StartStop -- ^ /type/ attribute
        , measureRepeatSlashes :: (Maybe PositiveInteger) -- ^ /slashes/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureRepeat where
    emitXml (MeasureRepeat a b c) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "slashes" Nothing).emitXml) c])
        []
parseMeasureRepeat :: P.XParse MeasureRepeat
parseMeasureRepeat = 
      MeasureRepeat
        <$> (P.xtext >>= parsePositiveIntegerOrEmpty)
        <*> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "slashes") >>= parsePositiveInteger)

-- | Smart constructor for 'MeasureRepeat'
mkMeasureRepeat :: PositiveIntegerOrEmpty -> StartStop -> MeasureRepeat
mkMeasureRepeat a b = MeasureRepeat a b Nothing

-- | @measure-style@ /(complex)/
--
-- A measure-style indicates a special way to print partial to multiple measures within a part. This includes multiple rests over several measures, repeats of beats, single, or multiple measures, and use of slash notation.
-- 
-- The multiple-rest and measure-repeat symbols indicate the number of measures covered in the element content. The beat-repeat and slash elements can cover partial measures. All but the multiple-rest element use a type attribute to indicate starting and stopping the use of the style. The optional number attribute specifies the staff number from top to bottom on the system, as with clef.
data MeasureStyle = 
      MeasureStyle {
          measureStyleNumber :: (Maybe StaffNumber) -- ^ /number/ attribute
        , measureStyleFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , measureStyleFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , measureStyleFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , measureStyleFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , measureStyleColor :: (Maybe Color) -- ^ /color/ attribute
        , measureStyleMeasureStyle :: ChxMeasureStyle
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureStyle where
    emitXml (MeasureStyle a b c d e f g) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        ([emitXml g])
parseMeasureStyle :: P.XParse MeasureStyle
parseMeasureStyle = 
      MeasureStyle
        <$> P.optional (P.xattr (P.name "number") >>= parseStaffNumber)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> parseChxMeasureStyle

-- | Smart constructor for 'MeasureStyle'
mkMeasureStyle :: ChxMeasureStyle -> MeasureStyle
mkMeasureStyle g = MeasureStyle Nothing Nothing Nothing Nothing Nothing Nothing g

-- | @metronome@ /(complex)/
--
-- The metronome type represents metronome marks and other metric relationships. The beat-unit group and per-minute element specify regular metronome marks. The metronome-note and metronome-relation elements allow for the specification of more complicated metric relationships, such as swing tempo marks where two eighths are equated to a quarter note / eighth note triplet. The parentheses attribute indicates whether or not to put the metronome mark in parentheses; its value is no if not specified.
data Metronome = 
      Metronome {
          metronomeParentheses :: (Maybe YesNo) -- ^ /parentheses/ attribute
        , metronomeDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , metronomeDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , metronomeRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , metronomeRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , metronomeFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , metronomeFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , metronomeFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , metronomeFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , metronomeColor :: (Maybe Color) -- ^ /color/ attribute
        , metronomeMetronome :: ChxMetronome
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Metronome where
    emitXml (Metronome a b c d e f g h i j k) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        ([emitXml k])
parseMetronome :: P.XParse Metronome
parseMetronome = 
      Metronome
        <$> P.optional (P.xattr (P.name "parentheses") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> parseChxMetronome

-- | Smart constructor for 'Metronome'
mkMetronome :: ChxMetronome -> Metronome
mkMetronome k = Metronome Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing k

-- | @metronome-beam@ /(complex)/
--
-- The metronome-beam type works like the beam type in defining metric relationships, but does not include all the attributes available in the beam type.
data MetronomeBeam = 
      MetronomeBeam {
          metronomeBeamBeamValue :: BeamValue -- ^ text content
        , metronomeBeamNumber :: (Maybe BeamLevel) -- ^ /number/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MetronomeBeam where
    emitXml (MetronomeBeam a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b])
        []
parseMetronomeBeam :: P.XParse MetronomeBeam
parseMetronomeBeam = 
      MetronomeBeam
        <$> (P.xtext >>= parseBeamValue)
        <*> P.optional (P.xattr (P.name "number") >>= parseBeamLevel)

-- | Smart constructor for 'MetronomeBeam'
mkMetronomeBeam :: BeamValue -> MetronomeBeam
mkMetronomeBeam a = MetronomeBeam a Nothing

-- | @metronome-note@ /(complex)/
--
-- The metronome-note type defines the appearance of a note within a metric relationship mark.
data MetronomeNote = 
      MetronomeNote {
          metronomeNoteMetronomeType :: NoteTypeValue -- ^ /metronome-type/ child element
        , metronomeNoteMetronomeDot :: [Empty] -- ^ /metronome-dot/ child element
        , metronomeNoteMetronomeBeam :: [MetronomeBeam] -- ^ /metronome-beam/ child element
        , metronomeNoteMetronomeTuplet :: (Maybe MetronomeTuplet) -- ^ /metronome-tuplet/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MetronomeNote where
    emitXml (MetronomeNote a b c d) =
      XContent XEmpty
        []
        ([XElement (QN "metronome-type" Nothing) (emitXml a)]++map (XElement (QN "metronome-dot" Nothing).emitXml) b++map (XElement (QN "metronome-beam" Nothing).emitXml) c++[maybe XEmpty (XElement (QN "metronome-tuplet" Nothing).emitXml) d])
parseMetronomeNote :: P.XParse MetronomeNote
parseMetronomeNote = 
      MetronomeNote
        <$> (P.xchild (P.name "metronome-type") (P.xtext >>= parseNoteTypeValue))
        <*> P.many (P.xchild (P.name "metronome-dot") (parseEmpty))
        <*> P.many (P.xchild (P.name "metronome-beam") (parseMetronomeBeam))
        <*> P.optional (P.xchild (P.name "metronome-tuplet") (parseMetronomeTuplet))

-- | Smart constructor for 'MetronomeNote'
mkMetronomeNote :: NoteTypeValue -> MetronomeNote
mkMetronomeNote a = MetronomeNote a [] [] Nothing

-- | @metronome-tuplet@ /(complex)/
--
-- The metronome-tuplet type uses the same element structure as the time-modification element along with some attributes from the tuplet element.
data MetronomeTuplet = 
      MetronomeTuplet {
          metronomeTupletTimeModification :: MetronomeTuplet
        , metronomeTupletType :: StartStop -- ^ /type/ attribute
        , metronomeTupletBracket :: (Maybe YesNo) -- ^ /bracket/ attribute
        , metronomeTupletShowNumber :: (Maybe ShowTuplet) -- ^ /show-number/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MetronomeTuplet where
    emitXml (MetronomeTuplet a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "show-number" Nothing).emitXml) d])
        ([emitXml a])
parseMetronomeTuplet :: P.XParse MetronomeTuplet
parseMetronomeTuplet = 
      MetronomeTuplet
        <$> parseMetronomeTuplet
        <*> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "bracket") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "show-number") >>= parseShowTuplet)

-- | Smart constructor for 'MetronomeTuplet'
mkMetronomeTuplet :: MetronomeTuplet -> StartStop -> MetronomeTuplet
mkMetronomeTuplet a b = MetronomeTuplet a b Nothing Nothing

-- | @midi-device@ /(complex)/
--
-- The midi-device type corresponds to the DeviceName meta event in Standard MIDI Files. The optional port attribute is a number from 1 to 16 that can be used with the unofficial MIDI port (or cable) meta event.
data MidiDevice = 
      MidiDevice {
          midiDeviceString :: String -- ^ text content
        , midiDevicePort :: (Maybe Midi16) -- ^ /port/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MidiDevice where
    emitXml (MidiDevice a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "port" Nothing).emitXml) b])
        []
parseMidiDevice :: P.XParse MidiDevice
parseMidiDevice = 
      MidiDevice
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "port") >>= parseMidi16)

-- | Smart constructor for 'MidiDevice'
mkMidiDevice :: String -> MidiDevice
mkMidiDevice a = MidiDevice a Nothing

-- | @midi-instrument@ /(complex)/
--
-- The midi-instrument type defines MIDI 1.0 instrument playback. The midi-instrument element can be a part of either the score-instrument element at the start of a part, or the sound element within a part. The id attribute refers to the score-instrument affected by the change.
data MidiInstrument = 
      MidiInstrument {
          midiInstrumentId :: IDREF -- ^ /id/ attribute
        , midiInstrumentMidiChannel :: (Maybe Midi16) -- ^ /midi-channel/ child element
        , midiInstrumentMidiName :: (Maybe String) -- ^ /midi-name/ child element
        , midiInstrumentMidiBank :: (Maybe Midi16384) -- ^ /midi-bank/ child element
        , midiInstrumentMidiProgram :: (Maybe Midi128) -- ^ /midi-program/ child element
        , midiInstrumentMidiUnpitched :: (Maybe Midi128) -- ^ /midi-unpitched/ child element
        , midiInstrumentVolume :: (Maybe Percent) -- ^ /volume/ child element
        , midiInstrumentPan :: (Maybe RotationDegrees) -- ^ /pan/ child element
        , midiInstrumentElevation :: (Maybe RotationDegrees) -- ^ /elevation/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MidiInstrument where
    emitXml (MidiInstrument a b c d e f g h i) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([maybe XEmpty (XElement (QN "midi-channel" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "midi-name" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "midi-bank" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "midi-program" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "midi-unpitched" Nothing).emitXml) f]++[maybe XEmpty (XElement (QN "volume" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "pan" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "elevation" Nothing).emitXml) i])
parseMidiInstrument :: P.XParse MidiInstrument
parseMidiInstrument = 
      MidiInstrument
        <$> (P.xattr (P.name "id") >>= parseIDREF)
        <*> P.optional (P.xchild (P.name "midi-channel") (P.xtext >>= parseMidi16))
        <*> P.optional (P.xchild (P.name "midi-name") (P.xtext >>= return))
        <*> P.optional (P.xchild (P.name "midi-bank") (P.xtext >>= parseMidi16384))
        <*> P.optional (P.xchild (P.name "midi-program") (P.xtext >>= parseMidi128))
        <*> P.optional (P.xchild (P.name "midi-unpitched") (P.xtext >>= parseMidi128))
        <*> P.optional (P.xchild (P.name "volume") (P.xtext >>= parsePercent))
        <*> P.optional (P.xchild (P.name "pan") (P.xtext >>= parseRotationDegrees))
        <*> P.optional (P.xchild (P.name "elevation") (P.xtext >>= parseRotationDegrees))

-- | Smart constructor for 'MidiInstrument'
mkMidiInstrument :: IDREF -> MidiInstrument
mkMidiInstrument a = MidiInstrument a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @miscellaneous@ /(complex)/
--
-- If a program has other metadata not yet supported in the MusicXML format, it can go in the miscellaneous element. The miscellaneous type puts each separate part of metadata into its own miscellaneous-field type.
data Miscellaneous = 
      Miscellaneous {
          miscellaneousMiscellaneousField :: [MiscellaneousField] -- ^ /miscellaneous-field/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Miscellaneous where
    emitXml (Miscellaneous a) =
      XContent XEmpty
        []
        (map (XElement (QN "miscellaneous-field" Nothing).emitXml) a)
parseMiscellaneous :: P.XParse Miscellaneous
parseMiscellaneous = 
      Miscellaneous
        <$> P.many (P.xchild (P.name "miscellaneous-field") (parseMiscellaneousField))

-- | Smart constructor for 'Miscellaneous'
mkMiscellaneous :: Miscellaneous
mkMiscellaneous = Miscellaneous []

-- | @miscellaneous-field@ /(complex)/
--
-- If a program has other metadata not yet supported in the MusicXML format, each type of metadata can go in a miscellaneous-field element. The required name attribute indicates the type of metadata the element content represents.
data MiscellaneousField = 
      MiscellaneousField {
          miscellaneousFieldString :: String -- ^ text content
        , miscellaneousFieldName :: Token -- ^ /name/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MiscellaneousField where
    emitXml (MiscellaneousField a b) =
      XContent (emitXml a)
        ([XAttr (QN "name" Nothing) (emitXml b)])
        []
parseMiscellaneousField :: P.XParse MiscellaneousField
parseMiscellaneousField = 
      MiscellaneousField
        <$> (P.xtext >>= return)
        <*> (P.xattr (P.name "name") >>= parseToken)

-- | Smart constructor for 'MiscellaneousField'
mkMiscellaneousField :: String -> Token -> MiscellaneousField
mkMiscellaneousField a b = MiscellaneousField a b

-- | @mordent@ /(complex)/
--
-- The mordent type is used for both represents the mordent sign with the vertical line and the inverted-mordent sign without the line. The long attribute is "no" by default.
data Mordent = 
      Mordent {
          mordentEmptyTrillSound :: Mordent
        , mordentLong :: (Maybe YesNo) -- ^ /long/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Mordent where
    emitXml (Mordent a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "long" Nothing).emitXml) b])
        ([emitXml a])
parseMordent :: P.XParse Mordent
parseMordent = 
      Mordent
        <$> parseMordent
        <*> P.optional (P.xattr (P.name "long") >>= parseYesNo)

-- | Smart constructor for 'Mordent'
mkMordent :: Mordent -> Mordent
mkMordent a = Mordent a Nothing

-- | @multiple-rest@ /(complex)/
--
-- The text of the multiple-rest type indicates the number of measures in the multiple rest. Multiple rests may use the 1-bar / 2-bar / 4-bar rest symbols, or a single shape. The use-symbols attribute indicates which to use; it is no if not specified. The element text is ignored when the type is stop.
data MultipleRest = 
      MultipleRest {
          multipleRestPositiveIntegerOrEmpty :: PositiveIntegerOrEmpty -- ^ text content
        , multipleRestUseSymbols :: (Maybe YesNo) -- ^ /use-symbols/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MultipleRest where
    emitXml (MultipleRest a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "use-symbols" Nothing).emitXml) b])
        []
parseMultipleRest :: P.XParse MultipleRest
parseMultipleRest = 
      MultipleRest
        <$> (P.xtext >>= parsePositiveIntegerOrEmpty)
        <*> P.optional (P.xattr (P.name "use-symbols") >>= parseYesNo)

-- | Smart constructor for 'MultipleRest'
mkMultipleRest :: PositiveIntegerOrEmpty -> MultipleRest
mkMultipleRest a = MultipleRest a Nothing

-- | @name-display@ /(complex)/
--
-- The name-display type is used for exact formatting of multi-font text in part and group names to the left of the system. The print-object attribute can be used to determine what, if anything, is printed at the start of each system. Enclosure for the display-text element is none by default. Language for the display-text element is Italian ("it") by default.
data NameDisplay = 
      NameDisplay {
          nameDisplayPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , nameDisplayNameDisplay :: [ChxNameDisplay]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NameDisplay where
    emitXml (NameDisplay a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) a])
        ([emitXml b])
parseNameDisplay :: P.XParse NameDisplay
parseNameDisplay = 
      NameDisplay
        <$> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.many (parseChxNameDisplay)

-- | Smart constructor for 'NameDisplay'
mkNameDisplay :: NameDisplay
mkNameDisplay = NameDisplay Nothing []

-- | @non-arpeggiate@ /(complex)/
--
-- The non-arpeggiate type indicates that this note is at the top or bottom of a bracket indicating to not arpeggiate these notes. Since this does not involve playback, it is only used on the top or bottom notes, not on each note as for the arpeggiate type.
data NonArpeggiate = 
      NonArpeggiate {
          nonArpeggiateType :: TopBottom -- ^ /type/ attribute
        , nonArpeggiateNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , nonArpeggiateDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , nonArpeggiateDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , nonArpeggiateRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , nonArpeggiateRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , nonArpeggiatePlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , nonArpeggiateColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NonArpeggiate where
    emitXml (NonArpeggiate a b c d e f g h) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
parseNonArpeggiate :: P.XParse NonArpeggiate
parseNonArpeggiate = 
      NonArpeggiate
        <$> (P.xattr (P.name "type") >>= parseTopBottom)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'NonArpeggiate'
mkNonArpeggiate :: TopBottom -> NonArpeggiate
mkNonArpeggiate a = NonArpeggiate a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @notations@ /(complex)/
--
-- Notations refer to musical notations, not XML notations. Multiple notations are allowed in order to represent multiple editorial levels. The set of notations may be refined and expanded over time, especially to handle more instrument-specific technical notations.
data Notations = 
      Notations {
          notationsEditorial :: Editorial
        , notationsNotations :: [ChxNotations]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Notations where
    emitXml (Notations a b) =
      XReps [emitXml a,emitXml b]
parseNotations :: P.XParse Notations
parseNotations = 
      Notations
        <$> parseEditorial
        <*> P.many (parseChxNotations)

-- | Smart constructor for 'Notations'
mkNotations :: Editorial -> Notations
mkNotations a = Notations a []

-- | @note@ /(complex)/
--
-- Notes are the most common type of MusicXML data. The MusicXML format keeps the MuseData distinction between elements used for sound information and elements used for notation information (e.g., tie is used for sound, tied for notation). Thus grace notes do not have a duration element. Cue notes have a duration element, as do forward elements, but no tie elements. Having these two types of information available can make interchange considerably easier, as some programs handle one type of information much more readily than the other. 
-- 	
-- The dynamics and end-dynamics attributes correspond to MIDI 1.0's Note On and Note Off velocities, respectively. They are expressed in terms of percentages of the default forte value (90 for MIDI 1.0). The attack and release attributes are used to alter the staring and stopping time of the note from when it would otherwise occur based on the flow of durations - information that is specific to a performance. They are expressed in terms of divisions, either positive or negative. A note that starts a tie should not have a release attribute, and a note that stops a tie should not have an attack attribute. If a note is played only one time through a repeat, the time-only attribute shows which time to play the note. The pizzicato attribute is used when just this note is sounded pizzicato, vs. the pizzicato element which changes overall playback between pizzicato and arco.
data Note = 
      Note {
          noteDynamics :: (Maybe NonNegativeDecimal) -- ^ /dynamics/ attribute
        , noteEndDynamics :: (Maybe NonNegativeDecimal) -- ^ /end-dynamics/ attribute
        , noteAttack :: (Maybe Divisions) -- ^ /attack/ attribute
        , noteRelease :: (Maybe Divisions) -- ^ /release/ attribute
        , noteTimeOnly :: (Maybe Token) -- ^ /time-only/ attribute
        , notePizzicato :: (Maybe YesNo) -- ^ /pizzicato/ attribute
        , noteDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , noteDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , noteRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , noteRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , noteFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , noteFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , noteFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , noteFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , noteColor :: (Maybe Color) -- ^ /color/ attribute
        , notePrintDot :: (Maybe YesNo) -- ^ /print-dot/ attribute
        , notePrintLyric :: (Maybe YesNo) -- ^ /print-lyric/ attribute
        , notePrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , notePrintSpacing :: (Maybe YesNo) -- ^ /print-spacing/ attribute
        , noteNote :: ChxNote
        , noteInstrument :: (Maybe Instrument) -- ^ /instrument/ child element
        , noteEditorialVoice :: EditorialVoice
        , noteType :: (Maybe NoteType) -- ^ /type/ child element
        , noteDot :: [EmptyPlacement] -- ^ /dot/ child element
        , noteAccidental :: (Maybe Accidental) -- ^ /accidental/ child element
        , noteTimeModification :: (Maybe TimeModification) -- ^ /time-modification/ child element
        , noteStem :: (Maybe Stem) -- ^ /stem/ child element
        , noteNotehead :: (Maybe Notehead) -- ^ /notehead/ child element
        , noteStaff :: (Maybe Staff)
        , noteBeam :: [Beam] -- ^ /beam/ child element
        , noteNotations :: [Notations] -- ^ /notations/ child element
        , noteLyric :: [Lyric] -- ^ /lyric/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Note where
    emitXml (Note a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "dynamics" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "end-dynamics" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "attack" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "release" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "time-only" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "pizzicato" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "print-dot" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "print-lyric" Nothing).emitXml) q]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) r]++[maybe XEmpty (XAttr (QN "print-spacing" Nothing).emitXml) s])
        ([emitXml t]++[maybe XEmpty (XElement (QN "instrument" Nothing).emitXml) u]++[emitXml v]++[maybe XEmpty (XElement (QN "type" Nothing).emitXml) w]++map (XElement (QN "dot" Nothing).emitXml) x++[maybe XEmpty (XElement (QN "accidental" Nothing).emitXml) y]++[maybe XEmpty (XElement (QN "time-modification" Nothing).emitXml) z]++[maybe XEmpty (XElement (QN "stem" Nothing).emitXml) a1]++[maybe XEmpty (XElement (QN "notehead" Nothing).emitXml) b1]++[emitXml c1]++map (XElement (QN "beam" Nothing).emitXml) d1++map (XElement (QN "notations" Nothing).emitXml) e1++map (XElement (QN "lyric" Nothing).emitXml) f1)
parseNote :: P.XParse Note
parseNote = 
      Note
        <$> P.optional (P.xattr (P.name "dynamics") >>= parseNonNegativeDecimal)
        <*> P.optional (P.xattr (P.name "end-dynamics") >>= parseNonNegativeDecimal)
        <*> P.optional (P.xattr (P.name "attack") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "release") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "time-only") >>= parseToken)
        <*> P.optional (P.xattr (P.name "pizzicato") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "print-dot") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-lyric") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-spacing") >>= parseYesNo)
        <*> parseChxNote
        <*> P.optional (P.xchild (P.name "instrument") (parseInstrument))
        <*> parseEditorialVoice
        <*> P.optional (P.xchild (P.name "type") (parseNoteType))
        <*> P.many (P.xchild (P.name "dot") (parseEmptyPlacement))
        <*> P.optional (P.xchild (P.name "accidental") (parseAccidental))
        <*> P.optional (P.xchild (P.name "time-modification") (parseTimeModification))
        <*> P.optional (P.xchild (P.name "stem") (parseStem))
        <*> P.optional (P.xchild (P.name "notehead") (parseNotehead))
        <*> P.optional (parseStaff)
        <*> P.many (P.xchild (P.name "beam") (parseBeam))
        <*> P.many (P.xchild (P.name "notations") (parseNotations))
        <*> P.many (P.xchild (P.name "lyric") (parseLyric))

-- | Smart constructor for 'Note'
mkNote :: ChxNote -> EditorialVoice -> Note
mkNote t v = Note Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing t Nothing v Nothing [] Nothing Nothing Nothing Nothing Nothing [] [] []

-- | @note-size@ /(complex)/
--
-- The note-size type indicates the percentage of the regular note size to use for notes with a cue and large size as defined in the type element. The grace type is used for notes of cue size that that include a grace element. The cue type is used for all other notes with cue size, whether defined explicitly or implicitly via a cue element. The large type is used for notes of large size. The text content represent the numeric percentage. A value of 100 would be identical to the size of a regular note as defined by the music font.
data NoteSize = 
      NoteSize {
          noteSizeNonNegativeDecimal :: NonNegativeDecimal -- ^ text content
        , noteSizeType :: NoteSizeType -- ^ /type/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NoteSize where
    emitXml (NoteSize a b) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)])
        []
parseNoteSize :: P.XParse NoteSize
parseNoteSize = 
      NoteSize
        <$> (P.xtext >>= parseNonNegativeDecimal)
        <*> (P.xattr (P.name "type") >>= parseNoteSizeType)

-- | Smart constructor for 'NoteSize'
mkNoteSize :: NonNegativeDecimal -> NoteSizeType -> NoteSize
mkNoteSize a b = NoteSize a b

-- | @note-type@ /(complex)/
--
-- The note-type type indicates the graphic note type. Values range from 256th to long. The size attribute indicates full, cue, or large size, with full the default for regular notes and cue the default for cue and grace notes.
data NoteType = 
      NoteType {
          noteTypeNoteTypeValue :: NoteTypeValue -- ^ text content
        , noteTypeSize :: (Maybe SymbolSize) -- ^ /size/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NoteType where
    emitXml (NoteType a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "size" Nothing).emitXml) b])
        []
parseNoteType :: P.XParse NoteType
parseNoteType = 
      NoteType
        <$> (P.xtext >>= parseNoteTypeValue)
        <*> P.optional (P.xattr (P.name "size") >>= parseSymbolSize)

-- | Smart constructor for 'NoteType'
mkNoteType :: NoteTypeValue -> NoteType
mkNoteType a = NoteType a Nothing

-- | @notehead@ /(complex)/
--
-- The notehead element indicates shapes other than the open and closed ovals associated with note durations.
-- 	
-- For the enclosed shapes, the default is to be hollow for half notes and longer, and filled otherwise. The filled attribute can be set to change this if needed.
-- 	
-- If the parentheses attribute is set to yes, the notehead is parenthesized. It is no by default.
data Notehead = 
      Notehead {
          noteheadNoteheadValue :: NoteheadValue -- ^ text content
        , noteheadFilled :: (Maybe YesNo) -- ^ /filled/ attribute
        , noteheadParentheses :: (Maybe YesNo) -- ^ /parentheses/ attribute
        , noteheadFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , noteheadFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , noteheadFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , noteheadFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , noteheadColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Notehead where
    emitXml (Notehead a b c d e f g h) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "filled" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
parseNotehead :: P.XParse Notehead
parseNotehead = 
      Notehead
        <$> (P.xtext >>= parseNoteheadValue)
        <*> P.optional (P.xattr (P.name "filled") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "parentheses") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Notehead'
mkNotehead :: NoteheadValue -> Notehead
mkNotehead a = Notehead a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @octave-shift@ /(complex)/
--
-- The octave shift type indicates where notes are shifted up or down from their true pitched values because of printing difficulty. Thus a treble clef line noted with 8va will be indicated with an octave-shift down from the pitch data indicated in the notes. A size of 8 indicates one octave; a size of 15 indicates two octaves.
data OctaveShift = 
      OctaveShift {
          octaveShiftType :: UpDownStop -- ^ /type/ attribute
        , octaveShiftNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , octaveShiftSize :: (Maybe PositiveInteger) -- ^ /size/ attribute
        , octaveShiftDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , octaveShiftDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , octaveShiftRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , octaveShiftRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , octaveShiftFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , octaveShiftFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , octaveShiftFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , octaveShiftFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , octaveShiftColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OctaveShift where
    emitXml (OctaveShift a b c d e f g h i j k l) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l])
        []
parseOctaveShift :: P.XParse OctaveShift
parseOctaveShift = 
      OctaveShift
        <$> (P.xattr (P.name "type") >>= parseUpDownStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "size") >>= parsePositiveInteger)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'OctaveShift'
mkOctaveShift :: UpDownStop -> OctaveShift
mkOctaveShift a = OctaveShift a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @offset@ /(complex)/
--
-- An offset is represented in terms of divisions, and indicates where the direction will appear relative to the current musical location. This affects the visual appearance of the direction. If the sound attribute is "yes", then the offset affects playback too. If the sound attribute is "no", then any sound associated with the direction takes effect at the current location. The sound attribute is "no" by default for compatibility with earlier versions of the MusicXML format. If an element within a direction includes a default-x attribute, the offset value will be ignored when determining the appearance of that element.
data Offset = 
      Offset {
          offsetDivisions :: Divisions -- ^ text content
        , offsetSound :: (Maybe YesNo) -- ^ /sound/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Offset where
    emitXml (Offset a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "sound" Nothing).emitXml) b])
        []
parseOffset :: P.XParse Offset
parseOffset = 
      Offset
        <$> (P.xtext >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "sound") >>= parseYesNo)

-- | Smart constructor for 'Offset'
mkOffset :: Divisions -> Offset
mkOffset a = Offset a Nothing

-- | @opus@ /(complex)/
--
-- The opus type represents a link to a MusicXML opus document that composes multiple MusicXML scores into a collection.
data Opus = 
      Opus {
          opusHref :: String -- ^ /xlink:href/ attribute
        , opusType :: (Maybe Type) -- ^ /xlink:type/ attribute
        , opusRole :: (Maybe Token) -- ^ /xlink:role/ attribute
        , opusTitle :: (Maybe Token) -- ^ /xlink:title/ attribute
        , opusShow :: (Maybe SmpShow) -- ^ /xlink:show/ attribute
        , opusActuate :: (Maybe Actuate) -- ^ /xlink:actuate/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Opus where
    emitXml (Opus a b c d e f) =
      XContent XEmpty
        ([XAttr (QN "href" (Just "xlink")) (emitXml a)]++[maybe XEmpty (XAttr (QN "type" (Just "xlink")).emitXml) b]++[maybe XEmpty (XAttr (QN "role" (Just "xlink")).emitXml) c]++[maybe XEmpty (XAttr (QN "title" (Just "xlink")).emitXml) d]++[maybe XEmpty (XAttr (QN "show" (Just "xlink")).emitXml) e]++[maybe XEmpty (XAttr (QN "actuate" (Just "xlink")).emitXml) f])
        []
parseOpus :: P.XParse Opus
parseOpus = 
      Opus
        <$> (P.xattr (P.name "xlink:href") >>= return)
        <*> P.optional (P.xattr (P.name "xlink:type") >>= parseType)
        <*> P.optional (P.xattr (P.name "xlink:role") >>= parseToken)
        <*> P.optional (P.xattr (P.name "xlink:title") >>= parseToken)
        <*> P.optional (P.xattr (P.name "xlink:show") >>= parseSmpShow)
        <*> P.optional (P.xattr (P.name "xlink:actuate") >>= parseActuate)

-- | Smart constructor for 'Opus'
mkOpus :: String -> Opus
mkOpus a = Opus a Nothing Nothing Nothing Nothing Nothing

-- | @ornaments@ /(complex)/
--
-- Ornaments can be any of several types, followed optionally by accidentals. The accidental-mark element's content is represented the same as an accidental element, but with a different name to reflect the different musical meaning.
data Ornaments = 
      Ornaments {
          ornamentsOrnaments :: [SeqOrnaments]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Ornaments where
    emitXml (Ornaments a) =
      XReps [emitXml a]
parseOrnaments :: P.XParse Ornaments
parseOrnaments = 
      Ornaments
        <$> P.many (parseSeqOrnaments)

-- | Smart constructor for 'Ornaments'
mkOrnaments :: Ornaments
mkOrnaments = Ornaments []

-- | @other-appearance@ /(complex)/
--
-- The other-appearance type is used to define any graphical settings not yet in the current version of the MusicXML format. This allows extended representation, though without application interoperability.
data OtherAppearance = 
      OtherAppearance {
          otherAppearanceString :: String -- ^ text content
        , otherAppearanceType :: Token -- ^ /type/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OtherAppearance where
    emitXml (OtherAppearance a b) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)])
        []
parseOtherAppearance :: P.XParse OtherAppearance
parseOtherAppearance = 
      OtherAppearance
        <$> (P.xtext >>= return)
        <*> (P.xattr (P.name "type") >>= parseToken)

-- | Smart constructor for 'OtherAppearance'
mkOtherAppearance :: String -> Token -> OtherAppearance
mkOtherAppearance a b = OtherAppearance a b

-- | @other-direction@ /(complex)/
--
-- The other-direction type is used to define any direction symbols not yet in the current version of the MusicXML format. This allows extended representation, though without application interoperability.
data OtherDirection = 
      OtherDirection {
          otherDirectionString :: String -- ^ text content
        , otherDirectionPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , otherDirectionDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , otherDirectionDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , otherDirectionRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , otherDirectionRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , otherDirectionFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , otherDirectionFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , otherDirectionFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , otherDirectionFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , otherDirectionColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OtherDirection where
    emitXml (OtherDirection a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseOtherDirection :: P.XParse OtherDirection
parseOtherDirection = 
      OtherDirection
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'OtherDirection'
mkOtherDirection :: String -> OtherDirection
mkOtherDirection a = OtherDirection a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @other-notation@ /(complex)/
--
-- The other-notation type is used to define any notations not yet in the MusicXML format. This allows extended representation, though without application interoperability. It handles notations where more specific extension elements such as other-dynamics and other-technical are not appropriate.
data OtherNotation = 
      OtherNotation {
          otherNotationString :: String -- ^ text content
        , otherNotationType :: StartStopSingle -- ^ /type/ attribute
        , otherNotationNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , otherNotationPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , otherNotationDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , otherNotationDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , otherNotationRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , otherNotationRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , otherNotationFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , otherNotationFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , otherNotationFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , otherNotationFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , otherNotationColor :: (Maybe Color) -- ^ /color/ attribute
        , otherNotationPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OtherNotation where
    emitXml (OtherNotation a b c d e f g h i j k l m n) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) n])
        []
parseOtherNotation :: P.XParse OtherNotation
parseOtherNotation = 
      OtherNotation
        <$> (P.xtext >>= return)
        <*> (P.xattr (P.name "type") >>= parseStartStopSingle)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'OtherNotation'
mkOtherNotation :: String -> StartStopSingle -> OtherNotation
mkOtherNotation a b = OtherNotation a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @page-layout@ /(complex)/
--
-- Page layout can be defined both in score-wide defaults and in the print element. Page margins are specified either for both even and odd pages, or via separate odd and even page number values. The type is not needed when used as part of a print element. If omitted when used in the defaults element, "both" is the default.
data PageLayout = 
      PageLayout {
          pageLayoutPageLayout :: (Maybe SeqPageLayout)
        , pageLayoutPageMargins :: [PageMargins] -- ^ /page-margins/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PageLayout where
    emitXml (PageLayout a b) =
      XContent XEmpty
        []
        ([emitXml a]++map (XElement (QN "page-margins" Nothing).emitXml) b)
parsePageLayout :: P.XParse PageLayout
parsePageLayout = 
      PageLayout
        <$> P.optional (parseSeqPageLayout)
        <*> P.many (P.xchild (P.name "page-margins") (parsePageMargins))

-- | Smart constructor for 'PageLayout'
mkPageLayout :: PageLayout
mkPageLayout = PageLayout Nothing []

-- | @page-margins@ /(complex)/
--
-- Page margins are specified either for both even and odd pages, or via separate odd and even page number values. The type attribute is not needed when used as part of a print element. If omitted when the page-margins type is used in the defaults element, "both" is the default value.
data PageMargins = 
      PageMargins {
          pageMarginsType :: (Maybe MarginType) -- ^ /type/ attribute
        , pageMarginsAllMargins :: AllMargins
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PageMargins where
    emitXml (PageMargins a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) a])
        ([emitXml b])
parsePageMargins :: P.XParse PageMargins
parsePageMargins = 
      PageMargins
        <$> P.optional (P.xattr (P.name "type") >>= parseMarginType)
        <*> parseAllMargins

-- | Smart constructor for 'PageMargins'
mkPageMargins :: AllMargins -> PageMargins
mkPageMargins b = PageMargins Nothing b

-- | @part@ /(complex)/
data CmpPart = 
      CmpPart {
          partId :: IDREF -- ^ /id/ attribute
        , partMeasure :: [Measure] -- ^ /measure/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpPart where
    emitXml (CmpPart a b) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        (map (XElement (QN "measure" Nothing).emitXml) b)
parseCmpPart :: P.XParse CmpPart
parseCmpPart = 
      CmpPart
        <$> (P.xattr (P.name "id") >>= parseIDREF)
        <*> P.many (P.xchild (P.name "measure") (parseMeasure))

-- | Smart constructor for 'CmpPart'
mkCmpPart :: IDREF -> CmpPart
mkCmpPart a = CmpPart a []

-- | @part@ /(complex)/

-- mangled: 1
data Part = 
      Part {
          cmppartId :: IDREF -- ^ /id/ attribute
        , partMusicData :: MusicData
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Part where
    emitXml (Part a b) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([emitXml b])
parsePart :: P.XParse Part
parsePart = 
      Part
        <$> (P.xattr (P.name "id") >>= parseIDREF)
        <*> parseMusicData

-- | Smart constructor for 'Part'
mkPart :: IDREF -> MusicData -> Part
mkPart a b = Part a b

-- | @part-group@ /(complex)/
--
-- The part-group element indicates groupings of parts in the score, usually indicated by braces and brackets. Braces that are used for multi-staff parts should be defined in the attributes element for that part. The part-group start element appears before the first score-part in the group. The part-group stop element appears after the last score-part in the group.
-- 	
-- The number attribute is used to distinguish overlapping and nested part-groups, not the sequence of groups. As with parts, groups can have a name and abbreviation. Values for the child elements are ignored at the stop of a group. 
-- 	
-- A part-group element is not needed for a single multi-staff part. By default, multi-staff parts include a brace symbol and (if appropriate given the bar-style) common barlines. The symbol formatting for a multi-staff part can be more fully specified using the part-symbol element.
data PartGroup = 
      PartGroup {
          partGroupType :: StartStop -- ^ /type/ attribute
        , partGroupNumber :: (Maybe Token) -- ^ /number/ attribute
        , partGroupGroupName :: (Maybe GroupName) -- ^ /group-name/ child element
        , partGroupGroupNameDisplay :: (Maybe NameDisplay) -- ^ /group-name-display/ child element
        , partGroupGroupAbbreviation :: (Maybe GroupName) -- ^ /group-abbreviation/ child element
        , partGroupGroupAbbreviationDisplay :: (Maybe NameDisplay) -- ^ /group-abbreviation-display/ child element
        , partGroupGroupSymbol :: (Maybe GroupSymbol) -- ^ /group-symbol/ child element
        , partGroupGroupBarline :: (Maybe GroupBarline) -- ^ /group-barline/ child element
        , partGroupGroupTime :: (Maybe Empty) -- ^ /group-time/ child element
        , partGroupEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PartGroup where
    emitXml (PartGroup a b c d e f g h i j) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b])
        ([maybe XEmpty (XElement (QN "group-name" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "group-name-display" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "group-abbreviation" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "group-abbreviation-display" Nothing).emitXml) f]++[maybe XEmpty (XElement (QN "group-symbol" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "group-barline" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "group-time" Nothing).emitXml) i]++[emitXml j])
parsePartGroup :: P.XParse PartGroup
parsePartGroup = 
      PartGroup
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseToken)
        <*> P.optional (P.xchild (P.name "group-name") (parseGroupName))
        <*> P.optional (P.xchild (P.name "group-name-display") (parseNameDisplay))
        <*> P.optional (P.xchild (P.name "group-abbreviation") (parseGroupName))
        <*> P.optional (P.xchild (P.name "group-abbreviation-display") (parseNameDisplay))
        <*> P.optional (P.xchild (P.name "group-symbol") (parseGroupSymbol))
        <*> P.optional (P.xchild (P.name "group-barline") (parseGroupBarline))
        <*> P.optional (P.xchild (P.name "group-time") (parseEmpty))
        <*> parseEditorial

-- | Smart constructor for 'PartGroup'
mkPartGroup :: StartStop -> Editorial -> PartGroup
mkPartGroup a j = PartGroup a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing j

-- | @part-list@ /(complex)/
--
-- The part-list identifies the different musical parts in this movement. Each part has an ID that is used later within the musical data. Since parts may be encoded separately and combined later, identification elements are present at both the score and score-part levels. There must be at least one score-part, combined as desired with part-group elements that indicate braces and brackets. Parts are ordered from top to bottom in a score based on the order in which they appear in the part-list.
data PartList = 
      PartList {
          partListPartGroup :: [GrpPartGroup]
        , partListScorePart :: ScorePart
        , partListPartList :: [ChxPartList]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PartList where
    emitXml (PartList a b c) =
      XReps [emitXml a,emitXml b,emitXml c]
parsePartList :: P.XParse PartList
parsePartList = 
      PartList
        <$> P.many (parseGrpPartGroup)
        <*> parseScorePart
        <*> P.many (parseChxPartList)

-- | Smart constructor for 'PartList'
mkPartList :: ScorePart -> PartList
mkPartList b = PartList [] b []

-- | @part-name@ /(complex)/
--
-- The part-name type describes the name or abbreviation of a score-part element. Formatting attributes for the part-name element are deprecated in Version 2.0 in favor of the new part-name-display and part-abbreviation-display elements.
data PartName = 
      PartName {
          partNameString :: String -- ^ text content
        , partNameDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , partNameDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , partNameRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , partNameRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , partNameFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , partNameFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , partNameFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , partNameFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , partNameColor :: (Maybe Color) -- ^ /color/ attribute
        , partNamePrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , partNameJustify :: (Maybe LeftCenterRight) -- ^ /justify/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PartName where
    emitXml (PartName a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) l])
        []
parsePartName :: P.XParse PartName
parsePartName = 
      PartName
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "justify") >>= parseLeftCenterRight)

-- | Smart constructor for 'PartName'
mkPartName :: String -> PartName
mkPartName a = PartName a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @part-symbol@ /(complex)/
--
-- The part-symbol element indicates how a symbol for a multi-staff part is indicated in the score. Values include none, brace, line, and bracket; brace is the default. The top-staff and bottom-staff elements are used when the brace does not extend across the entire part. For example, in a 3-staff organ part, the top-staff will typically be 1 for the right hand, while the bottom-staff will typically be 2 for the left hand. Staff 3 for the pedals is usually outside the brace.
data PartSymbol = 
      PartSymbol {
          partSymbolGroupSymbolValue :: GroupSymbolValue -- ^ text content
        , partSymbolTopStaff :: (Maybe StaffNumber) -- ^ /top-staff/ attribute
        , partSymbolBottomStaff :: (Maybe StaffNumber) -- ^ /bottom-staff/ attribute
        , partSymbolDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , partSymbolDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , partSymbolRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , partSymbolRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , partSymbolColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PartSymbol where
    emitXml (PartSymbol a b c d e f g h) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "top-staff" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "bottom-staff" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
parsePartSymbol :: P.XParse PartSymbol
parsePartSymbol = 
      PartSymbol
        <$> (P.xtext >>= parseGroupSymbolValue)
        <*> P.optional (P.xattr (P.name "top-staff") >>= parseStaffNumber)
        <*> P.optional (P.xattr (P.name "bottom-staff") >>= parseStaffNumber)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'PartSymbol'
mkPartSymbol :: GroupSymbolValue -> PartSymbol
mkPartSymbol a = PartSymbol a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @pedal@ /(complex)/
--
-- The pedal type represents piano pedal marks. The line attribute is yes if pedal lines are used, no if Ped and * signs are used. The change type is used with line set to yes.
data Pedal = 
      Pedal {
          pedalType :: StartStopChange -- ^ /type/ attribute
        , pedalLine :: (Maybe YesNo) -- ^ /line/ attribute
        , pedalDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , pedalDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , pedalRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , pedalRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , pedalFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , pedalFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , pedalFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , pedalFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , pedalColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Pedal where
    emitXml (Pedal a b c d e f g h i j k) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "line" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parsePedal :: P.XParse Pedal
parsePedal = 
      Pedal
        <$> (P.xattr (P.name "type") >>= parseStartStopChange)
        <*> P.optional (P.xattr (P.name "line") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Pedal'
mkPedal :: StartStopChange -> Pedal
mkPedal a = Pedal a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @pedal-tuning@ /(complex)/
--
-- The pedal-tuning type specifies the tuning of a single harp pedal.
data PedalTuning = 
      PedalTuning {
          pedalTuningPedalStep :: Step -- ^ /pedal-step/ child element
        , pedalTuningPedalAlter :: Semitones -- ^ /pedal-alter/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PedalTuning where
    emitXml (PedalTuning a b) =
      XContent XEmpty
        []
        ([XElement (QN "pedal-step" Nothing) (emitXml a)]++[XElement (QN "pedal-alter" Nothing) (emitXml b)])
parsePedalTuning :: P.XParse PedalTuning
parsePedalTuning = 
      PedalTuning
        <$> (P.xchild (P.name "pedal-step") (P.xtext >>= parseStep))
        <*> (P.xchild (P.name "pedal-alter") (P.xtext >>= parseSemitones))

-- | Smart constructor for 'PedalTuning'
mkPedalTuning :: Step -> Semitones -> PedalTuning
mkPedalTuning a b = PedalTuning a b

-- | @per-minute@ /(complex)/
--
-- The per-minute type can be a number, or a text description including numbers. If a font is specified, it overrides the font specified for the overall metronome element. This allows separate specification of a music font for the beat-unit and a text font for the numeric value, in cases where a single metronome font is not used.
data PerMinute = 
      PerMinute {
          perMinuteString :: String -- ^ text content
        , perMinuteFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , perMinuteFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , perMinuteFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , perMinuteFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PerMinute where
    emitXml (PerMinute a b c d e) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e])
        []
parsePerMinute :: P.XParse PerMinute
parsePerMinute = 
      PerMinute
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)

-- | Smart constructor for 'PerMinute'
mkPerMinute :: String -> PerMinute
mkPerMinute a = PerMinute a Nothing Nothing Nothing Nothing

-- | @pitch@ /(complex)/
--
-- Pitch is represented as a combination of the step of the diatonic scale, the chromatic alteration, and the octave.
data Pitch = 
      Pitch {
          pitchStep :: Step -- ^ /step/ child element
        , pitchAlter :: (Maybe Semitones) -- ^ /alter/ child element
        , pitchOctave :: Octave -- ^ /octave/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Pitch where
    emitXml (Pitch a b c) =
      XContent XEmpty
        []
        ([XElement (QN "step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "alter" Nothing).emitXml) b]++[XElement (QN "octave" Nothing) (emitXml c)])
parsePitch :: P.XParse Pitch
parsePitch = 
      Pitch
        <$> (P.xchild (P.name "step") (P.xtext >>= parseStep))
        <*> P.optional (P.xchild (P.name "alter") (P.xtext >>= parseSemitones))
        <*> (P.xchild (P.name "octave") (P.xtext >>= parseOctave))

-- | Smart constructor for 'Pitch'
mkPitch :: Step -> Octave -> Pitch
mkPitch a c = Pitch a Nothing c

-- | @placement-text@ /(complex)/
--
-- The placement-text type represents a text element with print-style and placement attribute groups.
data PlacementText = 
      PlacementText {
          placementTextString :: String -- ^ text content
        , placementTextDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , placementTextDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , placementTextRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , placementTextRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , placementTextFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , placementTextFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , placementTextFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , placementTextFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , placementTextColor :: (Maybe Color) -- ^ /color/ attribute
        , placementTextPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PlacementText where
    emitXml (PlacementText a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        []
parsePlacementText :: P.XParse PlacementText
parsePlacementText = 
      PlacementText
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'PlacementText'
mkPlacementText :: String -> PlacementText
mkPlacementText a = PlacementText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @print@ /(complex)/
--
-- The print type contains general printing parameters, including the layout elements defined in the layout.mod file. The part-name-display and part-abbreviation-display elements used in the score.mod file may also be used here to change how a part name or abbreviation is displayed over the course of a piece. They take effect when the current measure or a succeeding measure starts a new system.
-- 	
-- Layout elements in a print statement only apply to the current page, system, staff, or measure. Music that follows continues to take the default values from the layout included in the defaults element.
data Print = 
      Print {
          printStaffSpacing :: (Maybe Tenths) -- ^ /staff-spacing/ attribute
        , printNewSystem :: (Maybe YesNo) -- ^ /new-system/ attribute
        , printNewPage :: (Maybe YesNo) -- ^ /new-page/ attribute
        , printBlankPage :: (Maybe PositiveInteger) -- ^ /blank-page/ attribute
        , printPageNumber :: (Maybe Token) -- ^ /page-number/ attribute
        , printLayout :: Layout
        , printMeasureLayout :: (Maybe MeasureLayout) -- ^ /measure-layout/ child element
        , printMeasureNumbering :: (Maybe MeasureNumbering) -- ^ /measure-numbering/ child element
        , printPartNameDisplay :: (Maybe NameDisplay) -- ^ /part-name-display/ child element
        , printPartAbbreviationDisplay :: (Maybe NameDisplay) -- ^ /part-abbreviation-display/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Print where
    emitXml (Print a b c d e f g h i j) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "staff-spacing" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "new-system" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "new-page" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "blank-page" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "page-number" Nothing).emitXml) e])
        ([emitXml f]++[maybe XEmpty (XElement (QN "measure-layout" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "measure-numbering" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "part-name-display" Nothing).emitXml) i]++[maybe XEmpty (XElement (QN "part-abbreviation-display" Nothing).emitXml) j])
parsePrint :: P.XParse Print
parsePrint = 
      Print
        <$> P.optional (P.xattr (P.name "staff-spacing") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "new-system") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "new-page") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "blank-page") >>= parsePositiveInteger)
        <*> P.optional (P.xattr (P.name "page-number") >>= parseToken)
        <*> parseLayout
        <*> P.optional (P.xchild (P.name "measure-layout") (parseMeasureLayout))
        <*> P.optional (P.xchild (P.name "measure-numbering") (parseMeasureNumbering))
        <*> P.optional (P.xchild (P.name "part-name-display") (parseNameDisplay))
        <*> P.optional (P.xchild (P.name "part-abbreviation-display") (parseNameDisplay))

-- | Smart constructor for 'Print'
mkPrint :: Layout -> Print
mkPrint f = Print Nothing Nothing Nothing Nothing Nothing f Nothing Nothing Nothing Nothing

-- | @rehearsal@ /(complex)/
--
-- The rehearsal type specifies a rehearsal mark. Language is Italian ("it") by default. Enclosure is square by default.
data Rehearsal = 
      Rehearsal {
          rehearsalString :: String -- ^ text content
        , rehearsalLang :: (Maybe Lang) -- ^ /xml:lang/ attribute
        , rehearsalEnclosure :: (Maybe RehearsalEnclosure) -- ^ /enclosure/ attribute
        , rehearsalDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , rehearsalDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , rehearsalRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , rehearsalRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , rehearsalFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , rehearsalFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , rehearsalFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , rehearsalFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , rehearsalColor :: (Maybe Color) -- ^ /color/ attribute
        , rehearsalUnderline :: (Maybe NumberOfLines) -- ^ /underline/ attribute
        , rehearsalOverline :: (Maybe NumberOfLines) -- ^ /overline/ attribute
        , rehearsalLineThrough :: (Maybe NumberOfLines) -- ^ /line-through/ attribute
        , rehearsalDir :: (Maybe TextDirection) -- ^ /dir/ attribute
        , rehearsalRotation :: (Maybe RotationDegrees) -- ^ /rotation/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Rehearsal where
    emitXml (Rehearsal a b c d e f g h i j k l m n o p q) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "enclosure" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) q])
        []
parseRehearsal :: P.XParse Rehearsal
parseRehearsal = 
      Rehearsal
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "xml:lang") >>= parseLang)
        <*> P.optional (P.xattr (P.name "enclosure") >>= parseRehearsalEnclosure)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "underline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "overline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "line-through") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "dir") >>= parseTextDirection)
        <*> P.optional (P.xattr (P.name "rotation") >>= parseRotationDegrees)

-- | Smart constructor for 'Rehearsal'
mkRehearsal :: String -> Rehearsal
mkRehearsal a = Rehearsal a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @repeat@ /(complex)/
--
-- The repeat type represents repeat marks. The start of the repeat has a forward direction while the end of the repeat has a backward direction. Backward repeats that are not part of an ending can use the times attribute to indicate the number of times the repeated section is played.
data Repeat = 
      Repeat {
          repeatDirection :: BackwardForward -- ^ /direction/ attribute
        , repeatTimes :: (Maybe NonNegativeInteger) -- ^ /times/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Repeat where
    emitXml (Repeat a b) =
      XContent XEmpty
        ([XAttr (QN "direction" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "times" Nothing).emitXml) b])
        []
parseRepeat :: P.XParse Repeat
parseRepeat = 
      Repeat
        <$> (P.xattr (P.name "direction") >>= parseBackwardForward)
        <*> P.optional (P.xattr (P.name "times") >>= parseNonNegativeInteger)

-- | Smart constructor for 'Repeat'
mkRepeat :: BackwardForward -> Repeat
mkRepeat a = Repeat a Nothing

-- | @root@ /(complex)/
--
-- The root type indicates a pitch like C, D, E vs. a function indication like I, II, III. It is used with chord symbols in popular music. The root element has a root-step and optional root-alter element similar to the step and alter elements, but renamed to distinguish the different musical meanings.
data Root = 
      Root {
          rootRootStep :: RootStep -- ^ /root-step/ child element
        , rootRootAlter :: (Maybe RootAlter) -- ^ /root-alter/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Root where
    emitXml (Root a b) =
      XContent XEmpty
        []
        ([XElement (QN "root-step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "root-alter" Nothing).emitXml) b])
parseRoot :: P.XParse Root
parseRoot = 
      Root
        <$> (P.xchild (P.name "root-step") (parseRootStep))
        <*> P.optional (P.xchild (P.name "root-alter") (parseRootAlter))

-- | Smart constructor for 'Root'
mkRoot :: RootStep -> Root
mkRoot a = Root a Nothing

-- | @root-alter@ /(complex)/
--
-- The root-alter type represents the chromatic alteration of the root of the current chord within the harmony element. In some chord styles, the text for the root-step element may include root-alter information. In that case, the print-object attribute of the root-alter element can be set to no. The location attribute indicates whether the alteration should appear to the left or the right of the root-step; it is right by default.
data RootAlter = 
      RootAlter {
          rootAlterSemitones :: Semitones -- ^ text content
        , rootAlterLocation :: (Maybe LeftRight) -- ^ /location/ attribute
        , rootAlterPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , rootAlterDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , rootAlterDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , rootAlterRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , rootAlterRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , rootAlterFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , rootAlterFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , rootAlterFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , rootAlterFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , rootAlterColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml RootAlter where
    emitXml (RootAlter a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l])
        []
parseRootAlter :: P.XParse RootAlter
parseRootAlter = 
      RootAlter
        <$> (P.xtext >>= parseSemitones)
        <*> P.optional (P.xattr (P.name "location") >>= parseLeftRight)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'RootAlter'
mkRootAlter :: Semitones -> RootAlter
mkRootAlter a = RootAlter a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @root-step@ /(complex)/
--
-- The root-step type represents the pitch step of the root of the current chord within the harmony element. The text attribute indicates how the root should appear on the page if not using the element contents.
data RootStep = 
      RootStep {
          rootStepStep :: Step -- ^ text content
        , rootStepText :: (Maybe Token) -- ^ /text/ attribute
        , rootStepDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , rootStepDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , rootStepRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , rootStepRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , rootStepFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , rootStepFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , rootStepFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , rootStepFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , rootStepColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml RootStep where
    emitXml (RootStep a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
parseRootStep :: P.XParse RootStep
parseRootStep = 
      RootStep
        <$> (P.xtext >>= parseStep)
        <*> P.optional (P.xattr (P.name "text") >>= parseToken)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'RootStep'
mkRootStep :: Step -> RootStep
mkRootStep a = RootStep a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @scaling@ /(complex)/
--
-- Margins, page sizes, and distances are all measured in tenths to keep MusicXML data in a consistent coordinate system as much as possible. The translation to absolute units is done with the scaling type, which specifies how many millimeters are equal to how many tenths. For a staff height of 7 mm, millimeters would be set to 7 while tenths is set to 40. The ability to set a formula rather than a single scaling factor helps avoid roundoff errors.
data Scaling = 
      Scaling {
          scalingMillimeters :: Millimeters -- ^ /millimeters/ child element
        , scalingTenths :: Tenths -- ^ /tenths/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Scaling where
    emitXml (Scaling a b) =
      XContent XEmpty
        []
        ([XElement (QN "millimeters" Nothing) (emitXml a)]++[XElement (QN "tenths" Nothing) (emitXml b)])
parseScaling :: P.XParse Scaling
parseScaling = 
      Scaling
        <$> (P.xchild (P.name "millimeters") (P.xtext >>= parseMillimeters))
        <*> (P.xchild (P.name "tenths") (P.xtext >>= parseTenths))

-- | Smart constructor for 'Scaling'
mkScaling :: Millimeters -> Tenths -> Scaling
mkScaling a b = Scaling a b

-- | @scordatura@ /(complex)/
--
-- Scordatura string tunings are represented by a series of accord elements, similar to the staff-tuning elements. Strings are numbered from high to low.
data Scordatura = 
      Scordatura {
          scordaturaAccord :: [Accord] -- ^ /accord/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Scordatura where
    emitXml (Scordatura a) =
      XContent XEmpty
        []
        (map (XElement (QN "accord" Nothing).emitXml) a)
parseScordatura :: P.XParse Scordatura
parseScordatura = 
      Scordatura
        <$> P.many (P.xchild (P.name "accord") (parseAccord))

-- | Smart constructor for 'Scordatura'
mkScordatura :: Scordatura
mkScordatura = Scordatura []

-- | @score-instrument@ /(complex)/
--
-- The score-instrument type represents a single instrument within a score-part. As with the score-part type, each score-instrument has a required ID attribute, a name, and an optional abbreviation.
-- 	
-- A score-instrument type is also required if the score specifies MIDI 1.0 channels, banks, or programs. An initial midi-instrument assignment can also be made here. MusicXML software should be able to automatically assign reasonable channels and instruments without these elements in simple cases, such as where part names match General MIDI instrument names.
data ScoreInstrument = 
      ScoreInstrument {
          scoreInstrumentId :: ID -- ^ /id/ attribute
        , scoreInstrumentInstrumentName :: String -- ^ /instrument-name/ child element
        , scoreInstrumentInstrumentAbbreviation :: (Maybe String) -- ^ /instrument-abbreviation/ child element
        , scoreInstrumentScoreInstrument :: (Maybe ChxScoreInstrument)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScoreInstrument where
    emitXml (ScoreInstrument a b c d) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([XElement (QN "instrument-name" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "instrument-abbreviation" Nothing).emitXml) c]++[emitXml d])
parseScoreInstrument :: P.XParse ScoreInstrument
parseScoreInstrument = 
      ScoreInstrument
        <$> (P.xattr (P.name "id") >>= parseID)
        <*> (P.xchild (P.name "instrument-name") (P.xtext >>= return))
        <*> P.optional (P.xchild (P.name "instrument-abbreviation") (P.xtext >>= return))
        <*> P.optional (parseChxScoreInstrument)

-- | Smart constructor for 'ScoreInstrument'
mkScoreInstrument :: ID -> String -> ScoreInstrument
mkScoreInstrument a b = ScoreInstrument a b Nothing Nothing

-- | @score-part@ /(complex)/
--
-- Each MusicXML part corresponds to a track in a Standard MIDI Format 1 file. The score-instrument elements are used when there are multiple instruments per track. The midi-device element is used to make a MIDI device or port assignment for the given track. Initial midi-instrument assignments may be made here as well.
data CmpScorePart = 
      CmpScorePart {
          scorePartId :: ID -- ^ /id/ attribute
        , scorePartIdentification :: (Maybe Identification) -- ^ /identification/ child element
        , scorePartPartName :: PartName -- ^ /part-name/ child element
        , scorePartPartNameDisplay :: (Maybe NameDisplay) -- ^ /part-name-display/ child element
        , scorePartPartAbbreviation :: (Maybe PartName) -- ^ /part-abbreviation/ child element
        , scorePartPartAbbreviationDisplay :: (Maybe NameDisplay) -- ^ /part-abbreviation-display/ child element
        , scorePartGroup :: [String] -- ^ /group/ child element
        , scorePartScoreInstrument :: [ScoreInstrument] -- ^ /score-instrument/ child element
        , scorePartMidiDevice :: (Maybe MidiDevice) -- ^ /midi-device/ child element
        , scorePartMidiInstrument :: [MidiInstrument] -- ^ /midi-instrument/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpScorePart where
    emitXml (CmpScorePart a b c d e f g h i j) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([maybe XEmpty (XElement (QN "identification" Nothing).emitXml) b]++[XElement (QN "part-name" Nothing) (emitXml c)]++[maybe XEmpty (XElement (QN "part-name-display" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "part-abbreviation" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "part-abbreviation-display" Nothing).emitXml) f]++map (XElement (QN "group" Nothing).emitXml) g++map (XElement (QN "score-instrument" Nothing).emitXml) h++[maybe XEmpty (XElement (QN "midi-device" Nothing).emitXml) i]++map (XElement (QN "midi-instrument" Nothing).emitXml) j)
parseCmpScorePart :: P.XParse CmpScorePart
parseCmpScorePart = 
      CmpScorePart
        <$> (P.xattr (P.name "id") >>= parseID)
        <*> P.optional (P.xchild (P.name "identification") (parseIdentification))
        <*> (P.xchild (P.name "part-name") (parsePartName))
        <*> P.optional (P.xchild (P.name "part-name-display") (parseNameDisplay))
        <*> P.optional (P.xchild (P.name "part-abbreviation") (parsePartName))
        <*> P.optional (P.xchild (P.name "part-abbreviation-display") (parseNameDisplay))
        <*> P.many (P.xchild (P.name "group") (P.xtext >>= return))
        <*> P.many (P.xchild (P.name "score-instrument") (parseScoreInstrument))
        <*> P.optional (P.xchild (P.name "midi-device") (parseMidiDevice))
        <*> P.many (P.xchild (P.name "midi-instrument") (parseMidiInstrument))

-- | Smart constructor for 'CmpScorePart'
mkCmpScorePart :: ID -> PartName -> CmpScorePart
mkCmpScorePart a c = CmpScorePart a Nothing c Nothing Nothing Nothing [] [] Nothing []

-- | @score-partwise@ /(complex)/
data ScorePartwise = 
      ScorePartwise {
          scorePartwiseVersion :: (Maybe Token) -- ^ /version/ attribute
        , scorePartwiseScoreHeader :: ScoreHeader
        , scorePartwisePart :: [CmpPart] -- ^ /part/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScorePartwise where
    emitXml (ScorePartwise a b c) =
      XElement (QN "score-partwise" Nothing) $ XContent XEmpty
        ([maybe XEmpty (XAttr (QN "version" Nothing).emitXml) a])
        ([emitXml b]++map (XElement (QN "part" Nothing).emitXml) c)
parseScorePartwise :: P.XParse ScorePartwise
parseScorePartwise = 
      ScorePartwise
        <$> P.optional (P.xattr (P.name "version") >>= parseToken)
        <*> parseScoreHeader
        <*> P.many (P.xchild (P.name "part") (parseCmpPart))

-- | Smart constructor for 'ScorePartwise'
mkScorePartwise :: ScoreHeader -> ScorePartwise
mkScorePartwise b = ScorePartwise Nothing b []

-- | @score-timewise@ /(complex)/
data ScoreTimewise = 
      ScoreTimewise {
          scoreTimewiseVersion :: (Maybe Token) -- ^ /version/ attribute
        , scoreTimewiseScoreHeader :: ScoreHeader
        , scoreTimewiseMeasure :: [CmpMeasure] -- ^ /measure/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScoreTimewise where
    emitXml (ScoreTimewise a b c) =
      XElement (QN "score-timewise" Nothing) $ XContent XEmpty
        ([maybe XEmpty (XAttr (QN "version" Nothing).emitXml) a])
        ([emitXml b]++map (XElement (QN "measure" Nothing).emitXml) c)
parseScoreTimewise :: P.XParse ScoreTimewise
parseScoreTimewise = 
      ScoreTimewise
        <$> P.optional (P.xattr (P.name "version") >>= parseToken)
        <*> parseScoreHeader
        <*> P.many (P.xchild (P.name "measure") (parseCmpMeasure))

-- | Smart constructor for 'ScoreTimewise'
mkScoreTimewise :: ScoreHeader -> ScoreTimewise
mkScoreTimewise b = ScoreTimewise Nothing b []

-- | @slash@ /(complex)/
--
-- The slash type is used to indicate that slash notation is to be used. If the slash is on every beat, use-stems is no (the default). To indicate rhythms but not pitches, use-stems is set to yes. The type attribute indicates whether this is the start or stop of a slash notation style. The use-dots attribute works as for the beat-repeat element, and only has effect if use-stems is no.
data CmpSlash = 
      CmpSlash {
          slashType :: StartStop -- ^ /type/ attribute
        , slashUseDots :: (Maybe YesNo) -- ^ /use-dots/ attribute
        , slashUseStems :: (Maybe YesNo) -- ^ /use-stems/ attribute
        , slashSlash :: (Maybe Slash)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpSlash where
    emitXml (CmpSlash a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "use-dots" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "use-stems" Nothing).emitXml) c])
        ([emitXml d])
parseCmpSlash :: P.XParse CmpSlash
parseCmpSlash = 
      CmpSlash
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "use-dots") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "use-stems") >>= parseYesNo)
        <*> P.optional (parseSlash)

-- | Smart constructor for 'CmpSlash'
mkCmpSlash :: StartStop -> CmpSlash
mkCmpSlash a = CmpSlash a Nothing Nothing Nothing

-- | @slide@ /(complex)/
--
-- Glissando and slide types both indicate rapidly moving from one pitch to the other so that individual notes are not discerned. The distinction is similar to that between NIFF's glissando and portamento elements. A slide is continuous between two notes and defaults to a solid line. The optional text for a is printed alongside the line.
data Slide = 
      Slide {
          slideString :: String -- ^ text content
        , slideType :: StartStop -- ^ /type/ attribute
        , slideNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , slideLineType :: (Maybe LineType) -- ^ /line-type/ attribute
        , slideDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , slideDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , slideRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , slideRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , slideFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , slideFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , slideFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , slideFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , slideColor :: (Maybe Color) -- ^ /color/ attribute
        , slideAccelerate :: (Maybe YesNo) -- ^ /accelerate/ attribute
        , slideBeats :: (Maybe TrillBeats) -- ^ /beats/ attribute
        , slideFirstBeat :: (Maybe Percent) -- ^ /first-beat/ attribute
        , slideLastBeat :: (Maybe Percent) -- ^ /last-beat/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Slide where
    emitXml (Slide a b c d e f g h i j k l m n o p q) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "first-beat" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) q])
        []
parseSlide :: P.XParse Slide
parseSlide = 
      Slide
        <$> (P.xtext >>= return)
        <*> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "line-type") >>= parseLineType)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "accelerate") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "beats") >>= parseTrillBeats)
        <*> P.optional (P.xattr (P.name "first-beat") >>= parsePercent)
        <*> P.optional (P.xattr (P.name "last-beat") >>= parsePercent)

-- | Smart constructor for 'Slide'
mkSlide :: String -> StartStop -> Slide
mkSlide a b = Slide a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @slur@ /(complex)/
--
-- Slur types are empty. Most slurs are represented with two elements: one with a start type, and one with a stop type. Slurs can add more elements using a continue type. This is typically used to specify the formatting of cross-system slurs, or to specify the shape of very complex slurs.
data Slur = 
      Slur {
          slurType :: StartStopContinue -- ^ /type/ attribute
        , slurNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , slurLineType :: (Maybe LineType) -- ^ /line-type/ attribute
        , slurDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , slurDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , slurRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , slurRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , slurPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , slurOrientation :: (Maybe OverUnder) -- ^ /orientation/ attribute
        , slurBezierOffset :: (Maybe Divisions) -- ^ /bezier-offset/ attribute
        , slurBezierOffset2 :: (Maybe Divisions) -- ^ /bezier-offset2/ attribute
        , slurBezierX :: (Maybe Tenths) -- ^ /bezier-x/ attribute
        , slurBezierY :: (Maybe Tenths) -- ^ /bezier-y/ attribute
        , slurBezierX2 :: (Maybe Tenths) -- ^ /bezier-x2/ attribute
        , slurBezierY2 :: (Maybe Tenths) -- ^ /bezier-y2/ attribute
        , slurColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Slur where
    emitXml (Slur a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "orientation" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "bezier-offset" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "bezier-offset2" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "bezier-x" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "bezier-y" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "bezier-x2" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "bezier-y2" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) p])
        []
parseSlur :: P.XParse Slur
parseSlur = 
      Slur
        <$> (P.xattr (P.name "type") >>= parseStartStopContinue)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "line-type") >>= parseLineType)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "orientation") >>= parseOverUnder)
        <*> P.optional (P.xattr (P.name "bezier-offset") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "bezier-offset2") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "bezier-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "bezier-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "bezier-x2") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "bezier-y2") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Slur'
mkSlur :: StartStopContinue -> Slur
mkSlur a = Slur a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @sound@ /(complex)/
--
-- The sound element contains general playback parameters. They can stand alone within a part/measure, or be a component element within a direction.
--
-- @
-- 	
-- Tempo is expressed in quarter notes per minute. If 0, the sound-generating program should prompt the user at the time of compiling a sound (MIDI) file.
-- 	
-- Dynamics (or MIDI velocity) are expressed as a percentage of the default forte value (90 for MIDI 1.0).
-- 	
-- Dacapo indicates to go back to the beginning of the movement. When used it always has the value "yes".
-- 	
-- Segno and dalsegno are used for backwards jumps to a segno sign; coda and tocoda are used for forward jumps to a coda sign. If there are multiple jumps, the value of these parameters can be used to name and distinguish them. If segno or coda is used, the divisions attribute can also be used to indicate the number of divisions per quarter note. Otherwise sound and MIDI generating programs may have to recompute this.
-- 	
-- By default, a dalsegno or dacapo attribute indicates that the jump should occur the first time through, while a tocoda attribute indicates the jump should occur the second time through. The time that jumps occur can be changed by using the time-only attribute.
-- 	
-- Forward-repeat is used when a forward repeat sign is implied, and usually follows a bar line. When used it always has the value of "yes".
-- 	
-- The fine attribute follows the final note or rest in a movement with a da capo or dal segno direction. If numeric, the value represents the actual duration of the final note or rest, which can be ambiguous in written notation and different among parts and voices. The value may also be "yes" to indicate no change to the final duration.
-- 	
-- If the sound element applies only one time through a repeat, the time-only attribute indicates which time to apply the sound element.
-- 	
-- Pizzicato in a sound element effects all following notes. Yes indicates pizzicato, no indicates arco.
-- 
-- The pan and elevation attributes are deprecated in Version 2.0. The pan and elevation elements in the midi-instrument element should be used instead. The meaning of the pan and elevation attributes is the same as for the pan and elevation elements. If both are present, the mid-instrument elements take priority.
-- 	
-- The damper-pedal, soft-pedal, and sostenuto-pedal attributes effect playback of the three common piano pedals and their MIDI controller equivalents. The yes value indicates the pedal is depressed; no indicates the pedal is released. A numeric value from 0 to 100 may also be used for half pedaling. This value is the percentage that the pedal is depressed. A value of 0 is equivalent to no, and a value of 100 is equivalent to yes.
-- 	
-- MIDI instruments are changed using the midi-instrument element.
-- 
-- The offset element is used to indicate that the sound takes place offset from the current score position. If the sound element is a child of a direction element, the sound offset element overrides the direction offset element if both elements are present. Note that the offset reflects the intended musical position for the change in sound. It should not be used to compensate for latency issues in particular hardware configurations.
-- @
data Sound = 
      Sound {
          soundTempo :: (Maybe NonNegativeDecimal) -- ^ /tempo/ attribute
        , soundDynamics :: (Maybe NonNegativeDecimal) -- ^ /dynamics/ attribute
        , soundDacapo :: (Maybe YesNo) -- ^ /dacapo/ attribute
        , soundSegno :: (Maybe Token) -- ^ /segno/ attribute
        , soundDalsegno :: (Maybe Token) -- ^ /dalsegno/ attribute
        , soundCoda :: (Maybe Token) -- ^ /coda/ attribute
        , soundTocoda :: (Maybe Token) -- ^ /tocoda/ attribute
        , soundDivisions :: (Maybe Divisions) -- ^ /divisions/ attribute
        , soundForwardRepeat :: (Maybe YesNo) -- ^ /forward-repeat/ attribute
        , soundFine :: (Maybe Token) -- ^ /fine/ attribute
        , soundTimeOnly :: (Maybe Token) -- ^ /time-only/ attribute
        , soundPizzicato :: (Maybe YesNo) -- ^ /pizzicato/ attribute
        , soundPan :: (Maybe RotationDegrees) -- ^ /pan/ attribute
        , soundElevation :: (Maybe RotationDegrees) -- ^ /elevation/ attribute
        , soundDamperPedal :: (Maybe YesNoNumber) -- ^ /damper-pedal/ attribute
        , soundSoftPedal :: (Maybe YesNoNumber) -- ^ /soft-pedal/ attribute
        , soundSostenutoPedal :: (Maybe YesNoNumber) -- ^ /sostenuto-pedal/ attribute
        , soundMidiInstrument :: [MidiInstrument] -- ^ /midi-instrument/ child element
        , soundOffset :: (Maybe Offset) -- ^ /offset/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Sound where
    emitXml (Sound a b c d e f g h i j k l m n o p q r s) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "tempo" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "dynamics" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "dacapo" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "segno" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "dalsegno" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "coda" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "tocoda" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "divisions" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "forward-repeat" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "fine" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "time-only" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "pizzicato" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "pan" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "elevation" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "damper-pedal" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "soft-pedal" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "sostenuto-pedal" Nothing).emitXml) q])
        (map (XElement (QN "midi-instrument" Nothing).emitXml) r++[maybe XEmpty (XElement (QN "offset" Nothing).emitXml) s])
parseSound :: P.XParse Sound
parseSound = 
      Sound
        <$> P.optional (P.xattr (P.name "tempo") >>= parseNonNegativeDecimal)
        <*> P.optional (P.xattr (P.name "dynamics") >>= parseNonNegativeDecimal)
        <*> P.optional (P.xattr (P.name "dacapo") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "segno") >>= parseToken)
        <*> P.optional (P.xattr (P.name "dalsegno") >>= parseToken)
        <*> P.optional (P.xattr (P.name "coda") >>= parseToken)
        <*> P.optional (P.xattr (P.name "tocoda") >>= parseToken)
        <*> P.optional (P.xattr (P.name "divisions") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "forward-repeat") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "fine") >>= parseToken)
        <*> P.optional (P.xattr (P.name "time-only") >>= parseToken)
        <*> P.optional (P.xattr (P.name "pizzicato") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "pan") >>= parseRotationDegrees)
        <*> P.optional (P.xattr (P.name "elevation") >>= parseRotationDegrees)
        <*> P.optional (P.xattr (P.name "damper-pedal") >>= parseYesNoNumber)
        <*> P.optional (P.xattr (P.name "soft-pedal") >>= parseYesNoNumber)
        <*> P.optional (P.xattr (P.name "sostenuto-pedal") >>= parseYesNoNumber)
        <*> P.many (P.xchild (P.name "midi-instrument") (parseMidiInstrument))
        <*> P.optional (P.xchild (P.name "offset") (parseOffset))

-- | Smart constructor for 'Sound'
mkSound :: Sound
mkSound = Sound Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

-- | @staff-details@ /(complex)/
--
-- The staff-details element is used to indicate different types of staves. The optional number attribute specifies the staff number from top to bottom on the system, as with clef. The print-object attribute is used to indicate when a staff is not printed in a part, usually in large scores where empty parts are omitted. It is yes by default. If print-spacing is yes while print-object is no, the score is printed in cutaway format where vertical space is left for the empty part.
data StaffDetails = 
      StaffDetails {
          staffDetailsNumber :: (Maybe StaffNumber) -- ^ /number/ attribute
        , staffDetailsShowFrets :: (Maybe ShowFrets) -- ^ /show-frets/ attribute
        , staffDetailsPrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , staffDetailsPrintSpacing :: (Maybe YesNo) -- ^ /print-spacing/ attribute
        , staffDetailsStaffType :: (Maybe StaffType) -- ^ /staff-type/ child element
        , staffDetailsStaffLines :: (Maybe NonNegativeInteger) -- ^ /staff-lines/ child element
        , staffDetailsStaffTuning :: [StaffTuning] -- ^ /staff-tuning/ child element
        , staffDetailsCapo :: (Maybe NonNegativeInteger) -- ^ /capo/ child element
        , staffDetailsStaffSize :: (Maybe NonNegativeDecimal) -- ^ /staff-size/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StaffDetails where
    emitXml (StaffDetails a b c d e f g h i) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "show-frets" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "print-spacing" Nothing).emitXml) d])
        ([maybe XEmpty (XElement (QN "staff-type" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "staff-lines" Nothing).emitXml) f]++map (XElement (QN "staff-tuning" Nothing).emitXml) g++[maybe XEmpty (XElement (QN "capo" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "staff-size" Nothing).emitXml) i])
parseStaffDetails :: P.XParse StaffDetails
parseStaffDetails = 
      StaffDetails
        <$> P.optional (P.xattr (P.name "number") >>= parseStaffNumber)
        <*> P.optional (P.xattr (P.name "show-frets") >>= parseShowFrets)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "print-spacing") >>= parseYesNo)
        <*> P.optional (P.xchild (P.name "staff-type") (P.xtext >>= parseStaffType))
        <*> P.optional (P.xchild (P.name "staff-lines") (P.xtext >>= parseNonNegativeInteger))
        <*> P.many (P.xchild (P.name "staff-tuning") (parseStaffTuning))
        <*> P.optional (P.xchild (P.name "capo") (P.xtext >>= parseNonNegativeInteger))
        <*> P.optional (P.xchild (P.name "staff-size") (P.xtext >>= parseNonNegativeDecimal))

-- | Smart constructor for 'StaffDetails'
mkStaffDetails :: StaffDetails
mkStaffDetails = StaffDetails Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing

-- | @staff-layout@ /(complex)/
--
-- Staff layout includes the vertical distance from the bottom line of the previous staff in this system to the top line of the staff specified by the number attribute. The optional number attribute refers to staff numbers within the part, from top to bottom on the system. A value of 1 is assumed if not present. When used in the defaults element, the values apply to all parts. This value is ignored for the first staff in a system.
data StaffLayout = 
      StaffLayout {
          staffLayoutNumber :: (Maybe StaffNumber) -- ^ /number/ attribute
        , staffLayoutStaffDistance :: (Maybe Tenths) -- ^ /staff-distance/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StaffLayout where
    emitXml (StaffLayout a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a])
        ([maybe XEmpty (XElement (QN "staff-distance" Nothing).emitXml) b])
parseStaffLayout :: P.XParse StaffLayout
parseStaffLayout = 
      StaffLayout
        <$> P.optional (P.xattr (P.name "number") >>= parseStaffNumber)
        <*> P.optional (P.xchild (P.name "staff-distance") (P.xtext >>= parseTenths))

-- | Smart constructor for 'StaffLayout'
mkStaffLayout :: StaffLayout
mkStaffLayout = StaffLayout Nothing Nothing

-- | @staff-tuning@ /(complex)/
--
-- The staff-tuning type specifies the open, non-capo tuning of the lines on a tablature staff.
data StaffTuning = 
      StaffTuning {
          staffTuningLine :: (Maybe StaffLine) -- ^ /line/ attribute
        , staffTuningTuning :: Tuning
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StaffTuning where
    emitXml (StaffTuning a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "line" Nothing).emitXml) a])
        ([emitXml b])
parseStaffTuning :: P.XParse StaffTuning
parseStaffTuning = 
      StaffTuning
        <$> P.optional (P.xattr (P.name "line") >>= parseStaffLine)
        <*> parseTuning

-- | Smart constructor for 'StaffTuning'
mkStaffTuning :: Tuning -> StaffTuning
mkStaffTuning b = StaffTuning Nothing b

-- | @stem@ /(complex)/
--
-- Stems can be down, up, none, or double. For down and up stems, the position attributes can be used to specify stem length. The relative values specify the end of the stem relative to the program default. Default values specify an absolute end stem position. Negative values of relative-y that would flip a stem instead of shortening it are ignored.
data Stem = 
      Stem {
          stemStemValue :: StemValue -- ^ text content
        , stemDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , stemDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , stemRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , stemRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , stemColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Stem where
    emitXml (Stem a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
parseStem :: P.XParse Stem
parseStem = 
      Stem
        <$> (P.xtext >>= parseStemValue)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Stem'
mkStem :: StemValue -> Stem
mkStem a = Stem a Nothing Nothing Nothing Nothing Nothing

-- | @string@ /(complex)/
--
-- The string type is used with tablature notation, regular notation (where it is often circled), and chord diagrams. String numbers start with 1 for the highest string.
data CmpString = 
      CmpString {
          stringStringNumber :: StringNumber -- ^ text content
        , stringDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , stringDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , stringRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , stringRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , stringFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , stringFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , stringFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , stringFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , stringColor :: (Maybe Color) -- ^ /color/ attribute
        , stringPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpString where
    emitXml (CmpString a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        []
parseCmpString :: P.XParse CmpString
parseCmpString = 
      CmpString
        <$> (P.xtext >>= parseStringNumber)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'CmpString'
mkCmpString :: StringNumber -> CmpString
mkCmpString a = CmpString a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @strong-accent@ /(complex)/
--
-- The strong-accent type indicates a vertical accent mark. The type attribute indicates if the point of the accent is down or up.
data StrongAccent = 
      StrongAccent {
          strongAccentEmptyPlacement :: StrongAccent
        , strongAccentType :: (Maybe UpDown) -- ^ /type/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StrongAccent where
    emitXml (StrongAccent a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b])
        ([emitXml a])
parseStrongAccent :: P.XParse StrongAccent
parseStrongAccent = 
      StrongAccent
        <$> parseStrongAccent
        <*> P.optional (P.xattr (P.name "type") >>= parseUpDown)

-- | Smart constructor for 'StrongAccent'
mkStrongAccent :: StrongAccent -> StrongAccent
mkStrongAccent a = StrongAccent a Nothing

-- | @style-text@ /(complex)/
--
-- The style-text type represents a text element with a print-style attribute group.
data StyleText = 
      StyleText {
          styleTextString :: String -- ^ text content
        , styleTextDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , styleTextDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , styleTextRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , styleTextRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , styleTextFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , styleTextFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , styleTextFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , styleTextFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , styleTextColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StyleText where
    emitXml (StyleText a b c d e f g h i j) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
parseStyleText :: P.XParse StyleText
parseStyleText = 
      StyleText
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'StyleText'
mkStyleText :: String -> StyleText
mkStyleText a = StyleText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @supports@ /(complex)/
--
-- The supports type indicates if a MusicXML encoding supports a particular MusicXML element. This is recommended for elements like beam, stem, and accidental, where the absence of an element is ambiguous if you do not know if the encoding supports that element. For Version 2.0, the supports element is expanded to allow programs to indicate support for particular attributes or particular values. This lets applications communicate, for example, that all system and/or page breaks are contained in the MusicXML file.
data Supports = 
      Supports {
          supportsType :: YesNo -- ^ /type/ attribute
        , supportsElement :: NMTOKEN -- ^ /element/ attribute
        , supportsAttribute :: (Maybe NMTOKEN) -- ^ /attribute/ attribute
        , supportsValue :: (Maybe Token) -- ^ /value/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Supports where
    emitXml (Supports a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[XAttr (QN "element" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "attribute" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "value" Nothing).emitXml) d])
        []
parseSupports :: P.XParse Supports
parseSupports = 
      Supports
        <$> (P.xattr (P.name "type") >>= parseYesNo)
        <*> (P.xattr (P.name "element") >>= parseNMTOKEN)
        <*> P.optional (P.xattr (P.name "attribute") >>= parseNMTOKEN)
        <*> P.optional (P.xattr (P.name "value") >>= parseToken)

-- | Smart constructor for 'Supports'
mkSupports :: YesNo -> NMTOKEN -> Supports
mkSupports a b = Supports a b Nothing Nothing

-- | @system-layout@ /(complex)/
--
-- System layout includes left and right margins and the vertical distance from the previous system. The system distance is measured from the bottom line of the previous system to the top line of the current system. It is ignored for the first system on a page. The top system distance is measured from the page's top margin to the top line of the first system. It is ignored for all but the first system on a page.
-- 	
-- Sometimes the sum of measure widths in a system may not equal the system width specified by the layout elements due to roundoff or other errors. The behavior when reading MusicXML files in these cases is application-dependent. For instance, applications may find that the system layout data is more reliable than the sum of the measure widths, and adjust the measure widths accordingly.
data SystemLayout = 
      SystemLayout {
          systemLayoutSystemMargins :: (Maybe SystemMargins) -- ^ /system-margins/ child element
        , systemLayoutSystemDistance :: (Maybe Tenths) -- ^ /system-distance/ child element
        , systemLayoutTopSystemDistance :: (Maybe Tenths) -- ^ /top-system-distance/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SystemLayout where
    emitXml (SystemLayout a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "system-margins" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "system-distance" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "top-system-distance" Nothing).emitXml) c])
parseSystemLayout :: P.XParse SystemLayout
parseSystemLayout = 
      SystemLayout
        <$> P.optional (P.xchild (P.name "system-margins") (parseSystemMargins))
        <*> P.optional (P.xchild (P.name "system-distance") (P.xtext >>= parseTenths))
        <*> P.optional (P.xchild (P.name "top-system-distance") (P.xtext >>= parseTenths))

-- | Smart constructor for 'SystemLayout'
mkSystemLayout :: SystemLayout
mkSystemLayout = SystemLayout Nothing Nothing Nothing

-- | @system-margins@ /(complex)/
--
-- System margins are relative to the page margins. Positive values indent and negative values reduce the margin size.
data SystemMargins = 
      SystemMargins {
          systemMarginsLeftRightMargins :: LeftRightMargins
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SystemMargins where
    emitXml (SystemMargins a) =
      XReps [emitXml a]
parseSystemMargins :: P.XParse SystemMargins
parseSystemMargins = 
      SystemMargins
        <$> parseLeftRightMargins

-- | Smart constructor for 'SystemMargins'
mkSystemMargins :: LeftRightMargins -> SystemMargins
mkSystemMargins a = SystemMargins a

-- | @technical@ /(complex)/
--
-- Technical indications give performance information for individual instruments.
data Technical = 
      Technical {
          technicalTechnical :: [ChxTechnical]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Technical where
    emitXml (Technical a) =
      XReps [emitXml a]
parseTechnical :: P.XParse Technical
parseTechnical = 
      Technical
        <$> P.many (parseChxTechnical)

-- | Smart constructor for 'Technical'
mkTechnical :: Technical
mkTechnical = Technical []

-- | @text-element-data@ /(complex)/
--
-- The text-element-data type represents a syllable or portion of a syllable for lyric text underlay. A hyphen in the string content should only be used for an actual hyphenated word. Language names for text elements come from ISO 639, with optional country subcodes from ISO 3166.
data TextElementData = 
      TextElementData {
          textElementDataString :: String -- ^ text content
        , textElementDataLang :: (Maybe Lang) -- ^ /xml:lang/ attribute
        , textElementDataFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , textElementDataFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , textElementDataFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , textElementDataFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , textElementDataColor :: (Maybe Color) -- ^ /color/ attribute
        , textElementDataUnderline :: (Maybe NumberOfLines) -- ^ /underline/ attribute
        , textElementDataOverline :: (Maybe NumberOfLines) -- ^ /overline/ attribute
        , textElementDataLineThrough :: (Maybe NumberOfLines) -- ^ /line-through/ attribute
        , textElementDataRotation :: (Maybe RotationDegrees) -- ^ /rotation/ attribute
        , textElementDataLetterSpacing :: (Maybe NumberOrNormal) -- ^ /letter-spacing/ attribute
        , textElementDataDir :: (Maybe TextDirection) -- ^ /dir/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TextElementData where
    emitXml (TextElementData a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "letter-spacing" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) m])
        []
parseTextElementData :: P.XParse TextElementData
parseTextElementData = 
      TextElementData
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "xml:lang") >>= parseLang)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "underline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "overline") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "line-through") >>= parseNumberOfLines)
        <*> P.optional (P.xattr (P.name "rotation") >>= parseRotationDegrees)
        <*> P.optional (P.xattr (P.name "letter-spacing") >>= parseNumberOrNormal)
        <*> P.optional (P.xattr (P.name "dir") >>= parseTextDirection)

-- | Smart constructor for 'TextElementData'
mkTextElementData :: String -> TextElementData
mkTextElementData a = TextElementData a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @tie@ /(complex)/
--
-- The tie element indicates that a tie begins or ends with this note. The tie element indicates sound; the tied element indicates notation.
data Tie = 
      Tie {
          tieType :: StartStop -- ^ /type/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tie where
    emitXml (Tie a) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)])
        []
parseTie :: P.XParse Tie
parseTie = 
      Tie
        <$> (P.xattr (P.name "type") >>= parseStartStop)

-- | Smart constructor for 'Tie'
mkTie :: StartStop -> Tie
mkTie a = Tie a

-- | @tied@ /(complex)/
--
-- The tied type represents the notated tie. The tie element represents the tie sound.
data Tied = 
      Tied {
          tiedType :: StartStop -- ^ /type/ attribute
        , tiedNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , tiedLineType :: (Maybe LineType) -- ^ /line-type/ attribute
        , tiedDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , tiedDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , tiedRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , tiedRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , tiedPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , tiedOrientation :: (Maybe OverUnder) -- ^ /orientation/ attribute
        , tiedBezierOffset :: (Maybe Divisions) -- ^ /bezier-offset/ attribute
        , tiedBezierOffset2 :: (Maybe Divisions) -- ^ /bezier-offset2/ attribute
        , tiedBezierX :: (Maybe Tenths) -- ^ /bezier-x/ attribute
        , tiedBezierY :: (Maybe Tenths) -- ^ /bezier-y/ attribute
        , tiedBezierX2 :: (Maybe Tenths) -- ^ /bezier-x2/ attribute
        , tiedBezierY2 :: (Maybe Tenths) -- ^ /bezier-y2/ attribute
        , tiedColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tied where
    emitXml (Tied a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "orientation" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "bezier-offset" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "bezier-offset2" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "bezier-x" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "bezier-y" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "bezier-x2" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "bezier-y2" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) p])
        []
parseTied :: P.XParse Tied
parseTied = 
      Tied
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "line-type") >>= parseLineType)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "orientation") >>= parseOverUnder)
        <*> P.optional (P.xattr (P.name "bezier-offset") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "bezier-offset2") >>= parseDivisions)
        <*> P.optional (P.xattr (P.name "bezier-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "bezier-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "bezier-x2") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "bezier-y2") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Tied'
mkTied :: StartStop -> Tied
mkTied a = Tied a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @time@ /(complex)/
--
-- Time signatures are represented by the beats element for the numerator and the beat-type element for the denominator. The symbol attribute is used indicate common and cut time symbols as well as a single number display. Multiple pairs of beat and beat-type elements are used for composite time signatures with multiple denominators, such as 2/4 + 3/8. A composite such as 3+2/8 requires only one beat/beat-type pair.
-- 
-- The print-object attribute allows a time signature to be specified but not printed, as is the case for excerpts from the middle of a score. The value is "yes" if not present. The optional number attribute refers to staff numbers within the part. If absent, the time signature applies to all staves in the part.
data Time = 
      Time {
          timeNumber :: (Maybe StaffNumber) -- ^ /number/ attribute
        , timeSymbol :: (Maybe TimeSymbol) -- ^ /symbol/ attribute
        , timeDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , timeDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , timeRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , timeRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , timeFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , timeFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , timeFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , timeFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , timeColor :: (Maybe Color) -- ^ /color/ attribute
        , timePrintObject :: (Maybe YesNo) -- ^ /print-object/ attribute
        , timeTime :: ChxTime
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Time where
    emitXml (Time a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "symbol" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) l])
        ([emitXml m])
parseTime :: P.XParse Time
parseTime = 
      Time
        <$> P.optional (P.xattr (P.name "number") >>= parseStaffNumber)
        <*> P.optional (P.xattr (P.name "symbol") >>= parseTimeSymbol)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "print-object") >>= parseYesNo)
        <*> parseChxTime

-- | Smart constructor for 'Time'
mkTime :: ChxTime -> Time
mkTime m = Time Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing m

-- | @time-modification@ /(complex)/
--
-- The time-modification type represents tuplets and other durational changes.
data TimeModification = 
      TimeModification {
          timeModificationActualNotes :: NonNegativeInteger -- ^ /actual-notes/ child element
        , timeModificationNormalNotes :: NonNegativeInteger -- ^ /normal-notes/ child element
        , timeModificationTimeModification :: (Maybe SeqTimeModification)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TimeModification where
    emitXml (TimeModification a b c) =
      XContent XEmpty
        []
        ([XElement (QN "actual-notes" Nothing) (emitXml a)]++[XElement (QN "normal-notes" Nothing) (emitXml b)]++[emitXml c])
parseTimeModification :: P.XParse TimeModification
parseTimeModification = 
      TimeModification
        <$> (P.xchild (P.name "actual-notes") (P.xtext >>= parseNonNegativeInteger))
        <*> (P.xchild (P.name "normal-notes") (P.xtext >>= parseNonNegativeInteger))
        <*> P.optional (parseSeqTimeModification)

-- | Smart constructor for 'TimeModification'
mkTimeModification :: NonNegativeInteger -> NonNegativeInteger -> TimeModification
mkTimeModification a b = TimeModification a b Nothing

-- | @transpose@ /(complex)/
--
-- The transpose type represents what must be added to a written pitch to get a correct sounding pitch.
data Transpose = 
      Transpose {
          transposeDiatonic :: (Maybe Int) -- ^ /diatonic/ child element
        , transposeChromatic :: Semitones -- ^ /chromatic/ child element
        , transposeOctaveChange :: (Maybe Int) -- ^ /octave-change/ child element
        , transposeDouble :: (Maybe Empty) -- ^ /double/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Transpose where
    emitXml (Transpose a b c d) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "diatonic" Nothing).emitXml) a]++[XElement (QN "chromatic" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "octave-change" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "double" Nothing).emitXml) d])
parseTranspose :: P.XParse Transpose
parseTranspose = 
      Transpose
        <$> P.optional (P.xchild (P.name "diatonic") (P.xtext >>= (P.xread "Integer")))
        <*> (P.xchild (P.name "chromatic") (P.xtext >>= parseSemitones))
        <*> P.optional (P.xchild (P.name "octave-change") (P.xtext >>= (P.xread "Integer")))
        <*> P.optional (P.xchild (P.name "double") (parseEmpty))

-- | Smart constructor for 'Transpose'
mkTranspose :: Semitones -> Transpose
mkTranspose b = Transpose Nothing b Nothing Nothing

-- | @tremolo@ /(complex)/
--
-- While using repeater beams was the original method for indicating tremolos, often playback and display are not well-enough integrated in an application to make that feasible. The tremolo ornament can be used to indicate either single-note or double-note tremolos. Single-note tremolos use the single type, while double-note tremolos use the start and stop types. The default is "single" for compatibility with Version 1.1. The text of the element indicates the number of tremolo marks and is an integer from 0 to 6. Note that the number of attached beams is not included in this value, but is represented separately using the beam element.
data Tremolo = 
      Tremolo {
          tremoloTremoloMarks :: TremoloMarks -- ^ text content
        , tremoloType :: (Maybe StartStopSingle) -- ^ /type/ attribute
        , tremoloDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , tremoloDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , tremoloRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , tremoloRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , tremoloFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , tremoloFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , tremoloFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , tremoloFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , tremoloColor :: (Maybe Color) -- ^ /color/ attribute
        , tremoloPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tremolo where
    emitXml (Tremolo a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) l])
        []
parseTremolo :: P.XParse Tremolo
parseTremolo = 
      Tremolo
        <$> (P.xtext >>= parseTremoloMarks)
        <*> P.optional (P.xattr (P.name "type") >>= parseStartStopSingle)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)

-- | Smart constructor for 'Tremolo'
mkTremolo :: TremoloMarks -> Tremolo
mkTremolo a = Tremolo a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @tuplet@ /(complex)/
--
-- A tuplet element is present when a tuplet is to be displayed graphically, in addition to the sound data provided by the time-modification elements. The number attribute is used to distinguish nested tuplets. The bracket attribute is used to indicate the presence of a bracket. If unspecified, the results are implementation-dependent. The line-shape attribute is used to specify whether the bracket is straight or in the older curved or slurred style. It is straight by default.
-- 	
-- Whereas a time-modification element shows how the cumulative, sounding effect of tuplets compare to the written note type, the tuplet element describes how each tuplet is displayed.
-- 	
-- The show-number attribute is used to display either the number of actual notes, the number of both actual and normal notes, or neither. It is actual by default. The show-type attribute is used to display either the actual type, both the actual and normal types, or neither. It is none by default.
data Tuplet = 
      Tuplet {
          tupletType :: StartStop -- ^ /type/ attribute
        , tupletNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , tupletBracket :: (Maybe YesNo) -- ^ /bracket/ attribute
        , tupletShowNumber :: (Maybe ShowTuplet) -- ^ /show-number/ attribute
        , tupletShowType :: (Maybe ShowTuplet) -- ^ /show-type/ attribute
        , tupletLineShape :: (Maybe LineShape) -- ^ /line-shape/ attribute
        , tupletDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , tupletDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , tupletRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , tupletRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , tupletPlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , tupletTupletActual :: (Maybe TupletPortion) -- ^ /tuplet-actual/ child element
        , tupletTupletNormal :: (Maybe TupletPortion) -- ^ /tuplet-normal/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tuplet where
    emitXml (Tuplet a b c d e f g h i j k l m) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "show-number" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "show-type" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "line-shape" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        ([maybe XEmpty (XElement (QN "tuplet-actual" Nothing).emitXml) l]++[maybe XEmpty (XElement (QN "tuplet-normal" Nothing).emitXml) m])
parseTuplet :: P.XParse Tuplet
parseTuplet = 
      Tuplet
        <$> (P.xattr (P.name "type") >>= parseStartStop)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "bracket") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "show-number") >>= parseShowTuplet)
        <*> P.optional (P.xattr (P.name "show-type") >>= parseShowTuplet)
        <*> P.optional (P.xattr (P.name "line-shape") >>= parseLineShape)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xchild (P.name "tuplet-actual") (parseTupletPortion))
        <*> P.optional (P.xchild (P.name "tuplet-normal") (parseTupletPortion))

-- | Smart constructor for 'Tuplet'
mkTuplet :: StartStop -> Tuplet
mkTuplet a = Tuplet a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @tuplet-dot@ /(complex)/
--
-- The tuplet-dot type is used to specify dotted normal tuplet types.
data TupletDot = 
      TupletDot {
          tupletDotFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , tupletDotFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , tupletDotFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , tupletDotFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , tupletDotColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletDot where
    emitXml (TupletDot a b c d e) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) e])
        []
parseTupletDot :: P.XParse TupletDot
parseTupletDot = 
      TupletDot
        <$> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'TupletDot'
mkTupletDot :: TupletDot
mkTupletDot = TupletDot Nothing Nothing Nothing Nothing Nothing

-- | @tuplet-number@ /(complex)/
--
-- The tuplet-number type indicates the number of notes for this portion of the tuplet.
data TupletNumber = 
      TupletNumber {
          tupletNumberNonNegativeInteger :: NonNegativeInteger -- ^ text content
        , tupletNumberFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , tupletNumberFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , tupletNumberFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , tupletNumberFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , tupletNumberColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletNumber where
    emitXml (TupletNumber a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
parseTupletNumber :: P.XParse TupletNumber
parseTupletNumber = 
      TupletNumber
        <$> (P.xtext >>= parseNonNegativeInteger)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'TupletNumber'
mkTupletNumber :: NonNegativeInteger -> TupletNumber
mkTupletNumber a = TupletNumber a Nothing Nothing Nothing Nothing Nothing

-- | @tuplet-portion@ /(complex)/
--
-- The tuplet-portion type provides optional full control over tuplet specifications. It allows the number and note type (including dots) to be set for the actual and normal portions of a single tuplet. If any of these elements are absent, their values are based on the time-modification element.
data TupletPortion = 
      TupletPortion {
          tupletPortionTupletNumber :: (Maybe TupletNumber) -- ^ /tuplet-number/ child element
        , tupletPortionTupletType :: (Maybe TupletType) -- ^ /tuplet-type/ child element
        , tupletPortionTupletDot :: [TupletDot] -- ^ /tuplet-dot/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletPortion where
    emitXml (TupletPortion a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "tuplet-number" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "tuplet-type" Nothing).emitXml) b]++map (XElement (QN "tuplet-dot" Nothing).emitXml) c)
parseTupletPortion :: P.XParse TupletPortion
parseTupletPortion = 
      TupletPortion
        <$> P.optional (P.xchild (P.name "tuplet-number") (parseTupletNumber))
        <*> P.optional (P.xchild (P.name "tuplet-type") (parseTupletType))
        <*> P.many (P.xchild (P.name "tuplet-dot") (parseTupletDot))

-- | Smart constructor for 'TupletPortion'
mkTupletPortion :: TupletPortion
mkTupletPortion = TupletPortion Nothing Nothing []

-- | @tuplet-type@ /(complex)/
--
-- The tuplet-type type indicates the graphical note type of the notes for this portion of the tuplet.
data TupletType = 
      TupletType {
          tupletTypeNoteTypeValue :: NoteTypeValue -- ^ text content
        , tupletTypeFontFamily :: (Maybe CommaSeparatedText) -- ^ /font-family/ attribute
        , tupletTypeFontStyle :: (Maybe FontStyle) -- ^ /font-style/ attribute
        , tupletTypeFontSize :: (Maybe FontSize) -- ^ /font-size/ attribute
        , tupletTypeFontWeight :: (Maybe FontWeight) -- ^ /font-weight/ attribute
        , tupletTypeColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletType where
    emitXml (TupletType a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
parseTupletType :: P.XParse TupletType
parseTupletType = 
      TupletType
        <$> (P.xtext >>= parseNoteTypeValue)
        <*> P.optional (P.xattr (P.name "font-family") >>= parseCommaSeparatedText)
        <*> P.optional (P.xattr (P.name "font-style") >>= parseFontStyle)
        <*> P.optional (P.xattr (P.name "font-size") >>= parseFontSize)
        <*> P.optional (P.xattr (P.name "font-weight") >>= parseFontWeight)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'TupletType'
mkTupletType :: NoteTypeValue -> TupletType
mkTupletType a = TupletType a Nothing Nothing Nothing Nothing Nothing

-- | @typed-text@ /(complex)/
--
-- The typed-text type represents a text element with a type attributes.
data TypedText = 
      TypedText {
          typedTextString :: String -- ^ text content
        , typedTextType :: (Maybe Token) -- ^ /type/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TypedText where
    emitXml (TypedText a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b])
        []
parseTypedText :: P.XParse TypedText
parseTypedText = 
      TypedText
        <$> (P.xtext >>= return)
        <*> P.optional (P.xattr (P.name "type") >>= parseToken)

-- | Smart constructor for 'TypedText'
mkTypedText :: String -> TypedText
mkTypedText a = TypedText a Nothing

-- | @wavy-line@ /(complex)/
--
-- Wavy lines are one way to indicate trills. When used with a measure element, they should always have type="continue" set.
data WavyLine = 
      WavyLine {
          wavyLineType :: StartStopContinue -- ^ /type/ attribute
        , wavyLineNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , wavyLineDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , wavyLineDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , wavyLineRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , wavyLineRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , wavyLinePlacement :: (Maybe AboveBelow) -- ^ /placement/ attribute
        , wavyLineColor :: (Maybe Color) -- ^ /color/ attribute
        , wavyLineStartNote :: (Maybe StartNote) -- ^ /start-note/ attribute
        , wavyLineTrillStep :: (Maybe TrillStep) -- ^ /trill-step/ attribute
        , wavyLineTwoNoteTurn :: (Maybe TwoNoteTurn) -- ^ /two-note-turn/ attribute
        , wavyLineAccelerate :: (Maybe YesNo) -- ^ /accelerate/ attribute
        , wavyLineBeats :: (Maybe TrillBeats) -- ^ /beats/ attribute
        , wavyLineSecondBeat :: (Maybe Percent) -- ^ /second-beat/ attribute
        , wavyLineLastBeat :: (Maybe Percent) -- ^ /last-beat/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml WavyLine where
    emitXml (WavyLine a b c d e f g h i j k l m n o) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "start-note" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "trill-step" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "two-note-turn" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "second-beat" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) o])
        []
parseWavyLine :: P.XParse WavyLine
parseWavyLine = 
      WavyLine
        <$> (P.xattr (P.name "type") >>= parseStartStopContinue)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "placement") >>= parseAboveBelow)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)
        <*> P.optional (P.xattr (P.name "start-note") >>= parseStartNote)
        <*> P.optional (P.xattr (P.name "trill-step") >>= parseTrillStep)
        <*> P.optional (P.xattr (P.name "two-note-turn") >>= parseTwoNoteTurn)
        <*> P.optional (P.xattr (P.name "accelerate") >>= parseYesNo)
        <*> P.optional (P.xattr (P.name "beats") >>= parseTrillBeats)
        <*> P.optional (P.xattr (P.name "second-beat") >>= parsePercent)
        <*> P.optional (P.xattr (P.name "last-beat") >>= parsePercent)

-- | Smart constructor for 'WavyLine'
mkWavyLine :: StartStopContinue -> WavyLine
mkWavyLine a = WavyLine a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @wedge@ /(complex)/
--
-- The wedge type represents crescendo and diminuendo wedge symbols. The type attribute is crescendo for the start of a wedge that is closed at the left side, and diminuendo for the start of a wedge that is closed on the right side. Spread values are measured in tenths; those at the start of a crescendo wedge or end of a diminuendo wedge are ignored.
data Wedge = 
      Wedge {
          wedgeType :: WedgeType -- ^ /type/ attribute
        , wedgeNumber :: (Maybe NumberLevel) -- ^ /number/ attribute
        , wedgeSpread :: (Maybe Tenths) -- ^ /spread/ attribute
        , wedgeDefaultX :: (Maybe Tenths) -- ^ /default-x/ attribute
        , wedgeDefaultY :: (Maybe Tenths) -- ^ /default-y/ attribute
        , wedgeRelativeX :: (Maybe Tenths) -- ^ /relative-x/ attribute
        , wedgeRelativeY :: (Maybe Tenths) -- ^ /relative-y/ attribute
        , wedgeColor :: (Maybe Color) -- ^ /color/ attribute
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Wedge where
    emitXml (Wedge a b c d e f g h) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "spread" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
parseWedge :: P.XParse Wedge
parseWedge = 
      Wedge
        <$> (P.xattr (P.name "type") >>= parseWedgeType)
        <*> P.optional (P.xattr (P.name "number") >>= parseNumberLevel)
        <*> P.optional (P.xattr (P.name "spread") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "default-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-x") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "relative-y") >>= parseTenths)
        <*> P.optional (P.xattr (P.name "color") >>= parseColor)

-- | Smart constructor for 'Wedge'
mkWedge :: WedgeType -> Wedge
mkWedge a = Wedge a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | @work@ /(complex)/
--
-- Works are optionally identified by number and title. The work type also may indicate a link to the opus document that composes multiple scores into a collection.
data Work = 
      Work {
          workWorkNumber :: (Maybe String) -- ^ /work-number/ child element
        , workWorkTitle :: (Maybe String) -- ^ /work-title/ child element
        , workOpus :: (Maybe Opus) -- ^ /opus/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Work where
    emitXml (Work a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "work-number" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "work-title" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "opus" Nothing).emitXml) c])
parseWork :: P.XParse Work
parseWork = 
      Work
        <$> P.optional (P.xchild (P.name "work-number") (P.xtext >>= return))
        <*> P.optional (P.xchild (P.name "work-title") (P.xtext >>= return))
        <*> P.optional (P.xchild (P.name "opus") (parseOpus))

-- | Smart constructor for 'Work'
mkWork :: Work
mkWork = Work Nothing Nothing Nothing

-- | @articulations@ /(choice)/
data ChxArticulations = 
      ArticulationsAccent {
          articulationsAccent :: EmptyPlacement -- ^ /accent/ child element
       }
    | ArticulationsStrongAccent {
          articulationsStrongAccent :: StrongAccent -- ^ /strong-accent/ child element
       }
    | ArticulationsStaccato {
          articulationsStaccato :: EmptyPlacement -- ^ /staccato/ child element
       }
    | ArticulationsTenuto {
          articulationsTenuto :: EmptyPlacement -- ^ /tenuto/ child element
       }
    | ArticulationsDetachedLegato {
          articulationsDetachedLegato :: EmptyPlacement -- ^ /detached-legato/ child element
       }
    | ArticulationsStaccatissimo {
          articulationsStaccatissimo :: EmptyPlacement -- ^ /staccatissimo/ child element
       }
    | ArticulationsSpiccato {
          articulationsSpiccato :: EmptyPlacement -- ^ /spiccato/ child element
       }
    | ArticulationsScoop {
          articulationsScoop :: EmptyLine -- ^ /scoop/ child element
       }
    | ArticulationsPlop {
          articulationsPlop :: EmptyLine -- ^ /plop/ child element
       }
    | ArticulationsDoit {
          articulationsDoit :: EmptyLine -- ^ /doit/ child element
       }
    | ArticulationsFalloff {
          articulationsFalloff :: EmptyLine -- ^ /falloff/ child element
       }
    | ArticulationsBreathMark {
          articulationsBreathMark :: EmptyPlacement -- ^ /breath-mark/ child element
       }
    | ArticulationsCaesura {
          articulationsCaesura :: EmptyPlacement -- ^ /caesura/ child element
       }
    | ArticulationsStress {
          articulationsStress :: EmptyPlacement -- ^ /stress/ child element
       }
    | ArticulationsUnstress {
          articulationsUnstress :: EmptyPlacement -- ^ /unstress/ child element
       }
    | ArticulationsOtherArticulation {
          articulationsOtherArticulation :: PlacementText -- ^ /other-articulation/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxArticulations where
    emitXml (ArticulationsAccent a) =
      XContent XEmpty
        []
        ([XElement (QN "accent" Nothing) (emitXml a)])
    emitXml (ArticulationsStrongAccent a) =
      XContent XEmpty
        []
        ([XElement (QN "strong-accent" Nothing) (emitXml a)])
    emitXml (ArticulationsStaccato a) =
      XContent XEmpty
        []
        ([XElement (QN "staccato" Nothing) (emitXml a)])
    emitXml (ArticulationsTenuto a) =
      XContent XEmpty
        []
        ([XElement (QN "tenuto" Nothing) (emitXml a)])
    emitXml (ArticulationsDetachedLegato a) =
      XContent XEmpty
        []
        ([XElement (QN "detached-legato" Nothing) (emitXml a)])
    emitXml (ArticulationsStaccatissimo a) =
      XContent XEmpty
        []
        ([XElement (QN "staccatissimo" Nothing) (emitXml a)])
    emitXml (ArticulationsSpiccato a) =
      XContent XEmpty
        []
        ([XElement (QN "spiccato" Nothing) (emitXml a)])
    emitXml (ArticulationsScoop a) =
      XContent XEmpty
        []
        ([XElement (QN "scoop" Nothing) (emitXml a)])
    emitXml (ArticulationsPlop a) =
      XContent XEmpty
        []
        ([XElement (QN "plop" Nothing) (emitXml a)])
    emitXml (ArticulationsDoit a) =
      XContent XEmpty
        []
        ([XElement (QN "doit" Nothing) (emitXml a)])
    emitXml (ArticulationsFalloff a) =
      XContent XEmpty
        []
        ([XElement (QN "falloff" Nothing) (emitXml a)])
    emitXml (ArticulationsBreathMark a) =
      XContent XEmpty
        []
        ([XElement (QN "breath-mark" Nothing) (emitXml a)])
    emitXml (ArticulationsCaesura a) =
      XContent XEmpty
        []
        ([XElement (QN "caesura" Nothing) (emitXml a)])
    emitXml (ArticulationsStress a) =
      XContent XEmpty
        []
        ([XElement (QN "stress" Nothing) (emitXml a)])
    emitXml (ArticulationsUnstress a) =
      XContent XEmpty
        []
        ([XElement (QN "unstress" Nothing) (emitXml a)])
    emitXml (ArticulationsOtherArticulation a) =
      XContent XEmpty
        []
        ([XElement (QN "other-articulation" Nothing) (emitXml a)])
parseChxArticulations :: P.XParse ChxArticulations
parseChxArticulations = 
      ArticulationsAccent
        <$> (P.xchild (P.name "accent") (parseEmptyPlacement))
      <|> ArticulationsStrongAccent
        <$> (P.xchild (P.name "strong-accent") (parseStrongAccent))
      <|> ArticulationsStaccato
        <$> (P.xchild (P.name "staccato") (parseEmptyPlacement))
      <|> ArticulationsTenuto
        <$> (P.xchild (P.name "tenuto") (parseEmptyPlacement))
      <|> ArticulationsDetachedLegato
        <$> (P.xchild (P.name "detached-legato") (parseEmptyPlacement))
      <|> ArticulationsStaccatissimo
        <$> (P.xchild (P.name "staccatissimo") (parseEmptyPlacement))
      <|> ArticulationsSpiccato
        <$> (P.xchild (P.name "spiccato") (parseEmptyPlacement))
      <|> ArticulationsScoop
        <$> (P.xchild (P.name "scoop") (parseEmptyLine))
      <|> ArticulationsPlop
        <$> (P.xchild (P.name "plop") (parseEmptyLine))
      <|> ArticulationsDoit
        <$> (P.xchild (P.name "doit") (parseEmptyLine))
      <|> ArticulationsFalloff
        <$> (P.xchild (P.name "falloff") (parseEmptyLine))
      <|> ArticulationsBreathMark
        <$> (P.xchild (P.name "breath-mark") (parseEmptyPlacement))
      <|> ArticulationsCaesura
        <$> (P.xchild (P.name "caesura") (parseEmptyPlacement))
      <|> ArticulationsStress
        <$> (P.xchild (P.name "stress") (parseEmptyPlacement))
      <|> ArticulationsUnstress
        <$> (P.xchild (P.name "unstress") (parseEmptyPlacement))
      <|> ArticulationsOtherArticulation
        <$> (P.xchild (P.name "other-articulation") (parsePlacementText))

-- | Smart constructor for 'ArticulationsAccent'
mkArticulationsAccent :: EmptyPlacement -> ChxArticulations
mkArticulationsAccent a = ArticulationsAccent a
-- | Smart constructor for 'ArticulationsStrongAccent'
mkArticulationsStrongAccent :: StrongAccent -> ChxArticulations
mkArticulationsStrongAccent a = ArticulationsStrongAccent a
-- | Smart constructor for 'ArticulationsStaccato'
mkArticulationsStaccato :: EmptyPlacement -> ChxArticulations
mkArticulationsStaccato a = ArticulationsStaccato a
-- | Smart constructor for 'ArticulationsTenuto'
mkArticulationsTenuto :: EmptyPlacement -> ChxArticulations
mkArticulationsTenuto a = ArticulationsTenuto a
-- | Smart constructor for 'ArticulationsDetachedLegato'
mkArticulationsDetachedLegato :: EmptyPlacement -> ChxArticulations
mkArticulationsDetachedLegato a = ArticulationsDetachedLegato a
-- | Smart constructor for 'ArticulationsStaccatissimo'
mkArticulationsStaccatissimo :: EmptyPlacement -> ChxArticulations
mkArticulationsStaccatissimo a = ArticulationsStaccatissimo a
-- | Smart constructor for 'ArticulationsSpiccato'
mkArticulationsSpiccato :: EmptyPlacement -> ChxArticulations
mkArticulationsSpiccato a = ArticulationsSpiccato a
-- | Smart constructor for 'ArticulationsScoop'
mkArticulationsScoop :: EmptyLine -> ChxArticulations
mkArticulationsScoop a = ArticulationsScoop a
-- | Smart constructor for 'ArticulationsPlop'
mkArticulationsPlop :: EmptyLine -> ChxArticulations
mkArticulationsPlop a = ArticulationsPlop a
-- | Smart constructor for 'ArticulationsDoit'
mkArticulationsDoit :: EmptyLine -> ChxArticulations
mkArticulationsDoit a = ArticulationsDoit a
-- | Smart constructor for 'ArticulationsFalloff'
mkArticulationsFalloff :: EmptyLine -> ChxArticulations
mkArticulationsFalloff a = ArticulationsFalloff a
-- | Smart constructor for 'ArticulationsBreathMark'
mkArticulationsBreathMark :: EmptyPlacement -> ChxArticulations
mkArticulationsBreathMark a = ArticulationsBreathMark a
-- | Smart constructor for 'ArticulationsCaesura'
mkArticulationsCaesura :: EmptyPlacement -> ChxArticulations
mkArticulationsCaesura a = ArticulationsCaesura a
-- | Smart constructor for 'ArticulationsStress'
mkArticulationsStress :: EmptyPlacement -> ChxArticulations
mkArticulationsStress a = ArticulationsStress a
-- | Smart constructor for 'ArticulationsUnstress'
mkArticulationsUnstress :: EmptyPlacement -> ChxArticulations
mkArticulationsUnstress a = ArticulationsUnstress a
-- | Smart constructor for 'ArticulationsOtherArticulation'
mkArticulationsOtherArticulation :: PlacementText -> ChxArticulations
mkArticulationsOtherArticulation a = ArticulationsOtherArticulation a

-- | @bend@ /(choice)/
data ChxBend = 
      BendPreBend {
          bendPreBend :: Empty -- ^ /pre-bend/ child element
       }
    | BendRelease {
          bendRelease :: Empty -- ^ /release/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxBend where
    emitXml (BendPreBend a) =
      XContent XEmpty
        []
        ([XElement (QN "pre-bend" Nothing) (emitXml a)])
    emitXml (BendRelease a) =
      XContent XEmpty
        []
        ([XElement (QN "release" Nothing) (emitXml a)])
parseChxBend :: P.XParse ChxBend
parseChxBend = 
      BendPreBend
        <$> (P.xchild (P.name "pre-bend") (parseEmpty))
      <|> BendRelease
        <$> (P.xchild (P.name "release") (parseEmpty))

-- | Smart constructor for 'BendPreBend'
mkBendPreBend :: Empty -> ChxBend
mkBendPreBend a = BendPreBend a
-- | Smart constructor for 'BendRelease'
mkBendRelease :: Empty -> ChxBend
mkBendRelease a = BendRelease a

-- | @credit@ /(choice)/
data ChxCredit = 
      CreditCreditImage {
          creditCreditImage :: Image -- ^ /credit-image/ child element
       }
    | CreditCreditWords {
          creditCreditWords :: FormattedText -- ^ /credit-words/ child element
        , chxcreditCredit :: [SeqCredit]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxCredit where
    emitXml (CreditCreditImage a) =
      XContent XEmpty
        []
        ([XElement (QN "credit-image" Nothing) (emitXml a)])
    emitXml (CreditCreditWords a b) =
      XContent XEmpty
        []
        ([XElement (QN "credit-words" Nothing) (emitXml a)]++[emitXml b])
parseChxCredit :: P.XParse ChxCredit
parseChxCredit = 
      CreditCreditImage
        <$> (P.xchild (P.name "credit-image") (parseImage))
      <|> CreditCreditWords
        <$> (P.xchild (P.name "credit-words") (parseFormattedText))
        <*> P.many (parseSeqCredit)

-- | Smart constructor for 'CreditCreditImage'
mkCreditCreditImage :: Image -> ChxCredit
mkCreditCreditImage a = CreditCreditImage a
-- | Smart constructor for 'CreditCreditWords'
mkCreditCreditWords :: FormattedText -> ChxCredit
mkCreditCreditWords a = CreditCreditWords a []

-- | @direction-type@ /(choice)/
data ChxDirectionType = 
      DirectionTypeRehearsal {
          directionTypeRehearsal :: [Rehearsal] -- ^ /rehearsal/ child element
       }
    | DirectionTypeSegno {
          directionTypeSegno :: [EmptyPrintStyle] -- ^ /segno/ child element
       }
    | DirectionTypeWords {
          directionTypeWords :: [FormattedText] -- ^ /words/ child element
       }
    | DirectionTypeCoda {
          directionTypeCoda :: [EmptyPrintStyle] -- ^ /coda/ child element
       }
    | DirectionTypeWedge {
          directionTypeWedge :: Wedge -- ^ /wedge/ child element
       }
    | DirectionTypeDynamics {
          directionTypeDynamics :: [Dynamics] -- ^ /dynamics/ child element
       }
    | DirectionTypeDashes {
          directionTypeDashes :: Dashes -- ^ /dashes/ child element
       }
    | DirectionTypeBracket {
          directionTypeBracket :: Bracket -- ^ /bracket/ child element
       }
    | DirectionTypePedal {
          directionTypePedal :: Pedal -- ^ /pedal/ child element
       }
    | DirectionTypeMetronome {
          directionTypeMetronome :: Metronome -- ^ /metronome/ child element
       }
    | DirectionTypeOctaveShift {
          directionTypeOctaveShift :: OctaveShift -- ^ /octave-shift/ child element
       }
    | DirectionTypeHarpPedals {
          directionTypeHarpPedals :: HarpPedals -- ^ /harp-pedals/ child element
       }
    | DirectionTypeDamp {
          directionTypeDamp :: EmptyPrintStyle -- ^ /damp/ child element
       }
    | DirectionTypeDampAll {
          directionTypeDampAll :: EmptyPrintStyle -- ^ /damp-all/ child element
       }
    | DirectionTypeEyeglasses {
          directionTypeEyeglasses :: EmptyPrintStyle -- ^ /eyeglasses/ child element
       }
    | DirectionTypeScordatura {
          directionTypeScordatura :: Scordatura -- ^ /scordatura/ child element
       }
    | DirectionTypeImage {
          directionTypeImage :: Image -- ^ /image/ child element
       }
    | DirectionTypeAccordionRegistration {
          directionTypeAccordionRegistration :: AccordionRegistration -- ^ /accordion-registration/ child element
       }
    | DirectionTypeOtherDirection {
          directionTypeOtherDirection :: OtherDirection -- ^ /other-direction/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxDirectionType where
    emitXml (DirectionTypeRehearsal a) =
      XContent XEmpty
        []
        (map (XElement (QN "rehearsal" Nothing).emitXml) a)
    emitXml (DirectionTypeSegno a) =
      XContent XEmpty
        []
        (map (XElement (QN "segno" Nothing).emitXml) a)
    emitXml (DirectionTypeWords a) =
      XContent XEmpty
        []
        (map (XElement (QN "words" Nothing).emitXml) a)
    emitXml (DirectionTypeCoda a) =
      XContent XEmpty
        []
        (map (XElement (QN "coda" Nothing).emitXml) a)
    emitXml (DirectionTypeWedge a) =
      XContent XEmpty
        []
        ([XElement (QN "wedge" Nothing) (emitXml a)])
    emitXml (DirectionTypeDynamics a) =
      XContent XEmpty
        []
        (map (XElement (QN "dynamics" Nothing).emitXml) a)
    emitXml (DirectionTypeDashes a) =
      XContent XEmpty
        []
        ([XElement (QN "dashes" Nothing) (emitXml a)])
    emitXml (DirectionTypeBracket a) =
      XContent XEmpty
        []
        ([XElement (QN "bracket" Nothing) (emitXml a)])
    emitXml (DirectionTypePedal a) =
      XContent XEmpty
        []
        ([XElement (QN "pedal" Nothing) (emitXml a)])
    emitXml (DirectionTypeMetronome a) =
      XContent XEmpty
        []
        ([XElement (QN "metronome" Nothing) (emitXml a)])
    emitXml (DirectionTypeOctaveShift a) =
      XContent XEmpty
        []
        ([XElement (QN "octave-shift" Nothing) (emitXml a)])
    emitXml (DirectionTypeHarpPedals a) =
      XContent XEmpty
        []
        ([XElement (QN "harp-pedals" Nothing) (emitXml a)])
    emitXml (DirectionTypeDamp a) =
      XContent XEmpty
        []
        ([XElement (QN "damp" Nothing) (emitXml a)])
    emitXml (DirectionTypeDampAll a) =
      XContent XEmpty
        []
        ([XElement (QN "damp-all" Nothing) (emitXml a)])
    emitXml (DirectionTypeEyeglasses a) =
      XContent XEmpty
        []
        ([XElement (QN "eyeglasses" Nothing) (emitXml a)])
    emitXml (DirectionTypeScordatura a) =
      XContent XEmpty
        []
        ([XElement (QN "scordatura" Nothing) (emitXml a)])
    emitXml (DirectionTypeImage a) =
      XContent XEmpty
        []
        ([XElement (QN "image" Nothing) (emitXml a)])
    emitXml (DirectionTypeAccordionRegistration a) =
      XContent XEmpty
        []
        ([XElement (QN "accordion-registration" Nothing) (emitXml a)])
    emitXml (DirectionTypeOtherDirection a) =
      XContent XEmpty
        []
        ([XElement (QN "other-direction" Nothing) (emitXml a)])
parseChxDirectionType :: P.XParse ChxDirectionType
parseChxDirectionType = 
      DirectionTypeRehearsal
        <$> P.many (P.xchild (P.name "rehearsal") (parseRehearsal))
      <|> DirectionTypeSegno
        <$> P.many (P.xchild (P.name "segno") (parseEmptyPrintStyle))
      <|> DirectionTypeWords
        <$> P.many (P.xchild (P.name "words") (parseFormattedText))
      <|> DirectionTypeCoda
        <$> P.many (P.xchild (P.name "coda") (parseEmptyPrintStyle))
      <|> DirectionTypeWedge
        <$> (P.xchild (P.name "wedge") (parseWedge))
      <|> DirectionTypeDynamics
        <$> P.many (P.xchild (P.name "dynamics") (parseDynamics))
      <|> DirectionTypeDashes
        <$> (P.xchild (P.name "dashes") (parseDashes))
      <|> DirectionTypeBracket
        <$> (P.xchild (P.name "bracket") (parseBracket))
      <|> DirectionTypePedal
        <$> (P.xchild (P.name "pedal") (parsePedal))
      <|> DirectionTypeMetronome
        <$> (P.xchild (P.name "metronome") (parseMetronome))
      <|> DirectionTypeOctaveShift
        <$> (P.xchild (P.name "octave-shift") (parseOctaveShift))
      <|> DirectionTypeHarpPedals
        <$> (P.xchild (P.name "harp-pedals") (parseHarpPedals))
      <|> DirectionTypeDamp
        <$> (P.xchild (P.name "damp") (parseEmptyPrintStyle))
      <|> DirectionTypeDampAll
        <$> (P.xchild (P.name "damp-all") (parseEmptyPrintStyle))
      <|> DirectionTypeEyeglasses
        <$> (P.xchild (P.name "eyeglasses") (parseEmptyPrintStyle))
      <|> DirectionTypeScordatura
        <$> (P.xchild (P.name "scordatura") (parseScordatura))
      <|> DirectionTypeImage
        <$> (P.xchild (P.name "image") (parseImage))
      <|> DirectionTypeAccordionRegistration
        <$> (P.xchild (P.name "accordion-registration") (parseAccordionRegistration))
      <|> DirectionTypeOtherDirection
        <$> (P.xchild (P.name "other-direction") (parseOtherDirection))

-- | Smart constructor for 'DirectionTypeRehearsal'
mkDirectionTypeRehearsal :: ChxDirectionType
mkDirectionTypeRehearsal = DirectionTypeRehearsal []
-- | Smart constructor for 'DirectionTypeSegno'
mkDirectionTypeSegno :: ChxDirectionType
mkDirectionTypeSegno = DirectionTypeSegno []
-- | Smart constructor for 'DirectionTypeWords'
mkDirectionTypeWords :: ChxDirectionType
mkDirectionTypeWords = DirectionTypeWords []
-- | Smart constructor for 'DirectionTypeCoda'
mkDirectionTypeCoda :: ChxDirectionType
mkDirectionTypeCoda = DirectionTypeCoda []
-- | Smart constructor for 'DirectionTypeWedge'
mkDirectionTypeWedge :: Wedge -> ChxDirectionType
mkDirectionTypeWedge a = DirectionTypeWedge a
-- | Smart constructor for 'DirectionTypeDynamics'
mkDirectionTypeDynamics :: ChxDirectionType
mkDirectionTypeDynamics = DirectionTypeDynamics []
-- | Smart constructor for 'DirectionTypeDashes'
mkDirectionTypeDashes :: Dashes -> ChxDirectionType
mkDirectionTypeDashes a = DirectionTypeDashes a
-- | Smart constructor for 'DirectionTypeBracket'
mkDirectionTypeBracket :: Bracket -> ChxDirectionType
mkDirectionTypeBracket a = DirectionTypeBracket a
-- | Smart constructor for 'DirectionTypePedal'
mkDirectionTypePedal :: Pedal -> ChxDirectionType
mkDirectionTypePedal a = DirectionTypePedal a
-- | Smart constructor for 'DirectionTypeMetronome'
mkDirectionTypeMetronome :: Metronome -> ChxDirectionType
mkDirectionTypeMetronome a = DirectionTypeMetronome a
-- | Smart constructor for 'DirectionTypeOctaveShift'
mkDirectionTypeOctaveShift :: OctaveShift -> ChxDirectionType
mkDirectionTypeOctaveShift a = DirectionTypeOctaveShift a
-- | Smart constructor for 'DirectionTypeHarpPedals'
mkDirectionTypeHarpPedals :: HarpPedals -> ChxDirectionType
mkDirectionTypeHarpPedals a = DirectionTypeHarpPedals a
-- | Smart constructor for 'DirectionTypeDamp'
mkDirectionTypeDamp :: EmptyPrintStyle -> ChxDirectionType
mkDirectionTypeDamp a = DirectionTypeDamp a
-- | Smart constructor for 'DirectionTypeDampAll'
mkDirectionTypeDampAll :: EmptyPrintStyle -> ChxDirectionType
mkDirectionTypeDampAll a = DirectionTypeDampAll a
-- | Smart constructor for 'DirectionTypeEyeglasses'
mkDirectionTypeEyeglasses :: EmptyPrintStyle -> ChxDirectionType
mkDirectionTypeEyeglasses a = DirectionTypeEyeglasses a
-- | Smart constructor for 'DirectionTypeScordatura'
mkDirectionTypeScordatura :: Scordatura -> ChxDirectionType
mkDirectionTypeScordatura a = DirectionTypeScordatura a
-- | Smart constructor for 'DirectionTypeImage'
mkDirectionTypeImage :: Image -> ChxDirectionType
mkDirectionTypeImage a = DirectionTypeImage a
-- | Smart constructor for 'DirectionTypeAccordionRegistration'
mkDirectionTypeAccordionRegistration :: AccordionRegistration -> ChxDirectionType
mkDirectionTypeAccordionRegistration a = DirectionTypeAccordionRegistration a
-- | Smart constructor for 'DirectionTypeOtherDirection'
mkDirectionTypeOtherDirection :: OtherDirection -> ChxDirectionType
mkDirectionTypeOtherDirection a = DirectionTypeOtherDirection a

-- | @dynamics@ /(choice)/
data ChxDynamics = 
      DynamicsP {
          dynamicsP :: Empty -- ^ /p/ child element
       }
    | DynamicsPp {
          dynamicsPp :: Empty -- ^ /pp/ child element
       }
    | DynamicsPpp {
          dynamicsPpp :: Empty -- ^ /ppp/ child element
       }
    | DynamicsPppp {
          dynamicsPppp :: Empty -- ^ /pppp/ child element
       }
    | DynamicsPpppp {
          dynamicsPpppp :: Empty -- ^ /ppppp/ child element
       }
    | DynamicsPppppp {
          dynamicsPppppp :: Empty -- ^ /pppppp/ child element
       }
    | DynamicsF {
          dynamicsF :: Empty -- ^ /f/ child element
       }
    | DynamicsFf {
          dynamicsFf :: Empty -- ^ /ff/ child element
       }
    | DynamicsFff {
          dynamicsFff :: Empty -- ^ /fff/ child element
       }
    | DynamicsFfff {
          dynamicsFfff :: Empty -- ^ /ffff/ child element
       }
    | DynamicsFffff {
          dynamicsFffff :: Empty -- ^ /fffff/ child element
       }
    | DynamicsFfffff {
          dynamicsFfffff :: Empty -- ^ /ffffff/ child element
       }
    | DynamicsMp {
          dynamicsMp :: Empty -- ^ /mp/ child element
       }
    | DynamicsMf {
          dynamicsMf :: Empty -- ^ /mf/ child element
       }
    | DynamicsSf {
          dynamicsSf :: Empty -- ^ /sf/ child element
       }
    | DynamicsSfp {
          dynamicsSfp :: Empty -- ^ /sfp/ child element
       }
    | DynamicsSfpp {
          dynamicsSfpp :: Empty -- ^ /sfpp/ child element
       }
    | DynamicsFp {
          dynamicsFp :: Empty -- ^ /fp/ child element
       }
    | DynamicsRf {
          dynamicsRf :: Empty -- ^ /rf/ child element
       }
    | DynamicsRfz {
          dynamicsRfz :: Empty -- ^ /rfz/ child element
       }
    | DynamicsSfz {
          dynamicsSfz :: Empty -- ^ /sfz/ child element
       }
    | DynamicsSffz {
          dynamicsSffz :: Empty -- ^ /sffz/ child element
       }
    | DynamicsFz {
          dynamicsFz :: Empty -- ^ /fz/ child element
       }
    | DynamicsOtherDynamics {
          dynamicsOtherDynamics :: String -- ^ /other-dynamics/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxDynamics where
    emitXml (DynamicsP a) =
      XContent XEmpty
        []
        ([XElement (QN "p" Nothing) (emitXml a)])
    emitXml (DynamicsPp a) =
      XContent XEmpty
        []
        ([XElement (QN "pp" Nothing) (emitXml a)])
    emitXml (DynamicsPpp a) =
      XContent XEmpty
        []
        ([XElement (QN "ppp" Nothing) (emitXml a)])
    emitXml (DynamicsPppp a) =
      XContent XEmpty
        []
        ([XElement (QN "pppp" Nothing) (emitXml a)])
    emitXml (DynamicsPpppp a) =
      XContent XEmpty
        []
        ([XElement (QN "ppppp" Nothing) (emitXml a)])
    emitXml (DynamicsPppppp a) =
      XContent XEmpty
        []
        ([XElement (QN "pppppp" Nothing) (emitXml a)])
    emitXml (DynamicsF a) =
      XContent XEmpty
        []
        ([XElement (QN "f" Nothing) (emitXml a)])
    emitXml (DynamicsFf a) =
      XContent XEmpty
        []
        ([XElement (QN "ff" Nothing) (emitXml a)])
    emitXml (DynamicsFff a) =
      XContent XEmpty
        []
        ([XElement (QN "fff" Nothing) (emitXml a)])
    emitXml (DynamicsFfff a) =
      XContent XEmpty
        []
        ([XElement (QN "ffff" Nothing) (emitXml a)])
    emitXml (DynamicsFffff a) =
      XContent XEmpty
        []
        ([XElement (QN "fffff" Nothing) (emitXml a)])
    emitXml (DynamicsFfffff a) =
      XContent XEmpty
        []
        ([XElement (QN "ffffff" Nothing) (emitXml a)])
    emitXml (DynamicsMp a) =
      XContent XEmpty
        []
        ([XElement (QN "mp" Nothing) (emitXml a)])
    emitXml (DynamicsMf a) =
      XContent XEmpty
        []
        ([XElement (QN "mf" Nothing) (emitXml a)])
    emitXml (DynamicsSf a) =
      XContent XEmpty
        []
        ([XElement (QN "sf" Nothing) (emitXml a)])
    emitXml (DynamicsSfp a) =
      XContent XEmpty
        []
        ([XElement (QN "sfp" Nothing) (emitXml a)])
    emitXml (DynamicsSfpp a) =
      XContent XEmpty
        []
        ([XElement (QN "sfpp" Nothing) (emitXml a)])
    emitXml (DynamicsFp a) =
      XContent XEmpty
        []
        ([XElement (QN "fp" Nothing) (emitXml a)])
    emitXml (DynamicsRf a) =
      XContent XEmpty
        []
        ([XElement (QN "rf" Nothing) (emitXml a)])
    emitXml (DynamicsRfz a) =
      XContent XEmpty
        []
        ([XElement (QN "rfz" Nothing) (emitXml a)])
    emitXml (DynamicsSfz a) =
      XContent XEmpty
        []
        ([XElement (QN "sfz" Nothing) (emitXml a)])
    emitXml (DynamicsSffz a) =
      XContent XEmpty
        []
        ([XElement (QN "sffz" Nothing) (emitXml a)])
    emitXml (DynamicsFz a) =
      XContent XEmpty
        []
        ([XElement (QN "fz" Nothing) (emitXml a)])
    emitXml (DynamicsOtherDynamics a) =
      XContent XEmpty
        []
        ([XElement (QN "other-dynamics" Nothing) (emitXml a)])
parseChxDynamics :: P.XParse ChxDynamics
parseChxDynamics = 
      DynamicsP
        <$> (P.xchild (P.name "p") (parseEmpty))
      <|> DynamicsPp
        <$> (P.xchild (P.name "pp") (parseEmpty))
      <|> DynamicsPpp
        <$> (P.xchild (P.name "ppp") (parseEmpty))
      <|> DynamicsPppp
        <$> (P.xchild (P.name "pppp") (parseEmpty))
      <|> DynamicsPpppp
        <$> (P.xchild (P.name "ppppp") (parseEmpty))
      <|> DynamicsPppppp
        <$> (P.xchild (P.name "pppppp") (parseEmpty))
      <|> DynamicsF
        <$> (P.xchild (P.name "f") (parseEmpty))
      <|> DynamicsFf
        <$> (P.xchild (P.name "ff") (parseEmpty))
      <|> DynamicsFff
        <$> (P.xchild (P.name "fff") (parseEmpty))
      <|> DynamicsFfff
        <$> (P.xchild (P.name "ffff") (parseEmpty))
      <|> DynamicsFffff
        <$> (P.xchild (P.name "fffff") (parseEmpty))
      <|> DynamicsFfffff
        <$> (P.xchild (P.name "ffffff") (parseEmpty))
      <|> DynamicsMp
        <$> (P.xchild (P.name "mp") (parseEmpty))
      <|> DynamicsMf
        <$> (P.xchild (P.name "mf") (parseEmpty))
      <|> DynamicsSf
        <$> (P.xchild (P.name "sf") (parseEmpty))
      <|> DynamicsSfp
        <$> (P.xchild (P.name "sfp") (parseEmpty))
      <|> DynamicsSfpp
        <$> (P.xchild (P.name "sfpp") (parseEmpty))
      <|> DynamicsFp
        <$> (P.xchild (P.name "fp") (parseEmpty))
      <|> DynamicsRf
        <$> (P.xchild (P.name "rf") (parseEmpty))
      <|> DynamicsRfz
        <$> (P.xchild (P.name "rfz") (parseEmpty))
      <|> DynamicsSfz
        <$> (P.xchild (P.name "sfz") (parseEmpty))
      <|> DynamicsSffz
        <$> (P.xchild (P.name "sffz") (parseEmpty))
      <|> DynamicsFz
        <$> (P.xchild (P.name "fz") (parseEmpty))
      <|> DynamicsOtherDynamics
        <$> (P.xchild (P.name "other-dynamics") (P.xtext >>= return))

-- | Smart constructor for 'DynamicsP'
mkDynamicsP :: Empty -> ChxDynamics
mkDynamicsP a = DynamicsP a
-- | Smart constructor for 'DynamicsPp'
mkDynamicsPp :: Empty -> ChxDynamics
mkDynamicsPp a = DynamicsPp a
-- | Smart constructor for 'DynamicsPpp'
mkDynamicsPpp :: Empty -> ChxDynamics
mkDynamicsPpp a = DynamicsPpp a
-- | Smart constructor for 'DynamicsPppp'
mkDynamicsPppp :: Empty -> ChxDynamics
mkDynamicsPppp a = DynamicsPppp a
-- | Smart constructor for 'DynamicsPpppp'
mkDynamicsPpppp :: Empty -> ChxDynamics
mkDynamicsPpppp a = DynamicsPpppp a
-- | Smart constructor for 'DynamicsPppppp'
mkDynamicsPppppp :: Empty -> ChxDynamics
mkDynamicsPppppp a = DynamicsPppppp a
-- | Smart constructor for 'DynamicsF'
mkDynamicsF :: Empty -> ChxDynamics
mkDynamicsF a = DynamicsF a
-- | Smart constructor for 'DynamicsFf'
mkDynamicsFf :: Empty -> ChxDynamics
mkDynamicsFf a = DynamicsFf a
-- | Smart constructor for 'DynamicsFff'
mkDynamicsFff :: Empty -> ChxDynamics
mkDynamicsFff a = DynamicsFff a
-- | Smart constructor for 'DynamicsFfff'
mkDynamicsFfff :: Empty -> ChxDynamics
mkDynamicsFfff a = DynamicsFfff a
-- | Smart constructor for 'DynamicsFffff'
mkDynamicsFffff :: Empty -> ChxDynamics
mkDynamicsFffff a = DynamicsFffff a
-- | Smart constructor for 'DynamicsFfffff'
mkDynamicsFfffff :: Empty -> ChxDynamics
mkDynamicsFfffff a = DynamicsFfffff a
-- | Smart constructor for 'DynamicsMp'
mkDynamicsMp :: Empty -> ChxDynamics
mkDynamicsMp a = DynamicsMp a
-- | Smart constructor for 'DynamicsMf'
mkDynamicsMf :: Empty -> ChxDynamics
mkDynamicsMf a = DynamicsMf a
-- | Smart constructor for 'DynamicsSf'
mkDynamicsSf :: Empty -> ChxDynamics
mkDynamicsSf a = DynamicsSf a
-- | Smart constructor for 'DynamicsSfp'
mkDynamicsSfp :: Empty -> ChxDynamics
mkDynamicsSfp a = DynamicsSfp a
-- | Smart constructor for 'DynamicsSfpp'
mkDynamicsSfpp :: Empty -> ChxDynamics
mkDynamicsSfpp a = DynamicsSfpp a
-- | Smart constructor for 'DynamicsFp'
mkDynamicsFp :: Empty -> ChxDynamics
mkDynamicsFp a = DynamicsFp a
-- | Smart constructor for 'DynamicsRf'
mkDynamicsRf :: Empty -> ChxDynamics
mkDynamicsRf a = DynamicsRf a
-- | Smart constructor for 'DynamicsRfz'
mkDynamicsRfz :: Empty -> ChxDynamics
mkDynamicsRfz a = DynamicsRfz a
-- | Smart constructor for 'DynamicsSfz'
mkDynamicsSfz :: Empty -> ChxDynamics
mkDynamicsSfz a = DynamicsSfz a
-- | Smart constructor for 'DynamicsSffz'
mkDynamicsSffz :: Empty -> ChxDynamics
mkDynamicsSffz a = DynamicsSffz a
-- | Smart constructor for 'DynamicsFz'
mkDynamicsFz :: Empty -> ChxDynamics
mkDynamicsFz a = DynamicsFz a
-- | Smart constructor for 'DynamicsOtherDynamics'
mkDynamicsOtherDynamics :: String -> ChxDynamics
mkDynamicsOtherDynamics a = DynamicsOtherDynamics a

-- | @encoding@ /(choice)/
data ChxEncoding = 
      EncodingEncodingDate {
          encodingEncodingDate :: YyyyMmDd -- ^ /encoding-date/ child element
       }
    | EncodingEncoder {
          encodingEncoder :: TypedText -- ^ /encoder/ child element
       }
    | EncodingSoftware {
          encodingSoftware :: String -- ^ /software/ child element
       }
    | EncodingEncodingDescription {
          encodingEncodingDescription :: String -- ^ /encoding-description/ child element
       }
    | EncodingSupports {
          encodingSupports :: Supports -- ^ /supports/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxEncoding where
    emitXml (EncodingEncodingDate a) =
      XContent XEmpty
        []
        ([XElement (QN "encoding-date" Nothing) (emitXml a)])
    emitXml (EncodingEncoder a) =
      XContent XEmpty
        []
        ([XElement (QN "encoder" Nothing) (emitXml a)])
    emitXml (EncodingSoftware a) =
      XContent XEmpty
        []
        ([XElement (QN "software" Nothing) (emitXml a)])
    emitXml (EncodingEncodingDescription a) =
      XContent XEmpty
        []
        ([XElement (QN "encoding-description" Nothing) (emitXml a)])
    emitXml (EncodingSupports a) =
      XContent XEmpty
        []
        ([XElement (QN "supports" Nothing) (emitXml a)])
parseChxEncoding :: P.XParse ChxEncoding
parseChxEncoding = 
      EncodingEncodingDate
        <$> (P.xchild (P.name "encoding-date") (P.xtext >>= parseYyyyMmDd))
      <|> EncodingEncoder
        <$> (P.xchild (P.name "encoder") (parseTypedText))
      <|> EncodingSoftware
        <$> (P.xchild (P.name "software") (P.xtext >>= return))
      <|> EncodingEncodingDescription
        <$> (P.xchild (P.name "encoding-description") (P.xtext >>= return))
      <|> EncodingSupports
        <$> (P.xchild (P.name "supports") (parseSupports))

-- | Smart constructor for 'EncodingEncodingDate'
mkEncodingEncodingDate :: YyyyMmDd -> ChxEncoding
mkEncodingEncodingDate a = EncodingEncodingDate a
-- | Smart constructor for 'EncodingEncoder'
mkEncodingEncoder :: TypedText -> ChxEncoding
mkEncodingEncoder a = EncodingEncoder a
-- | Smart constructor for 'EncodingSoftware'
mkEncodingSoftware :: String -> ChxEncoding
mkEncodingSoftware a = EncodingSoftware a
-- | Smart constructor for 'EncodingEncodingDescription'
mkEncodingEncodingDescription :: String -> ChxEncoding
mkEncodingEncodingDescription a = EncodingEncodingDescription a
-- | Smart constructor for 'EncodingSupports'
mkEncodingSupports :: Supports -> ChxEncoding
mkEncodingSupports a = EncodingSupports a

-- | @full-note@ /(choice)/
data FullNote = 
      FullNotePitch {
          fullNotePitch :: Pitch -- ^ /pitch/ child element
       }
    | FullNoteUnpitched {
          fullNoteUnpitched :: DisplayStepOctave -- ^ /unpitched/ child element
       }
    | FullNoteRest {
          fullNoteRest :: DisplayStepOctave -- ^ /rest/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FullNote where
    emitXml (FullNotePitch a) =
      XContent XEmpty
        []
        ([XElement (QN "pitch" Nothing) (emitXml a)])
    emitXml (FullNoteUnpitched a) =
      XContent XEmpty
        []
        ([XElement (QN "unpitched" Nothing) (emitXml a)])
    emitXml (FullNoteRest a) =
      XContent XEmpty
        []
        ([XElement (QN "rest" Nothing) (emitXml a)])
parseFullNote :: P.XParse FullNote
parseFullNote = 
      FullNotePitch
        <$> (P.xchild (P.name "pitch") (parsePitch))
      <|> FullNoteUnpitched
        <$> (P.xchild (P.name "unpitched") (parseDisplayStepOctave))
      <|> FullNoteRest
        <$> (P.xchild (P.name "rest") (parseDisplayStepOctave))

-- | Smart constructor for 'FullNotePitch'
mkFullNotePitch :: Pitch -> FullNote
mkFullNotePitch a = FullNotePitch a
-- | Smart constructor for 'FullNoteUnpitched'
mkFullNoteUnpitched :: DisplayStepOctave -> FullNote
mkFullNoteUnpitched a = FullNoteUnpitched a
-- | Smart constructor for 'FullNoteRest'
mkFullNoteRest :: DisplayStepOctave -> FullNote
mkFullNoteRest a = FullNoteRest a

-- | @harmonic@ /(choice)/
data ChxHarmonic = 
      HarmonicNatural {
          harmonicNatural :: Empty -- ^ /natural/ child element
       }
    | HarmonicArtificial {
          harmonicArtificial :: Empty -- ^ /artificial/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxHarmonic where
    emitXml (HarmonicNatural a) =
      XContent XEmpty
        []
        ([XElement (QN "natural" Nothing) (emitXml a)])
    emitXml (HarmonicArtificial a) =
      XContent XEmpty
        []
        ([XElement (QN "artificial" Nothing) (emitXml a)])
parseChxHarmonic :: P.XParse ChxHarmonic
parseChxHarmonic = 
      HarmonicNatural
        <$> (P.xchild (P.name "natural") (parseEmpty))
      <|> HarmonicArtificial
        <$> (P.xchild (P.name "artificial") (parseEmpty))

-- | Smart constructor for 'HarmonicNatural'
mkHarmonicNatural :: Empty -> ChxHarmonic
mkHarmonicNatural a = HarmonicNatural a
-- | Smart constructor for 'HarmonicArtificial'
mkHarmonicArtificial :: Empty -> ChxHarmonic
mkHarmonicArtificial a = HarmonicArtificial a

-- | @harmonic@ /(choice)/

-- mangled: 1
data ChxHarmonic1 = 
      HarmonicBasePitch {
          harmonicBasePitch :: Empty -- ^ /base-pitch/ child element
       }
    | HarmonicTouchingPitch {
          harmonicTouchingPitch :: Empty -- ^ /touching-pitch/ child element
       }
    | HarmonicSoundingPitch {
          harmonicSoundingPitch :: Empty -- ^ /sounding-pitch/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxHarmonic1 where
    emitXml (HarmonicBasePitch a) =
      XContent XEmpty
        []
        ([XElement (QN "base-pitch" Nothing) (emitXml a)])
    emitXml (HarmonicTouchingPitch a) =
      XContent XEmpty
        []
        ([XElement (QN "touching-pitch" Nothing) (emitXml a)])
    emitXml (HarmonicSoundingPitch a) =
      XContent XEmpty
        []
        ([XElement (QN "sounding-pitch" Nothing) (emitXml a)])
parseChxHarmonic1 :: P.XParse ChxHarmonic1
parseChxHarmonic1 = 
      HarmonicBasePitch
        <$> (P.xchild (P.name "base-pitch") (parseEmpty))
      <|> HarmonicTouchingPitch
        <$> (P.xchild (P.name "touching-pitch") (parseEmpty))
      <|> HarmonicSoundingPitch
        <$> (P.xchild (P.name "sounding-pitch") (parseEmpty))

-- | Smart constructor for 'HarmonicBasePitch'
mkHarmonicBasePitch :: Empty -> ChxHarmonic1
mkHarmonicBasePitch a = HarmonicBasePitch a
-- | Smart constructor for 'HarmonicTouchingPitch'
mkHarmonicTouchingPitch :: Empty -> ChxHarmonic1
mkHarmonicTouchingPitch a = HarmonicTouchingPitch a
-- | Smart constructor for 'HarmonicSoundingPitch'
mkHarmonicSoundingPitch :: Empty -> ChxHarmonic1
mkHarmonicSoundingPitch a = HarmonicSoundingPitch a

-- | @harmony-chord@ /(choice)/
data ChxHarmonyChord = 
      HarmonyChordRoot {
          harmonyChordRoot :: Root -- ^ /root/ child element
       }
    | HarmonyChordFunction {
          harmonyChordFunction :: StyleText -- ^ /function/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxHarmonyChord where
    emitXml (HarmonyChordRoot a) =
      XContent XEmpty
        []
        ([XElement (QN "root" Nothing) (emitXml a)])
    emitXml (HarmonyChordFunction a) =
      XContent XEmpty
        []
        ([XElement (QN "function" Nothing) (emitXml a)])
parseChxHarmonyChord :: P.XParse ChxHarmonyChord
parseChxHarmonyChord = 
      HarmonyChordRoot
        <$> (P.xchild (P.name "root") (parseRoot))
      <|> HarmonyChordFunction
        <$> (P.xchild (P.name "function") (parseStyleText))

-- | Smart constructor for 'HarmonyChordRoot'
mkHarmonyChordRoot :: Root -> ChxHarmonyChord
mkHarmonyChordRoot a = HarmonyChordRoot a
-- | Smart constructor for 'HarmonyChordFunction'
mkHarmonyChordFunction :: StyleText -> ChxHarmonyChord
mkHarmonyChordFunction a = HarmonyChordFunction a

-- | @key@ /(choice)/
data ChxKey = 
      KeyTraditionalKey {
          keyTraditionalKey :: TraditionalKey
       }
    | KeyNonTraditionalKey {
          keyNonTraditionalKey :: [NonTraditionalKey]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxKey where
    emitXml (KeyTraditionalKey a) =
      XReps [emitXml a]
    emitXml (KeyNonTraditionalKey a) =
      XReps [emitXml a]
parseChxKey :: P.XParse ChxKey
parseChxKey = 
      KeyTraditionalKey
        <$> parseTraditionalKey
      <|> KeyNonTraditionalKey
        <$> P.many (parseNonTraditionalKey)

-- | Smart constructor for 'KeyTraditionalKey'
mkKeyTraditionalKey :: TraditionalKey -> ChxKey
mkKeyTraditionalKey a = KeyTraditionalKey a
-- | Smart constructor for 'KeyNonTraditionalKey'
mkKeyNonTraditionalKey :: ChxKey
mkKeyNonTraditionalKey = KeyNonTraditionalKey []

-- | @lyric@ /(choice)/
data ChxLyric = 
      LyricSyllabic {
          lyricSyllabic :: (Maybe Syllabic) -- ^ /syllabic/ child element
        , lyricText :: TextElementData -- ^ /text/ child element
        , chxlyricLyric :: [SeqLyric]
        , lyricExtend :: (Maybe Extend) -- ^ /extend/ child element
       }
    | LyricExtend {
          lyricExtend1 :: Extend -- ^ /extend/ child element
       }
    | LyricLaughing {
          lyricLaughing :: Empty -- ^ /laughing/ child element
       }
    | LyricHumming {
          lyricHumming :: Empty -- ^ /humming/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxLyric where
    emitXml (LyricSyllabic a b c d) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "syllabic" Nothing).emitXml) a]++[XElement (QN "text" Nothing) (emitXml b)]++[emitXml c]++[maybe XEmpty (XElement (QN "extend" Nothing).emitXml) d])
    emitXml (LyricExtend a) =
      XContent XEmpty
        []
        ([XElement (QN "extend" Nothing) (emitXml a)])
    emitXml (LyricLaughing a) =
      XContent XEmpty
        []
        ([XElement (QN "laughing" Nothing) (emitXml a)])
    emitXml (LyricHumming a) =
      XContent XEmpty
        []
        ([XElement (QN "humming" Nothing) (emitXml a)])
parseChxLyric :: P.XParse ChxLyric
parseChxLyric = 
      LyricSyllabic
        <$> P.optional (P.xchild (P.name "syllabic") (P.xtext >>= parseSyllabic))
        <*> (P.xchild (P.name "text") (parseTextElementData))
        <*> P.many (parseSeqLyric)
        <*> P.optional (P.xchild (P.name "extend") (parseExtend))
      <|> LyricExtend
        <$> (P.xchild (P.name "extend") (parseExtend))
      <|> LyricLaughing
        <$> (P.xchild (P.name "laughing") (parseEmpty))
      <|> LyricHumming
        <$> (P.xchild (P.name "humming") (parseEmpty))

-- | Smart constructor for 'LyricSyllabic'
mkLyricSyllabic :: TextElementData -> ChxLyric
mkLyricSyllabic b = LyricSyllabic Nothing b [] Nothing
-- | Smart constructor for 'LyricExtend'
mkLyricExtend :: Extend -> ChxLyric
mkLyricExtend a = LyricExtend a
-- | Smart constructor for 'LyricLaughing'
mkLyricLaughing :: Empty -> ChxLyric
mkLyricLaughing a = LyricLaughing a
-- | Smart constructor for 'LyricHumming'
mkLyricHumming :: Empty -> ChxLyric
mkLyricHumming a = LyricHumming a

-- | @measure-style@ /(choice)/
data ChxMeasureStyle = 
      MeasureStyleMultipleRest {
          measureStyleMultipleRest :: MultipleRest -- ^ /multiple-rest/ child element
       }
    | MeasureStyleMeasureRepeat {
          measureStyleMeasureRepeat :: MeasureRepeat -- ^ /measure-repeat/ child element
       }
    | MeasureStyleBeatRepeat {
          measureStyleBeatRepeat :: BeatRepeat -- ^ /beat-repeat/ child element
       }
    | MeasureStyleSlash {
          measureStyleSlash :: CmpSlash -- ^ /slash/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxMeasureStyle where
    emitXml (MeasureStyleMultipleRest a) =
      XContent XEmpty
        []
        ([XElement (QN "multiple-rest" Nothing) (emitXml a)])
    emitXml (MeasureStyleMeasureRepeat a) =
      XContent XEmpty
        []
        ([XElement (QN "measure-repeat" Nothing) (emitXml a)])
    emitXml (MeasureStyleBeatRepeat a) =
      XContent XEmpty
        []
        ([XElement (QN "beat-repeat" Nothing) (emitXml a)])
    emitXml (MeasureStyleSlash a) =
      XContent XEmpty
        []
        ([XElement (QN "slash" Nothing) (emitXml a)])
parseChxMeasureStyle :: P.XParse ChxMeasureStyle
parseChxMeasureStyle = 
      MeasureStyleMultipleRest
        <$> (P.xchild (P.name "multiple-rest") (parseMultipleRest))
      <|> MeasureStyleMeasureRepeat
        <$> (P.xchild (P.name "measure-repeat") (parseMeasureRepeat))
      <|> MeasureStyleBeatRepeat
        <$> (P.xchild (P.name "beat-repeat") (parseBeatRepeat))
      <|> MeasureStyleSlash
        <$> (P.xchild (P.name "slash") (parseCmpSlash))

-- | Smart constructor for 'MeasureStyleMultipleRest'
mkMeasureStyleMultipleRest :: MultipleRest -> ChxMeasureStyle
mkMeasureStyleMultipleRest a = MeasureStyleMultipleRest a
-- | Smart constructor for 'MeasureStyleMeasureRepeat'
mkMeasureStyleMeasureRepeat :: MeasureRepeat -> ChxMeasureStyle
mkMeasureStyleMeasureRepeat a = MeasureStyleMeasureRepeat a
-- | Smart constructor for 'MeasureStyleBeatRepeat'
mkMeasureStyleBeatRepeat :: BeatRepeat -> ChxMeasureStyle
mkMeasureStyleBeatRepeat a = MeasureStyleBeatRepeat a
-- | Smart constructor for 'MeasureStyleSlash'
mkMeasureStyleSlash :: CmpSlash -> ChxMeasureStyle
mkMeasureStyleSlash a = MeasureStyleSlash a

-- | @metronome@ /(choice)/
data ChxMetronome0 = 
      MetronomePerMinute {
          metronomePerMinute :: PerMinute -- ^ /per-minute/ child element
       }
    | MetronomeBeatUnit {
          metronomeBeatUnit :: BeatUnit
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxMetronome0 where
    emitXml (MetronomePerMinute a) =
      XContent XEmpty
        []
        ([XElement (QN "per-minute" Nothing) (emitXml a)])
    emitXml (MetronomeBeatUnit a) =
      XReps [emitXml a]
parseChxMetronome0 :: P.XParse ChxMetronome0
parseChxMetronome0 = 
      MetronomePerMinute
        <$> (P.xchild (P.name "per-minute") (parsePerMinute))
      <|> MetronomeBeatUnit
        <$> parseBeatUnit

-- | Smart constructor for 'MetronomePerMinute'
mkMetronomePerMinute :: PerMinute -> ChxMetronome0
mkMetronomePerMinute a = MetronomePerMinute a
-- | Smart constructor for 'MetronomeBeatUnit'
mkMetronomeBeatUnit :: BeatUnit -> ChxMetronome0
mkMetronomeBeatUnit a = MetronomeBeatUnit a

-- | @metronome@ /(choice)/

-- mangled: 1
data ChxMetronome = 
      ChxMetronomeBeatUnit {
          chxmetronomeBeatUnit :: BeatUnit
        , chxmetronomeMetronome :: ChxMetronome0
       }
    | MetronomeMetronomeNote {
          metronomeMetronomeNote :: [MetronomeNote] -- ^ /metronome-note/ child element
        , metronomeMetronome1 :: (Maybe SeqMetronome)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxMetronome where
    emitXml (ChxMetronomeBeatUnit a b) =
      XReps [emitXml a,emitXml b]
    emitXml (MetronomeMetronomeNote a b) =
      XContent XEmpty
        []
        (map (XElement (QN "metronome-note" Nothing).emitXml) a++[emitXml b])
parseChxMetronome :: P.XParse ChxMetronome
parseChxMetronome = 
      ChxMetronomeBeatUnit
        <$> parseBeatUnit
        <*> parseChxMetronome0
      <|> MetronomeMetronomeNote
        <$> P.many (P.xchild (P.name "metronome-note") (parseMetronomeNote))
        <*> P.optional (parseSeqMetronome)

-- | Smart constructor for 'ChxMetronomeBeatUnit'
mkChxMetronomeBeatUnit :: BeatUnit -> ChxMetronome0 -> ChxMetronome
mkChxMetronomeBeatUnit a b = ChxMetronomeBeatUnit a b
-- | Smart constructor for 'MetronomeMetronomeNote'
mkMetronomeMetronomeNote :: ChxMetronome
mkMetronomeMetronomeNote = MetronomeMetronomeNote [] Nothing

-- | @music-data@ /(choice)/
data ChxMusicData = 
      MusicDataNote {
          musicDataNote :: Note -- ^ /note/ child element
       }
    | MusicDataBackup {
          musicDataBackup :: Backup -- ^ /backup/ child element
       }
    | MusicDataForward {
          musicDataForward :: Forward -- ^ /forward/ child element
       }
    | MusicDataDirection {
          musicDataDirection :: Direction -- ^ /direction/ child element
       }
    | MusicDataAttributes {
          musicDataAttributes :: Attributes -- ^ /attributes/ child element
       }
    | MusicDataHarmony {
          musicDataHarmony :: Harmony -- ^ /harmony/ child element
       }
    | MusicDataFiguredBass {
          musicDataFiguredBass :: FiguredBass -- ^ /figured-bass/ child element
       }
    | MusicDataPrint {
          musicDataPrint :: Print -- ^ /print/ child element
       }
    | MusicDataSound {
          musicDataSound :: Sound -- ^ /sound/ child element
       }
    | MusicDataBarline {
          musicDataBarline :: Barline -- ^ /barline/ child element
       }
    | MusicDataGrouping {
          musicDataGrouping :: Grouping -- ^ /grouping/ child element
       }
    | MusicDataLink {
          musicDataLink :: Link -- ^ /link/ child element
       }
    | MusicDataBookmark {
          musicDataBookmark :: Bookmark -- ^ /bookmark/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxMusicData where
    emitXml (MusicDataNote a) =
      XContent XEmpty
        []
        ([XElement (QN "note" Nothing) (emitXml a)])
    emitXml (MusicDataBackup a) =
      XContent XEmpty
        []
        ([XElement (QN "backup" Nothing) (emitXml a)])
    emitXml (MusicDataForward a) =
      XContent XEmpty
        []
        ([XElement (QN "forward" Nothing) (emitXml a)])
    emitXml (MusicDataDirection a) =
      XContent XEmpty
        []
        ([XElement (QN "direction" Nothing) (emitXml a)])
    emitXml (MusicDataAttributes a) =
      XContent XEmpty
        []
        ([XElement (QN "attributes" Nothing) (emitXml a)])
    emitXml (MusicDataHarmony a) =
      XContent XEmpty
        []
        ([XElement (QN "harmony" Nothing) (emitXml a)])
    emitXml (MusicDataFiguredBass a) =
      XContent XEmpty
        []
        ([XElement (QN "figured-bass" Nothing) (emitXml a)])
    emitXml (MusicDataPrint a) =
      XContent XEmpty
        []
        ([XElement (QN "print" Nothing) (emitXml a)])
    emitXml (MusicDataSound a) =
      XContent XEmpty
        []
        ([XElement (QN "sound" Nothing) (emitXml a)])
    emitXml (MusicDataBarline a) =
      XContent XEmpty
        []
        ([XElement (QN "barline" Nothing) (emitXml a)])
    emitXml (MusicDataGrouping a) =
      XContent XEmpty
        []
        ([XElement (QN "grouping" Nothing) (emitXml a)])
    emitXml (MusicDataLink a) =
      XContent XEmpty
        []
        ([XElement (QN "link" Nothing) (emitXml a)])
    emitXml (MusicDataBookmark a) =
      XContent XEmpty
        []
        ([XElement (QN "bookmark" Nothing) (emitXml a)])
parseChxMusicData :: P.XParse ChxMusicData
parseChxMusicData = 
      MusicDataNote
        <$> (P.xchild (P.name "note") (parseNote))
      <|> MusicDataBackup
        <$> (P.xchild (P.name "backup") (parseBackup))
      <|> MusicDataForward
        <$> (P.xchild (P.name "forward") (parseForward))
      <|> MusicDataDirection
        <$> (P.xchild (P.name "direction") (parseDirection))
      <|> MusicDataAttributes
        <$> (P.xchild (P.name "attributes") (parseAttributes))
      <|> MusicDataHarmony
        <$> (P.xchild (P.name "harmony") (parseHarmony))
      <|> MusicDataFiguredBass
        <$> (P.xchild (P.name "figured-bass") (parseFiguredBass))
      <|> MusicDataPrint
        <$> (P.xchild (P.name "print") (parsePrint))
      <|> MusicDataSound
        <$> (P.xchild (P.name "sound") (parseSound))
      <|> MusicDataBarline
        <$> (P.xchild (P.name "barline") (parseBarline))
      <|> MusicDataGrouping
        <$> (P.xchild (P.name "grouping") (parseGrouping))
      <|> MusicDataLink
        <$> (P.xchild (P.name "link") (parseLink))
      <|> MusicDataBookmark
        <$> (P.xchild (P.name "bookmark") (parseBookmark))

-- | Smart constructor for 'MusicDataNote'
mkMusicDataNote :: Note -> ChxMusicData
mkMusicDataNote a = MusicDataNote a
-- | Smart constructor for 'MusicDataBackup'
mkMusicDataBackup :: Backup -> ChxMusicData
mkMusicDataBackup a = MusicDataBackup a
-- | Smart constructor for 'MusicDataForward'
mkMusicDataForward :: Forward -> ChxMusicData
mkMusicDataForward a = MusicDataForward a
-- | Smart constructor for 'MusicDataDirection'
mkMusicDataDirection :: Direction -> ChxMusicData
mkMusicDataDirection a = MusicDataDirection a
-- | Smart constructor for 'MusicDataAttributes'
mkMusicDataAttributes :: Attributes -> ChxMusicData
mkMusicDataAttributes a = MusicDataAttributes a
-- | Smart constructor for 'MusicDataHarmony'
mkMusicDataHarmony :: Harmony -> ChxMusicData
mkMusicDataHarmony a = MusicDataHarmony a
-- | Smart constructor for 'MusicDataFiguredBass'
mkMusicDataFiguredBass :: FiguredBass -> ChxMusicData
mkMusicDataFiguredBass a = MusicDataFiguredBass a
-- | Smart constructor for 'MusicDataPrint'
mkMusicDataPrint :: Print -> ChxMusicData
mkMusicDataPrint a = MusicDataPrint a
-- | Smart constructor for 'MusicDataSound'
mkMusicDataSound :: Sound -> ChxMusicData
mkMusicDataSound a = MusicDataSound a
-- | Smart constructor for 'MusicDataBarline'
mkMusicDataBarline :: Barline -> ChxMusicData
mkMusicDataBarline a = MusicDataBarline a
-- | Smart constructor for 'MusicDataGrouping'
mkMusicDataGrouping :: Grouping -> ChxMusicData
mkMusicDataGrouping a = MusicDataGrouping a
-- | Smart constructor for 'MusicDataLink'
mkMusicDataLink :: Link -> ChxMusicData
mkMusicDataLink a = MusicDataLink a
-- | Smart constructor for 'MusicDataBookmark'
mkMusicDataBookmark :: Bookmark -> ChxMusicData
mkMusicDataBookmark a = MusicDataBookmark a

-- | @name-display@ /(choice)/
data ChxNameDisplay = 
      NameDisplayDisplayText {
          nameDisplayDisplayText :: FormattedText -- ^ /display-text/ child element
       }
    | NameDisplayAccidentalText {
          nameDisplayAccidentalText :: AccidentalText -- ^ /accidental-text/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxNameDisplay where
    emitXml (NameDisplayDisplayText a) =
      XContent XEmpty
        []
        ([XElement (QN "display-text" Nothing) (emitXml a)])
    emitXml (NameDisplayAccidentalText a) =
      XContent XEmpty
        []
        ([XElement (QN "accidental-text" Nothing) (emitXml a)])
parseChxNameDisplay :: P.XParse ChxNameDisplay
parseChxNameDisplay = 
      NameDisplayDisplayText
        <$> (P.xchild (P.name "display-text") (parseFormattedText))
      <|> NameDisplayAccidentalText
        <$> (P.xchild (P.name "accidental-text") (parseAccidentalText))

-- | Smart constructor for 'NameDisplayDisplayText'
mkNameDisplayDisplayText :: FormattedText -> ChxNameDisplay
mkNameDisplayDisplayText a = NameDisplayDisplayText a
-- | Smart constructor for 'NameDisplayAccidentalText'
mkNameDisplayAccidentalText :: AccidentalText -> ChxNameDisplay
mkNameDisplayAccidentalText a = NameDisplayAccidentalText a

-- | @notations@ /(choice)/
data ChxNotations = 
      NotationsTied {
          notationsTied :: Tied -- ^ /tied/ child element
       }
    | NotationsSlur {
          notationsSlur :: Slur -- ^ /slur/ child element
       }
    | NotationsTuplet {
          notationsTuplet :: Tuplet -- ^ /tuplet/ child element
       }
    | NotationsGlissando {
          notationsGlissando :: Glissando -- ^ /glissando/ child element
       }
    | NotationsSlide {
          notationsSlide :: Slide -- ^ /slide/ child element
       }
    | NotationsOrnaments {
          notationsOrnaments :: Ornaments -- ^ /ornaments/ child element
       }
    | NotationsTechnical {
          notationsTechnical :: Technical -- ^ /technical/ child element
       }
    | NotationsArticulations {
          notationsArticulations :: Articulations -- ^ /articulations/ child element
       }
    | NotationsDynamics {
          notationsDynamics :: Dynamics -- ^ /dynamics/ child element
       }
    | NotationsFermata {
          notationsFermata :: Fermata -- ^ /fermata/ child element
       }
    | NotationsArpeggiate {
          notationsArpeggiate :: Arpeggiate -- ^ /arpeggiate/ child element
       }
    | NotationsNonArpeggiate {
          notationsNonArpeggiate :: NonArpeggiate -- ^ /non-arpeggiate/ child element
       }
    | NotationsAccidentalMark {
          notationsAccidentalMark :: AccidentalMark -- ^ /accidental-mark/ child element
       }
    | NotationsOtherNotation {
          notationsOtherNotation :: OtherNotation -- ^ /other-notation/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxNotations where
    emitXml (NotationsTied a) =
      XContent XEmpty
        []
        ([XElement (QN "tied" Nothing) (emitXml a)])
    emitXml (NotationsSlur a) =
      XContent XEmpty
        []
        ([XElement (QN "slur" Nothing) (emitXml a)])
    emitXml (NotationsTuplet a) =
      XContent XEmpty
        []
        ([XElement (QN "tuplet" Nothing) (emitXml a)])
    emitXml (NotationsGlissando a) =
      XContent XEmpty
        []
        ([XElement (QN "glissando" Nothing) (emitXml a)])
    emitXml (NotationsSlide a) =
      XContent XEmpty
        []
        ([XElement (QN "slide" Nothing) (emitXml a)])
    emitXml (NotationsOrnaments a) =
      XContent XEmpty
        []
        ([XElement (QN "ornaments" Nothing) (emitXml a)])
    emitXml (NotationsTechnical a) =
      XContent XEmpty
        []
        ([XElement (QN "technical" Nothing) (emitXml a)])
    emitXml (NotationsArticulations a) =
      XContent XEmpty
        []
        ([XElement (QN "articulations" Nothing) (emitXml a)])
    emitXml (NotationsDynamics a) =
      XContent XEmpty
        []
        ([XElement (QN "dynamics" Nothing) (emitXml a)])
    emitXml (NotationsFermata a) =
      XContent XEmpty
        []
        ([XElement (QN "fermata" Nothing) (emitXml a)])
    emitXml (NotationsArpeggiate a) =
      XContent XEmpty
        []
        ([XElement (QN "arpeggiate" Nothing) (emitXml a)])
    emitXml (NotationsNonArpeggiate a) =
      XContent XEmpty
        []
        ([XElement (QN "non-arpeggiate" Nothing) (emitXml a)])
    emitXml (NotationsAccidentalMark a) =
      XContent XEmpty
        []
        ([XElement (QN "accidental-mark" Nothing) (emitXml a)])
    emitXml (NotationsOtherNotation a) =
      XContent XEmpty
        []
        ([XElement (QN "other-notation" Nothing) (emitXml a)])
parseChxNotations :: P.XParse ChxNotations
parseChxNotations = 
      NotationsTied
        <$> (P.xchild (P.name "tied") (parseTied))
      <|> NotationsSlur
        <$> (P.xchild (P.name "slur") (parseSlur))
      <|> NotationsTuplet
        <$> (P.xchild (P.name "tuplet") (parseTuplet))
      <|> NotationsGlissando
        <$> (P.xchild (P.name "glissando") (parseGlissando))
      <|> NotationsSlide
        <$> (P.xchild (P.name "slide") (parseSlide))
      <|> NotationsOrnaments
        <$> (P.xchild (P.name "ornaments") (parseOrnaments))
      <|> NotationsTechnical
        <$> (P.xchild (P.name "technical") (parseTechnical))
      <|> NotationsArticulations
        <$> (P.xchild (P.name "articulations") (parseArticulations))
      <|> NotationsDynamics
        <$> (P.xchild (P.name "dynamics") (parseDynamics))
      <|> NotationsFermata
        <$> (P.xchild (P.name "fermata") (parseFermata))
      <|> NotationsArpeggiate
        <$> (P.xchild (P.name "arpeggiate") (parseArpeggiate))
      <|> NotationsNonArpeggiate
        <$> (P.xchild (P.name "non-arpeggiate") (parseNonArpeggiate))
      <|> NotationsAccidentalMark
        <$> (P.xchild (P.name "accidental-mark") (parseAccidentalMark))
      <|> NotationsOtherNotation
        <$> (P.xchild (P.name "other-notation") (parseOtherNotation))

-- | Smart constructor for 'NotationsTied'
mkNotationsTied :: Tied -> ChxNotations
mkNotationsTied a = NotationsTied a
-- | Smart constructor for 'NotationsSlur'
mkNotationsSlur :: Slur -> ChxNotations
mkNotationsSlur a = NotationsSlur a
-- | Smart constructor for 'NotationsTuplet'
mkNotationsTuplet :: Tuplet -> ChxNotations
mkNotationsTuplet a = NotationsTuplet a
-- | Smart constructor for 'NotationsGlissando'
mkNotationsGlissando :: Glissando -> ChxNotations
mkNotationsGlissando a = NotationsGlissando a
-- | Smart constructor for 'NotationsSlide'
mkNotationsSlide :: Slide -> ChxNotations
mkNotationsSlide a = NotationsSlide a
-- | Smart constructor for 'NotationsOrnaments'
mkNotationsOrnaments :: Ornaments -> ChxNotations
mkNotationsOrnaments a = NotationsOrnaments a
-- | Smart constructor for 'NotationsTechnical'
mkNotationsTechnical :: Technical -> ChxNotations
mkNotationsTechnical a = NotationsTechnical a
-- | Smart constructor for 'NotationsArticulations'
mkNotationsArticulations :: Articulations -> ChxNotations
mkNotationsArticulations a = NotationsArticulations a
-- | Smart constructor for 'NotationsDynamics'
mkNotationsDynamics :: Dynamics -> ChxNotations
mkNotationsDynamics a = NotationsDynamics a
-- | Smart constructor for 'NotationsFermata'
mkNotationsFermata :: Fermata -> ChxNotations
mkNotationsFermata a = NotationsFermata a
-- | Smart constructor for 'NotationsArpeggiate'
mkNotationsArpeggiate :: Arpeggiate -> ChxNotations
mkNotationsArpeggiate a = NotationsArpeggiate a
-- | Smart constructor for 'NotationsNonArpeggiate'
mkNotationsNonArpeggiate :: NonArpeggiate -> ChxNotations
mkNotationsNonArpeggiate a = NotationsNonArpeggiate a
-- | Smart constructor for 'NotationsAccidentalMark'
mkNotationsAccidentalMark :: AccidentalMark -> ChxNotations
mkNotationsAccidentalMark a = NotationsAccidentalMark a
-- | Smart constructor for 'NotationsOtherNotation'
mkNotationsOtherNotation :: OtherNotation -> ChxNotations
mkNotationsOtherNotation a = NotationsOtherNotation a

-- | @note@ /(choice)/
data ChxNote = 
      NoteGrace {
          noteGrace :: Grace -- ^ /grace/ child element
        , noteFullNote :: GrpFullNote
        , noteTie :: [Tie] -- ^ /tie/ child element
       }
    | NoteCue {
          noteCue :: Empty -- ^ /cue/ child element
        , noteFullNote1 :: GrpFullNote
        , noteDuration :: Duration
       }
    | NoteFullNote {
          noteFullNote2 :: GrpFullNote
        , noteDuration1 :: Duration
        , noteTie1 :: [Tie] -- ^ /tie/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxNote where
    emitXml (NoteGrace a b c) =
      XContent XEmpty
        []
        ([XElement (QN "grace" Nothing) (emitXml a)]++[emitXml b]++map (XElement (QN "tie" Nothing).emitXml) c)
    emitXml (NoteCue a b c) =
      XContent XEmpty
        []
        ([XElement (QN "cue" Nothing) (emitXml a)]++[emitXml b]++[emitXml c])
    emitXml (NoteFullNote a b c) =
      XContent XEmpty
        []
        ([emitXml a]++[emitXml b]++map (XElement (QN "tie" Nothing).emitXml) c)
parseChxNote :: P.XParse ChxNote
parseChxNote = 
      NoteGrace
        <$> (P.xchild (P.name "grace") (parseGrace))
        <*> parseGrpFullNote
        <*> P.many (P.xchild (P.name "tie") (parseTie))
      <|> NoteCue
        <$> (P.xchild (P.name "cue") (parseEmpty))
        <*> parseGrpFullNote
        <*> parseDuration
      <|> NoteFullNote
        <$> parseGrpFullNote
        <*> parseDuration
        <*> P.many (P.xchild (P.name "tie") (parseTie))

-- | Smart constructor for 'NoteGrace'
mkNoteGrace :: Grace -> GrpFullNote -> ChxNote
mkNoteGrace a b = NoteGrace a b []
-- | Smart constructor for 'NoteCue'
mkNoteCue :: Empty -> GrpFullNote -> Duration -> ChxNote
mkNoteCue a b c = NoteCue a b c
-- | Smart constructor for 'NoteFullNote'
mkNoteFullNote :: GrpFullNote -> Duration -> ChxNote
mkNoteFullNote a b = NoteFullNote a b []

-- | @ornaments@ /(choice)/
data ChxOrnaments = 
      OrnamentsTrillMark {
          ornamentsTrillMark :: EmptyTrillSound -- ^ /trill-mark/ child element
       }
    | OrnamentsTurn {
          ornamentsTurn :: EmptyTrillSound -- ^ /turn/ child element
       }
    | OrnamentsDelayedTurn {
          ornamentsDelayedTurn :: EmptyTrillSound -- ^ /delayed-turn/ child element
       }
    | OrnamentsInvertedTurn {
          ornamentsInvertedTurn :: EmptyTrillSound -- ^ /inverted-turn/ child element
       }
    | OrnamentsShake {
          ornamentsShake :: EmptyTrillSound -- ^ /shake/ child element
       }
    | OrnamentsWavyLine {
          ornamentsWavyLine :: WavyLine -- ^ /wavy-line/ child element
       }
    | OrnamentsMordent {
          ornamentsMordent :: Mordent -- ^ /mordent/ child element
       }
    | OrnamentsInvertedMordent {
          ornamentsInvertedMordent :: Mordent -- ^ /inverted-mordent/ child element
       }
    | OrnamentsSchleifer {
          ornamentsSchleifer :: EmptyPlacement -- ^ /schleifer/ child element
       }
    | OrnamentsTremolo {
          ornamentsTremolo :: Tremolo -- ^ /tremolo/ child element
       }
    | OrnamentsOtherOrnament {
          ornamentsOtherOrnament :: PlacementText -- ^ /other-ornament/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxOrnaments where
    emitXml (OrnamentsTrillMark a) =
      XContent XEmpty
        []
        ([XElement (QN "trill-mark" Nothing) (emitXml a)])
    emitXml (OrnamentsTurn a) =
      XContent XEmpty
        []
        ([XElement (QN "turn" Nothing) (emitXml a)])
    emitXml (OrnamentsDelayedTurn a) =
      XContent XEmpty
        []
        ([XElement (QN "delayed-turn" Nothing) (emitXml a)])
    emitXml (OrnamentsInvertedTurn a) =
      XContent XEmpty
        []
        ([XElement (QN "inverted-turn" Nothing) (emitXml a)])
    emitXml (OrnamentsShake a) =
      XContent XEmpty
        []
        ([XElement (QN "shake" Nothing) (emitXml a)])
    emitXml (OrnamentsWavyLine a) =
      XContent XEmpty
        []
        ([XElement (QN "wavy-line" Nothing) (emitXml a)])
    emitXml (OrnamentsMordent a) =
      XContent XEmpty
        []
        ([XElement (QN "mordent" Nothing) (emitXml a)])
    emitXml (OrnamentsInvertedMordent a) =
      XContent XEmpty
        []
        ([XElement (QN "inverted-mordent" Nothing) (emitXml a)])
    emitXml (OrnamentsSchleifer a) =
      XContent XEmpty
        []
        ([XElement (QN "schleifer" Nothing) (emitXml a)])
    emitXml (OrnamentsTremolo a) =
      XContent XEmpty
        []
        ([XElement (QN "tremolo" Nothing) (emitXml a)])
    emitXml (OrnamentsOtherOrnament a) =
      XContent XEmpty
        []
        ([XElement (QN "other-ornament" Nothing) (emitXml a)])
parseChxOrnaments :: P.XParse ChxOrnaments
parseChxOrnaments = 
      OrnamentsTrillMark
        <$> (P.xchild (P.name "trill-mark") (parseEmptyTrillSound))
      <|> OrnamentsTurn
        <$> (P.xchild (P.name "turn") (parseEmptyTrillSound))
      <|> OrnamentsDelayedTurn
        <$> (P.xchild (P.name "delayed-turn") (parseEmptyTrillSound))
      <|> OrnamentsInvertedTurn
        <$> (P.xchild (P.name "inverted-turn") (parseEmptyTrillSound))
      <|> OrnamentsShake
        <$> (P.xchild (P.name "shake") (parseEmptyTrillSound))
      <|> OrnamentsWavyLine
        <$> (P.xchild (P.name "wavy-line") (parseWavyLine))
      <|> OrnamentsMordent
        <$> (P.xchild (P.name "mordent") (parseMordent))
      <|> OrnamentsInvertedMordent
        <$> (P.xchild (P.name "inverted-mordent") (parseMordent))
      <|> OrnamentsSchleifer
        <$> (P.xchild (P.name "schleifer") (parseEmptyPlacement))
      <|> OrnamentsTremolo
        <$> (P.xchild (P.name "tremolo") (parseTremolo))
      <|> OrnamentsOtherOrnament
        <$> (P.xchild (P.name "other-ornament") (parsePlacementText))

-- | Smart constructor for 'OrnamentsTrillMark'
mkOrnamentsTrillMark :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsTrillMark a = OrnamentsTrillMark a
-- | Smart constructor for 'OrnamentsTurn'
mkOrnamentsTurn :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsTurn a = OrnamentsTurn a
-- | Smart constructor for 'OrnamentsDelayedTurn'
mkOrnamentsDelayedTurn :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsDelayedTurn a = OrnamentsDelayedTurn a
-- | Smart constructor for 'OrnamentsInvertedTurn'
mkOrnamentsInvertedTurn :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsInvertedTurn a = OrnamentsInvertedTurn a
-- | Smart constructor for 'OrnamentsShake'
mkOrnamentsShake :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsShake a = OrnamentsShake a
-- | Smart constructor for 'OrnamentsWavyLine'
mkOrnamentsWavyLine :: WavyLine -> ChxOrnaments
mkOrnamentsWavyLine a = OrnamentsWavyLine a
-- | Smart constructor for 'OrnamentsMordent'
mkOrnamentsMordent :: Mordent -> ChxOrnaments
mkOrnamentsMordent a = OrnamentsMordent a
-- | Smart constructor for 'OrnamentsInvertedMordent'
mkOrnamentsInvertedMordent :: Mordent -> ChxOrnaments
mkOrnamentsInvertedMordent a = OrnamentsInvertedMordent a
-- | Smart constructor for 'OrnamentsSchleifer'
mkOrnamentsSchleifer :: EmptyPlacement -> ChxOrnaments
mkOrnamentsSchleifer a = OrnamentsSchleifer a
-- | Smart constructor for 'OrnamentsTremolo'
mkOrnamentsTremolo :: Tremolo -> ChxOrnaments
mkOrnamentsTremolo a = OrnamentsTremolo a
-- | Smart constructor for 'OrnamentsOtherOrnament'
mkOrnamentsOtherOrnament :: PlacementText -> ChxOrnaments
mkOrnamentsOtherOrnament a = OrnamentsOtherOrnament a

-- | @part-list@ /(choice)/
data ChxPartList = 
      PartListPartGroup {
          chxpartListPartGroup :: GrpPartGroup
       }
    | PartListScorePart {
          chxpartListScorePart :: ScorePart
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxPartList where
    emitXml (PartListPartGroup a) =
      XReps [emitXml a]
    emitXml (PartListScorePart a) =
      XReps [emitXml a]
parseChxPartList :: P.XParse ChxPartList
parseChxPartList = 
      PartListPartGroup
        <$> parseGrpPartGroup
      <|> PartListScorePart
        <$> parseScorePart

-- | Smart constructor for 'PartListPartGroup'
mkPartListPartGroup :: GrpPartGroup -> ChxPartList
mkPartListPartGroup a = PartListPartGroup a
-- | Smart constructor for 'PartListScorePart'
mkPartListScorePart :: ScorePart -> ChxPartList
mkPartListScorePart a = PartListScorePart a

-- | @score-instrument@ /(choice)/
data ChxScoreInstrument = 
      ScoreInstrumentSolo {
          scoreInstrumentSolo :: Empty -- ^ /solo/ child element
       }
    | ScoreInstrumentEnsemble {
          scoreInstrumentEnsemble :: PositiveIntegerOrEmpty -- ^ /ensemble/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxScoreInstrument where
    emitXml (ScoreInstrumentSolo a) =
      XContent XEmpty
        []
        ([XElement (QN "solo" Nothing) (emitXml a)])
    emitXml (ScoreInstrumentEnsemble a) =
      XContent XEmpty
        []
        ([XElement (QN "ensemble" Nothing) (emitXml a)])
parseChxScoreInstrument :: P.XParse ChxScoreInstrument
parseChxScoreInstrument = 
      ScoreInstrumentSolo
        <$> (P.xchild (P.name "solo") (parseEmpty))
      <|> ScoreInstrumentEnsemble
        <$> (P.xchild (P.name "ensemble") (P.xtext >>= parsePositiveIntegerOrEmpty))

-- | Smart constructor for 'ScoreInstrumentSolo'
mkScoreInstrumentSolo :: Empty -> ChxScoreInstrument
mkScoreInstrumentSolo a = ScoreInstrumentSolo a
-- | Smart constructor for 'ScoreInstrumentEnsemble'
mkScoreInstrumentEnsemble :: PositiveIntegerOrEmpty -> ChxScoreInstrument
mkScoreInstrumentEnsemble a = ScoreInstrumentEnsemble a

-- | @technical@ /(choice)/
data ChxTechnical = 
      TechnicalUpBow {
          technicalUpBow :: EmptyPlacement -- ^ /up-bow/ child element
       }
    | TechnicalDownBow {
          technicalDownBow :: EmptyPlacement -- ^ /down-bow/ child element
       }
    | TechnicalHarmonic {
          technicalHarmonic :: Harmonic -- ^ /harmonic/ child element
       }
    | TechnicalOpenString {
          technicalOpenString :: EmptyPlacement -- ^ /open-string/ child element
       }
    | TechnicalThumbPosition {
          technicalThumbPosition :: EmptyPlacement -- ^ /thumb-position/ child element
       }
    | TechnicalFingering {
          technicalFingering :: Fingering -- ^ /fingering/ child element
       }
    | TechnicalPluck {
          technicalPluck :: PlacementText -- ^ /pluck/ child element
       }
    | TechnicalDoubleTongue {
          technicalDoubleTongue :: EmptyPlacement -- ^ /double-tongue/ child element
       }
    | TechnicalTripleTongue {
          technicalTripleTongue :: EmptyPlacement -- ^ /triple-tongue/ child element
       }
    | TechnicalStopped {
          technicalStopped :: EmptyPlacement -- ^ /stopped/ child element
       }
    | TechnicalSnapPizzicato {
          technicalSnapPizzicato :: EmptyPlacement -- ^ /snap-pizzicato/ child element
       }
    | TechnicalFret {
          technicalFret :: Fret -- ^ /fret/ child element
       }
    | TechnicalString {
          technicalString :: CmpString -- ^ /string/ child element
       }
    | TechnicalHammerOn {
          technicalHammerOn :: HammerOnPullOff -- ^ /hammer-on/ child element
       }
    | TechnicalPullOff {
          technicalPullOff :: HammerOnPullOff -- ^ /pull-off/ child element
       }
    | TechnicalBend {
          technicalBend :: Bend -- ^ /bend/ child element
       }
    | TechnicalTap {
          technicalTap :: PlacementText -- ^ /tap/ child element
       }
    | TechnicalHeel {
          technicalHeel :: HeelToe -- ^ /heel/ child element
       }
    | TechnicalToe {
          technicalToe :: HeelToe -- ^ /toe/ child element
       }
    | TechnicalFingernails {
          technicalFingernails :: EmptyPlacement -- ^ /fingernails/ child element
       }
    | TechnicalOtherTechnical {
          technicalOtherTechnical :: PlacementText -- ^ /other-technical/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxTechnical where
    emitXml (TechnicalUpBow a) =
      XContent XEmpty
        []
        ([XElement (QN "up-bow" Nothing) (emitXml a)])
    emitXml (TechnicalDownBow a) =
      XContent XEmpty
        []
        ([XElement (QN "down-bow" Nothing) (emitXml a)])
    emitXml (TechnicalHarmonic a) =
      XContent XEmpty
        []
        ([XElement (QN "harmonic" Nothing) (emitXml a)])
    emitXml (TechnicalOpenString a) =
      XContent XEmpty
        []
        ([XElement (QN "open-string" Nothing) (emitXml a)])
    emitXml (TechnicalThumbPosition a) =
      XContent XEmpty
        []
        ([XElement (QN "thumb-position" Nothing) (emitXml a)])
    emitXml (TechnicalFingering a) =
      XContent XEmpty
        []
        ([XElement (QN "fingering" Nothing) (emitXml a)])
    emitXml (TechnicalPluck a) =
      XContent XEmpty
        []
        ([XElement (QN "pluck" Nothing) (emitXml a)])
    emitXml (TechnicalDoubleTongue a) =
      XContent XEmpty
        []
        ([XElement (QN "double-tongue" Nothing) (emitXml a)])
    emitXml (TechnicalTripleTongue a) =
      XContent XEmpty
        []
        ([XElement (QN "triple-tongue" Nothing) (emitXml a)])
    emitXml (TechnicalStopped a) =
      XContent XEmpty
        []
        ([XElement (QN "stopped" Nothing) (emitXml a)])
    emitXml (TechnicalSnapPizzicato a) =
      XContent XEmpty
        []
        ([XElement (QN "snap-pizzicato" Nothing) (emitXml a)])
    emitXml (TechnicalFret a) =
      XContent XEmpty
        []
        ([XElement (QN "fret" Nothing) (emitXml a)])
    emitXml (TechnicalString a) =
      XContent XEmpty
        []
        ([XElement (QN "string" Nothing) (emitXml a)])
    emitXml (TechnicalHammerOn a) =
      XContent XEmpty
        []
        ([XElement (QN "hammer-on" Nothing) (emitXml a)])
    emitXml (TechnicalPullOff a) =
      XContent XEmpty
        []
        ([XElement (QN "pull-off" Nothing) (emitXml a)])
    emitXml (TechnicalBend a) =
      XContent XEmpty
        []
        ([XElement (QN "bend" Nothing) (emitXml a)])
    emitXml (TechnicalTap a) =
      XContent XEmpty
        []
        ([XElement (QN "tap" Nothing) (emitXml a)])
    emitXml (TechnicalHeel a) =
      XContent XEmpty
        []
        ([XElement (QN "heel" Nothing) (emitXml a)])
    emitXml (TechnicalToe a) =
      XContent XEmpty
        []
        ([XElement (QN "toe" Nothing) (emitXml a)])
    emitXml (TechnicalFingernails a) =
      XContent XEmpty
        []
        ([XElement (QN "fingernails" Nothing) (emitXml a)])
    emitXml (TechnicalOtherTechnical a) =
      XContent XEmpty
        []
        ([XElement (QN "other-technical" Nothing) (emitXml a)])
parseChxTechnical :: P.XParse ChxTechnical
parseChxTechnical = 
      TechnicalUpBow
        <$> (P.xchild (P.name "up-bow") (parseEmptyPlacement))
      <|> TechnicalDownBow
        <$> (P.xchild (P.name "down-bow") (parseEmptyPlacement))
      <|> TechnicalHarmonic
        <$> (P.xchild (P.name "harmonic") (parseHarmonic))
      <|> TechnicalOpenString
        <$> (P.xchild (P.name "open-string") (parseEmptyPlacement))
      <|> TechnicalThumbPosition
        <$> (P.xchild (P.name "thumb-position") (parseEmptyPlacement))
      <|> TechnicalFingering
        <$> (P.xchild (P.name "fingering") (parseFingering))
      <|> TechnicalPluck
        <$> (P.xchild (P.name "pluck") (parsePlacementText))
      <|> TechnicalDoubleTongue
        <$> (P.xchild (P.name "double-tongue") (parseEmptyPlacement))
      <|> TechnicalTripleTongue
        <$> (P.xchild (P.name "triple-tongue") (parseEmptyPlacement))
      <|> TechnicalStopped
        <$> (P.xchild (P.name "stopped") (parseEmptyPlacement))
      <|> TechnicalSnapPizzicato
        <$> (P.xchild (P.name "snap-pizzicato") (parseEmptyPlacement))
      <|> TechnicalFret
        <$> (P.xchild (P.name "fret") (parseFret))
      <|> TechnicalString
        <$> (P.xchild (P.name "string") (parseCmpString))
      <|> TechnicalHammerOn
        <$> (P.xchild (P.name "hammer-on") (parseHammerOnPullOff))
      <|> TechnicalPullOff
        <$> (P.xchild (P.name "pull-off") (parseHammerOnPullOff))
      <|> TechnicalBend
        <$> (P.xchild (P.name "bend") (parseBend))
      <|> TechnicalTap
        <$> (P.xchild (P.name "tap") (parsePlacementText))
      <|> TechnicalHeel
        <$> (P.xchild (P.name "heel") (parseHeelToe))
      <|> TechnicalToe
        <$> (P.xchild (P.name "toe") (parseHeelToe))
      <|> TechnicalFingernails
        <$> (P.xchild (P.name "fingernails") (parseEmptyPlacement))
      <|> TechnicalOtherTechnical
        <$> (P.xchild (P.name "other-technical") (parsePlacementText))

-- | Smart constructor for 'TechnicalUpBow'
mkTechnicalUpBow :: EmptyPlacement -> ChxTechnical
mkTechnicalUpBow a = TechnicalUpBow a
-- | Smart constructor for 'TechnicalDownBow'
mkTechnicalDownBow :: EmptyPlacement -> ChxTechnical
mkTechnicalDownBow a = TechnicalDownBow a
-- | Smart constructor for 'TechnicalHarmonic'
mkTechnicalHarmonic :: Harmonic -> ChxTechnical
mkTechnicalHarmonic a = TechnicalHarmonic a
-- | Smart constructor for 'TechnicalOpenString'
mkTechnicalOpenString :: EmptyPlacement -> ChxTechnical
mkTechnicalOpenString a = TechnicalOpenString a
-- | Smart constructor for 'TechnicalThumbPosition'
mkTechnicalThumbPosition :: EmptyPlacement -> ChxTechnical
mkTechnicalThumbPosition a = TechnicalThumbPosition a
-- | Smart constructor for 'TechnicalFingering'
mkTechnicalFingering :: Fingering -> ChxTechnical
mkTechnicalFingering a = TechnicalFingering a
-- | Smart constructor for 'TechnicalPluck'
mkTechnicalPluck :: PlacementText -> ChxTechnical
mkTechnicalPluck a = TechnicalPluck a
-- | Smart constructor for 'TechnicalDoubleTongue'
mkTechnicalDoubleTongue :: EmptyPlacement -> ChxTechnical
mkTechnicalDoubleTongue a = TechnicalDoubleTongue a
-- | Smart constructor for 'TechnicalTripleTongue'
mkTechnicalTripleTongue :: EmptyPlacement -> ChxTechnical
mkTechnicalTripleTongue a = TechnicalTripleTongue a
-- | Smart constructor for 'TechnicalStopped'
mkTechnicalStopped :: EmptyPlacement -> ChxTechnical
mkTechnicalStopped a = TechnicalStopped a
-- | Smart constructor for 'TechnicalSnapPizzicato'
mkTechnicalSnapPizzicato :: EmptyPlacement -> ChxTechnical
mkTechnicalSnapPizzicato a = TechnicalSnapPizzicato a
-- | Smart constructor for 'TechnicalFret'
mkTechnicalFret :: Fret -> ChxTechnical
mkTechnicalFret a = TechnicalFret a
-- | Smart constructor for 'TechnicalString'
mkTechnicalString :: CmpString -> ChxTechnical
mkTechnicalString a = TechnicalString a
-- | Smart constructor for 'TechnicalHammerOn'
mkTechnicalHammerOn :: HammerOnPullOff -> ChxTechnical
mkTechnicalHammerOn a = TechnicalHammerOn a
-- | Smart constructor for 'TechnicalPullOff'
mkTechnicalPullOff :: HammerOnPullOff -> ChxTechnical
mkTechnicalPullOff a = TechnicalPullOff a
-- | Smart constructor for 'TechnicalBend'
mkTechnicalBend :: Bend -> ChxTechnical
mkTechnicalBend a = TechnicalBend a
-- | Smart constructor for 'TechnicalTap'
mkTechnicalTap :: PlacementText -> ChxTechnical
mkTechnicalTap a = TechnicalTap a
-- | Smart constructor for 'TechnicalHeel'
mkTechnicalHeel :: HeelToe -> ChxTechnical
mkTechnicalHeel a = TechnicalHeel a
-- | Smart constructor for 'TechnicalToe'
mkTechnicalToe :: HeelToe -> ChxTechnical
mkTechnicalToe a = TechnicalToe a
-- | Smart constructor for 'TechnicalFingernails'
mkTechnicalFingernails :: EmptyPlacement -> ChxTechnical
mkTechnicalFingernails a = TechnicalFingernails a
-- | Smart constructor for 'TechnicalOtherTechnical'
mkTechnicalOtherTechnical :: PlacementText -> ChxTechnical
mkTechnicalOtherTechnical a = TechnicalOtherTechnical a

-- | @time@ /(choice)/
data ChxTime = 
      TimeTime {
          chxtimeTime :: [SeqTime]
       }
    | TimeSenzaMisura {
          timeSenzaMisura :: Empty -- ^ /senza-misura/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxTime where
    emitXml (TimeTime a) =
      XReps [emitXml a]
    emitXml (TimeSenzaMisura a) =
      XContent XEmpty
        []
        ([XElement (QN "senza-misura" Nothing) (emitXml a)])
parseChxTime :: P.XParse ChxTime
parseChxTime = 
      TimeTime
        <$> P.many (parseSeqTime)
      <|> TimeSenzaMisura
        <$> (P.xchild (P.name "senza-misura") (parseEmpty))

-- | Smart constructor for 'TimeTime'
mkTimeTime :: ChxTime
mkTimeTime = TimeTime []
-- | Smart constructor for 'TimeSenzaMisura'
mkTimeSenzaMisura :: Empty -> ChxTime
mkTimeSenzaMisura a = TimeSenzaMisura a

-- | @credit@ /(sequence)/
data SeqCredit = 
      SeqCredit {
          seqcreditLink :: [Link] -- ^ /link/ child element
        , seqcreditBookmark :: [Bookmark] -- ^ /bookmark/ child element
        , seqcreditCreditWords :: FormattedText -- ^ /credit-words/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqCredit where
    emitXml (SeqCredit a b c) =
      XContent XEmpty
        []
        (map (XElement (QN "link" Nothing).emitXml) a++map (XElement (QN "bookmark" Nothing).emitXml) b++[XElement (QN "credit-words" Nothing) (emitXml c)])
parseSeqCredit :: P.XParse SeqCredit
parseSeqCredit = 
      SeqCredit
        <$> P.many (P.xchild (P.name "link") (parseLink))
        <*> P.many (P.xchild (P.name "bookmark") (parseBookmark))
        <*> (P.xchild (P.name "credit-words") (parseFormattedText))

-- | Smart constructor for 'SeqCredit'
mkSeqCredit :: FormattedText -> SeqCredit
mkSeqCredit c = SeqCredit [] [] c

-- | @display-step-octave@ /(sequence)/
data SeqDisplayStepOctave = 
      SeqDisplayStepOctave {
          displayStepOctaveDisplayStep :: Step -- ^ /display-step/ child element
        , displayStepOctaveDisplayOctave :: Octave -- ^ /display-octave/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqDisplayStepOctave where
    emitXml (SeqDisplayStepOctave a b) =
      XContent XEmpty
        []
        ([XElement (QN "display-step" Nothing) (emitXml a)]++[XElement (QN "display-octave" Nothing) (emitXml b)])
parseSeqDisplayStepOctave :: P.XParse SeqDisplayStepOctave
parseSeqDisplayStepOctave = 
      SeqDisplayStepOctave
        <$> (P.xchild (P.name "display-step") (P.xtext >>= parseStep))
        <*> (P.xchild (P.name "display-octave") (P.xtext >>= parseOctave))

-- | Smart constructor for 'SeqDisplayStepOctave'
mkSeqDisplayStepOctave :: Step -> Octave -> SeqDisplayStepOctave
mkSeqDisplayStepOctave a b = SeqDisplayStepOctave a b

-- | @lyric@ /(sequence)/
data SeqLyric0 = 
      SeqLyric0 {
          lyricElision :: Elision -- ^ /elision/ child element
        , seqlyricSyllabic :: (Maybe Syllabic) -- ^ /syllabic/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqLyric0 where
    emitXml (SeqLyric0 a b) =
      XContent XEmpty
        []
        ([XElement (QN "elision" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "syllabic" Nothing).emitXml) b])
parseSeqLyric0 :: P.XParse SeqLyric0
parseSeqLyric0 = 
      SeqLyric0
        <$> (P.xchild (P.name "elision") (parseElision))
        <*> P.optional (P.xchild (P.name "syllabic") (P.xtext >>= parseSyllabic))

-- | Smart constructor for 'SeqLyric0'
mkSeqLyric0 :: Elision -> SeqLyric0
mkSeqLyric0 a = SeqLyric0 a Nothing

-- | @lyric@ /(sequence)/

-- mangled: 1
data SeqLyric = 
      SeqLyric {
          seqlyricLyric :: (Maybe SeqLyric0)
        , seqlyricText :: TextElementData -- ^ /text/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqLyric where
    emitXml (SeqLyric a b) =
      XContent XEmpty
        []
        ([emitXml a]++[XElement (QN "text" Nothing) (emitXml b)])
parseSeqLyric :: P.XParse SeqLyric
parseSeqLyric = 
      SeqLyric
        <$> P.optional (parseSeqLyric0)
        <*> (P.xchild (P.name "text") (parseTextElementData))

-- | Smart constructor for 'SeqLyric'
mkSeqLyric :: TextElementData -> SeqLyric
mkSeqLyric b = SeqLyric Nothing b

-- | @metronome@ /(sequence)/
data SeqMetronome = 
      SeqMetronome {
          metronomeMetronomeRelation :: String -- ^ /metronome-relation/ child element
        , seqmetronomeMetronomeNote :: [MetronomeNote] -- ^ /metronome-note/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqMetronome where
    emitXml (SeqMetronome a b) =
      XContent XEmpty
        []
        ([XElement (QN "metronome-relation" Nothing) (emitXml a)]++map (XElement (QN "metronome-note" Nothing).emitXml) b)
parseSeqMetronome :: P.XParse SeqMetronome
parseSeqMetronome = 
      SeqMetronome
        <$> (P.xchild (P.name "metronome-relation") (P.xtext >>= return))
        <*> P.many (P.xchild (P.name "metronome-note") (parseMetronomeNote))

-- | Smart constructor for 'SeqMetronome'
mkSeqMetronome :: String -> SeqMetronome
mkSeqMetronome a = SeqMetronome a []

-- | @metronome-tuplet@ /(sequence)/
data SeqMetronomeTuplet = 
      SeqMetronomeTuplet {
          metronomeTupletNormalType :: NoteTypeValue -- ^ /normal-type/ child element
        , metronomeTupletNormalDot :: [Empty] -- ^ /normal-dot/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqMetronomeTuplet where
    emitXml (SeqMetronomeTuplet a b) =
      XContent XEmpty
        []
        ([XElement (QN "normal-type" Nothing) (emitXml a)]++map (XElement (QN "normal-dot" Nothing).emitXml) b)
parseSeqMetronomeTuplet :: P.XParse SeqMetronomeTuplet
parseSeqMetronomeTuplet = 
      SeqMetronomeTuplet
        <$> (P.xchild (P.name "normal-type") (P.xtext >>= parseNoteTypeValue))
        <*> P.many (P.xchild (P.name "normal-dot") (parseEmpty))

-- | Smart constructor for 'SeqMetronomeTuplet'
mkSeqMetronomeTuplet :: NoteTypeValue -> SeqMetronomeTuplet
mkSeqMetronomeTuplet a = SeqMetronomeTuplet a []

-- | @ornaments@ /(sequence)/
data SeqOrnaments = 
      SeqOrnaments {
          seqornamentsOrnaments :: ChxOrnaments
        , ornamentsAccidentalMark :: [AccidentalMark] -- ^ /accidental-mark/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqOrnaments where
    emitXml (SeqOrnaments a b) =
      XContent XEmpty
        []
        ([emitXml a]++map (XElement (QN "accidental-mark" Nothing).emitXml) b)
parseSeqOrnaments :: P.XParse SeqOrnaments
parseSeqOrnaments = 
      SeqOrnaments
        <$> parseChxOrnaments
        <*> P.many (P.xchild (P.name "accidental-mark") (parseAccidentalMark))

-- | Smart constructor for 'SeqOrnaments'
mkSeqOrnaments :: ChxOrnaments -> SeqOrnaments
mkSeqOrnaments a = SeqOrnaments a []

-- | @page-layout@ /(sequence)/
data SeqPageLayout = 
      SeqPageLayout {
          pageLayoutPageHeight :: Tenths -- ^ /page-height/ child element
        , pageLayoutPageWidth :: Tenths -- ^ /page-width/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqPageLayout where
    emitXml (SeqPageLayout a b) =
      XContent XEmpty
        []
        ([XElement (QN "page-height" Nothing) (emitXml a)]++[XElement (QN "page-width" Nothing) (emitXml b)])
parseSeqPageLayout :: P.XParse SeqPageLayout
parseSeqPageLayout = 
      SeqPageLayout
        <$> (P.xchild (P.name "page-height") (P.xtext >>= parseTenths))
        <*> (P.xchild (P.name "page-width") (P.xtext >>= parseTenths))

-- | Smart constructor for 'SeqPageLayout'
mkSeqPageLayout :: Tenths -> Tenths -> SeqPageLayout
mkSeqPageLayout a b = SeqPageLayout a b

-- | @time@ /(sequence)/
data SeqTime = 
      SeqTime {
          timeBeats :: String -- ^ /beats/ child element
        , timeBeatType :: String -- ^ /beat-type/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqTime where
    emitXml (SeqTime a b) =
      XContent XEmpty
        []
        ([XElement (QN "beats" Nothing) (emitXml a)]++[XElement (QN "beat-type" Nothing) (emitXml b)])
parseSeqTime :: P.XParse SeqTime
parseSeqTime = 
      SeqTime
        <$> (P.xchild (P.name "beats") (P.xtext >>= return))
        <*> (P.xchild (P.name "beat-type") (P.xtext >>= return))

-- | Smart constructor for 'SeqTime'
mkSeqTime :: String -> String -> SeqTime
mkSeqTime a b = SeqTime a b

-- | @time-modification@ /(sequence)/
data SeqTimeModification = 
      SeqTimeModification {
          timeModificationNormalType :: NoteTypeValue -- ^ /normal-type/ child element
        , timeModificationNormalDot :: [Empty] -- ^ /normal-dot/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqTimeModification where
    emitXml (SeqTimeModification a b) =
      XContent XEmpty
        []
        ([XElement (QN "normal-type" Nothing) (emitXml a)]++map (XElement (QN "normal-dot" Nothing).emitXml) b)
parseSeqTimeModification :: P.XParse SeqTimeModification
parseSeqTimeModification = 
      SeqTimeModification
        <$> (P.xchild (P.name "normal-type") (P.xtext >>= parseNoteTypeValue))
        <*> P.many (P.xchild (P.name "normal-dot") (parseEmpty))

-- | Smart constructor for 'SeqTimeModification'
mkSeqTimeModification :: NoteTypeValue -> SeqTimeModification
mkSeqTimeModification a = SeqTimeModification a []

-- | @all-margins@ /(group)/
data AllMargins = 
      AllMargins {
          allMarginsLeftRightMargins :: LeftRightMargins
        , allMarginsTopMargin :: Tenths -- ^ /top-margin/ child element
        , allMarginsBottomMargin :: Tenths -- ^ /bottom-margin/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AllMargins where
    emitXml (AllMargins a b c) =
      XContent XEmpty
        []
        ([emitXml a]++[XElement (QN "top-margin" Nothing) (emitXml b)]++[XElement (QN "bottom-margin" Nothing) (emitXml c)])
parseAllMargins :: P.XParse AllMargins
parseAllMargins = 
      AllMargins
        <$> parseLeftRightMargins
        <*> (P.xchild (P.name "top-margin") (P.xtext >>= parseTenths))
        <*> (P.xchild (P.name "bottom-margin") (P.xtext >>= parseTenths))

-- | Smart constructor for 'AllMargins'
mkAllMargins :: LeftRightMargins -> Tenths -> Tenths -> AllMargins
mkAllMargins a b c = AllMargins a b c

-- | @beat-unit@ /(group)/
data BeatUnit = 
      BeatUnit {
          beatUnitBeatUnit :: NoteTypeValue -- ^ /beat-unit/ child element
        , beatUnitBeatUnitDot :: [Empty] -- ^ /beat-unit-dot/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BeatUnit where
    emitXml (BeatUnit a b) =
      XContent XEmpty
        []
        ([XElement (QN "beat-unit" Nothing) (emitXml a)]++map (XElement (QN "beat-unit-dot" Nothing).emitXml) b)
parseBeatUnit :: P.XParse BeatUnit
parseBeatUnit = 
      BeatUnit
        <$> (P.xchild (P.name "beat-unit") (P.xtext >>= parseNoteTypeValue))
        <*> P.many (P.xchild (P.name "beat-unit-dot") (parseEmpty))

-- | Smart constructor for 'BeatUnit'
mkBeatUnit :: NoteTypeValue -> BeatUnit
mkBeatUnit a = BeatUnit a []

-- | @duration@ /(group)/
data Duration = 
      Duration {
          durationDuration :: PositiveDivisions -- ^ /duration/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Duration where
    emitXml (Duration a) =
      XContent XEmpty
        []
        ([XElement (QN "duration" Nothing) (emitXml a)])
parseDuration :: P.XParse Duration
parseDuration = 
      Duration
        <$> (P.xchild (P.name "duration") (P.xtext >>= parsePositiveDivisions))

-- | Smart constructor for 'Duration'
mkDuration :: PositiveDivisions -> Duration
mkDuration a = Duration a

-- | @editorial@ /(group)/
data Editorial = 
      Editorial {
          editorialFootnote :: (Maybe Footnote)
        , editorialLevel :: (Maybe GrpLevel)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Editorial where
    emitXml (Editorial a b) =
      XReps [emitXml a,emitXml b]
parseEditorial :: P.XParse Editorial
parseEditorial = 
      Editorial
        <$> P.optional (parseFootnote)
        <*> P.optional (parseGrpLevel)

-- | Smart constructor for 'Editorial'
mkEditorial :: Editorial
mkEditorial = Editorial Nothing Nothing

-- | @editorial-voice@ /(group)/
data EditorialVoice = 
      EditorialVoice {
          editorialVoiceFootnote :: (Maybe Footnote)
        , editorialVoiceLevel :: (Maybe GrpLevel)
        , editorialVoiceVoice :: (Maybe Voice)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EditorialVoice where
    emitXml (EditorialVoice a b c) =
      XReps [emitXml a,emitXml b,emitXml c]
parseEditorialVoice :: P.XParse EditorialVoice
parseEditorialVoice = 
      EditorialVoice
        <$> P.optional (parseFootnote)
        <*> P.optional (parseGrpLevel)
        <*> P.optional (parseVoice)

-- | Smart constructor for 'EditorialVoice'
mkEditorialVoice :: EditorialVoice
mkEditorialVoice = EditorialVoice Nothing Nothing Nothing

-- | @editorial-voice-direction@ /(group)/
data EditorialVoiceDirection = 
      EditorialVoiceDirection {
          editorialVoiceDirectionFootnote :: (Maybe Footnote)
        , editorialVoiceDirectionLevel :: (Maybe GrpLevel)
        , editorialVoiceDirectionVoice :: (Maybe Voice)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EditorialVoiceDirection where
    emitXml (EditorialVoiceDirection a b c) =
      XReps [emitXml a,emitXml b,emitXml c]
parseEditorialVoiceDirection :: P.XParse EditorialVoiceDirection
parseEditorialVoiceDirection = 
      EditorialVoiceDirection
        <$> P.optional (parseFootnote)
        <*> P.optional (parseGrpLevel)
        <*> P.optional (parseVoice)

-- | Smart constructor for 'EditorialVoiceDirection'
mkEditorialVoiceDirection :: EditorialVoiceDirection
mkEditorialVoiceDirection = EditorialVoiceDirection Nothing Nothing Nothing

-- | @footnote@ /(group)/
data Footnote = 
      Footnote {
          footnoteFootnote :: FormattedText -- ^ /footnote/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Footnote where
    emitXml (Footnote a) =
      XContent XEmpty
        []
        ([XElement (QN "footnote" Nothing) (emitXml a)])
parseFootnote :: P.XParse Footnote
parseFootnote = 
      Footnote
        <$> (P.xchild (P.name "footnote") (parseFormattedText))

-- | Smart constructor for 'Footnote'
mkFootnote :: FormattedText -> Footnote
mkFootnote a = Footnote a

-- | @full-note@ /(group)/
data GrpFullNote = 
      GrpFullNote {
          fullNoteChord :: (Maybe Empty) -- ^ /chord/ child element
        , fullNoteFullNote :: FullNote
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GrpFullNote where
    emitXml (GrpFullNote a b) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "chord" Nothing).emitXml) a]++[emitXml b])
parseGrpFullNote :: P.XParse GrpFullNote
parseGrpFullNote = 
      GrpFullNote
        <$> P.optional (P.xchild (P.name "chord") (parseEmpty))
        <*> parseFullNote

-- | Smart constructor for 'GrpFullNote'
mkGrpFullNote :: FullNote -> GrpFullNote
mkGrpFullNote b = GrpFullNote Nothing b

-- | @harmony-chord@ /(group)/
data HarmonyChord = 
      HarmonyChord {
          harmonyChordHarmonyChord :: ChxHarmonyChord
        , harmonyChordKind :: Kind -- ^ /kind/ child element
        , harmonyChordInversion :: (Maybe Inversion) -- ^ /inversion/ child element
        , harmonyChordBass :: (Maybe Bass) -- ^ /bass/ child element
        , harmonyChordDegree :: [Degree] -- ^ /degree/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HarmonyChord where
    emitXml (HarmonyChord a b c d e) =
      XContent XEmpty
        []
        ([emitXml a]++[XElement (QN "kind" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "inversion" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "bass" Nothing).emitXml) d]++map (XElement (QN "degree" Nothing).emitXml) e)
parseHarmonyChord :: P.XParse HarmonyChord
parseHarmonyChord = 
      HarmonyChord
        <$> parseChxHarmonyChord
        <*> (P.xchild (P.name "kind") (parseKind))
        <*> P.optional (P.xchild (P.name "inversion") (parseInversion))
        <*> P.optional (P.xchild (P.name "bass") (parseBass))
        <*> P.many (P.xchild (P.name "degree") (parseDegree))

-- | Smart constructor for 'HarmonyChord'
mkHarmonyChord :: ChxHarmonyChord -> Kind -> HarmonyChord
mkHarmonyChord a b = HarmonyChord a b Nothing Nothing []

-- | @layout@ /(group)/
data Layout = 
      Layout {
          layoutPageLayout :: (Maybe PageLayout) -- ^ /page-layout/ child element
        , layoutSystemLayout :: (Maybe SystemLayout) -- ^ /system-layout/ child element
        , layoutStaffLayout :: [StaffLayout] -- ^ /staff-layout/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Layout where
    emitXml (Layout a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "page-layout" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "system-layout" Nothing).emitXml) b]++map (XElement (QN "staff-layout" Nothing).emitXml) c)
parseLayout :: P.XParse Layout
parseLayout = 
      Layout
        <$> P.optional (P.xchild (P.name "page-layout") (parsePageLayout))
        <*> P.optional (P.xchild (P.name "system-layout") (parseSystemLayout))
        <*> P.many (P.xchild (P.name "staff-layout") (parseStaffLayout))

-- | Smart constructor for 'Layout'
mkLayout :: Layout
mkLayout = Layout Nothing Nothing []

-- | @left-right-margins@ /(group)/
data LeftRightMargins = 
      LeftRightMargins {
          leftRightMarginsLeftMargin :: Tenths -- ^ /left-margin/ child element
        , leftRightMarginsRightMargin :: Tenths -- ^ /right-margin/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LeftRightMargins where
    emitXml (LeftRightMargins a b) =
      XContent XEmpty
        []
        ([XElement (QN "left-margin" Nothing) (emitXml a)]++[XElement (QN "right-margin" Nothing) (emitXml b)])
parseLeftRightMargins :: P.XParse LeftRightMargins
parseLeftRightMargins = 
      LeftRightMargins
        <$> (P.xchild (P.name "left-margin") (P.xtext >>= parseTenths))
        <*> (P.xchild (P.name "right-margin") (P.xtext >>= parseTenths))

-- | Smart constructor for 'LeftRightMargins'
mkLeftRightMargins :: Tenths -> Tenths -> LeftRightMargins
mkLeftRightMargins a b = LeftRightMargins a b

-- | @level@ /(group)/
data GrpLevel = 
      GrpLevel {
          levelLevel :: Level -- ^ /level/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GrpLevel where
    emitXml (GrpLevel a) =
      XContent XEmpty
        []
        ([XElement (QN "level" Nothing) (emitXml a)])
parseGrpLevel :: P.XParse GrpLevel
parseGrpLevel = 
      GrpLevel
        <$> (P.xchild (P.name "level") (parseLevel))

-- | Smart constructor for 'GrpLevel'
mkGrpLevel :: Level -> GrpLevel
mkGrpLevel a = GrpLevel a

-- | @music-data@ /(group)/
data MusicData = 
      MusicData {
          musicDataMusicData :: [ChxMusicData]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MusicData where
    emitXml (MusicData a) =
      XReps [emitXml a]
parseMusicData :: P.XParse MusicData
parseMusicData = 
      MusicData
        <$> P.many (parseChxMusicData)

-- | Smart constructor for 'MusicData'
mkMusicData :: MusicData
mkMusicData = MusicData []

-- | @non-traditional-key@ /(group)/
data NonTraditionalKey = 
      NonTraditionalKey {
          nonTraditionalKeyKeyStep :: Step -- ^ /key-step/ child element
        , nonTraditionalKeyKeyAlter :: Semitones -- ^ /key-alter/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NonTraditionalKey where
    emitXml (NonTraditionalKey a b) =
      XContent XEmpty
        []
        ([XElement (QN "key-step" Nothing) (emitXml a)]++[XElement (QN "key-alter" Nothing) (emitXml b)])
parseNonTraditionalKey :: P.XParse NonTraditionalKey
parseNonTraditionalKey = 
      NonTraditionalKey
        <$> (P.xchild (P.name "key-step") (P.xtext >>= parseStep))
        <*> (P.xchild (P.name "key-alter") (P.xtext >>= parseSemitones))

-- | Smart constructor for 'NonTraditionalKey'
mkNonTraditionalKey :: Step -> Semitones -> NonTraditionalKey
mkNonTraditionalKey a b = NonTraditionalKey a b

-- | @part-group@ /(group)/
data GrpPartGroup = 
      GrpPartGroup {
          partGroupPartGroup :: PartGroup -- ^ /part-group/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GrpPartGroup where
    emitXml (GrpPartGroup a) =
      XContent XEmpty
        []
        ([XElement (QN "part-group" Nothing) (emitXml a)])
parseGrpPartGroup :: P.XParse GrpPartGroup
parseGrpPartGroup = 
      GrpPartGroup
        <$> (P.xchild (P.name "part-group") (parsePartGroup))

-- | Smart constructor for 'GrpPartGroup'
mkGrpPartGroup :: PartGroup -> GrpPartGroup
mkGrpPartGroup a = GrpPartGroup a

-- | @score-header@ /(group)/
data ScoreHeader = 
      ScoreHeader {
          scoreHeaderWork :: (Maybe Work) -- ^ /work/ child element
        , scoreHeaderMovementNumber :: (Maybe String) -- ^ /movement-number/ child element
        , scoreHeaderMovementTitle :: (Maybe String) -- ^ /movement-title/ child element
        , scoreHeaderIdentification :: (Maybe Identification) -- ^ /identification/ child element
        , scoreHeaderDefaults :: (Maybe Defaults) -- ^ /defaults/ child element
        , scoreHeaderCredit :: [Credit] -- ^ /credit/ child element
        , scoreHeaderPartList :: PartList -- ^ /part-list/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScoreHeader where
    emitXml (ScoreHeader a b c d e f g) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "work" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "movement-number" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "movement-title" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "identification" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "defaults" Nothing).emitXml) e]++map (XElement (QN "credit" Nothing).emitXml) f++[XElement (QN "part-list" Nothing) (emitXml g)])
parseScoreHeader :: P.XParse ScoreHeader
parseScoreHeader = 
      ScoreHeader
        <$> P.optional (P.xchild (P.name "work") (parseWork))
        <*> P.optional (P.xchild (P.name "movement-number") (P.xtext >>= return))
        <*> P.optional (P.xchild (P.name "movement-title") (P.xtext >>= return))
        <*> P.optional (P.xchild (P.name "identification") (parseIdentification))
        <*> P.optional (P.xchild (P.name "defaults") (parseDefaults))
        <*> P.many (P.xchild (P.name "credit") (parseCredit))
        <*> (P.xchild (P.name "part-list") (parsePartList))

-- | Smart constructor for 'ScoreHeader'
mkScoreHeader :: PartList -> ScoreHeader
mkScoreHeader g = ScoreHeader Nothing Nothing Nothing Nothing Nothing [] g

-- | @score-part@ /(group)/
data ScorePart = 
      ScorePart {
          scorePartScorePart :: CmpScorePart -- ^ /score-part/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScorePart where
    emitXml (ScorePart a) =
      XContent XEmpty
        []
        ([XElement (QN "score-part" Nothing) (emitXml a)])
parseScorePart :: P.XParse ScorePart
parseScorePart = 
      ScorePart
        <$> (P.xchild (P.name "score-part") (parseCmpScorePart))

-- | Smart constructor for 'ScorePart'
mkScorePart :: CmpScorePart -> ScorePart
mkScorePart a = ScorePart a

-- | @slash@ /(group)/
data Slash = 
      Slash {
          slashSlashType :: NoteTypeValue -- ^ /slash-type/ child element
        , slashSlashDot :: [Empty] -- ^ /slash-dot/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Slash where
    emitXml (Slash a b) =
      XContent XEmpty
        []
        ([XElement (QN "slash-type" Nothing) (emitXml a)]++map (XElement (QN "slash-dot" Nothing).emitXml) b)
parseSlash :: P.XParse Slash
parseSlash = 
      Slash
        <$> (P.xchild (P.name "slash-type") (P.xtext >>= parseNoteTypeValue))
        <*> P.many (P.xchild (P.name "slash-dot") (parseEmpty))

-- | Smart constructor for 'Slash'
mkSlash :: NoteTypeValue -> Slash
mkSlash a = Slash a []

-- | @staff@ /(group)/
data Staff = 
      Staff {
          staffStaff :: PositiveInteger -- ^ /staff/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Staff where
    emitXml (Staff a) =
      XContent XEmpty
        []
        ([XElement (QN "staff" Nothing) (emitXml a)])
parseStaff :: P.XParse Staff
parseStaff = 
      Staff
        <$> (P.xchild (P.name "staff") (P.xtext >>= parsePositiveInteger))

-- | Smart constructor for 'Staff'
mkStaff :: PositiveInteger -> Staff
mkStaff a = Staff a

-- | @traditional-key@ /(group)/
data TraditionalKey = 
      TraditionalKey {
          traditionalKeyCancel :: (Maybe Cancel) -- ^ /cancel/ child element
        , traditionalKeyFifths :: Fifths -- ^ /fifths/ child element
        , traditionalKeyMode :: (Maybe Mode) -- ^ /mode/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TraditionalKey where
    emitXml (TraditionalKey a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "cancel" Nothing).emitXml) a]++[XElement (QN "fifths" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "mode" Nothing).emitXml) c])
parseTraditionalKey :: P.XParse TraditionalKey
parseTraditionalKey = 
      TraditionalKey
        <$> P.optional (P.xchild (P.name "cancel") (parseCancel))
        <*> (P.xchild (P.name "fifths") (P.xtext >>= parseFifths))
        <*> P.optional (P.xchild (P.name "mode") (P.xtext >>= parseMode))

-- | Smart constructor for 'TraditionalKey'
mkTraditionalKey :: Fifths -> TraditionalKey
mkTraditionalKey b = TraditionalKey Nothing b Nothing

-- | @tuning@ /(group)/
data Tuning = 
      Tuning {
          tuningTuningStep :: Step -- ^ /tuning-step/ child element
        , tuningTuningAlter :: (Maybe Semitones) -- ^ /tuning-alter/ child element
        , tuningTuningOctave :: Octave -- ^ /tuning-octave/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tuning where
    emitXml (Tuning a b c) =
      XContent XEmpty
        []
        ([XElement (QN "tuning-step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "tuning-alter" Nothing).emitXml) b]++[XElement (QN "tuning-octave" Nothing) (emitXml c)])
parseTuning :: P.XParse Tuning
parseTuning = 
      Tuning
        <$> (P.xchild (P.name "tuning-step") (P.xtext >>= parseStep))
        <*> P.optional (P.xchild (P.name "tuning-alter") (P.xtext >>= parseSemitones))
        <*> (P.xchild (P.name "tuning-octave") (P.xtext >>= parseOctave))

-- | Smart constructor for 'Tuning'
mkTuning :: Step -> Octave -> Tuning
mkTuning a c = Tuning a Nothing c

-- | @voice@ /(group)/
data Voice = 
      Voice {
          voiceVoice :: String -- ^ /voice/ child element
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Voice where
    emitXml (Voice a) =
      XContent XEmpty
        []
        ([XElement (QN "voice" Nothing) (emitXml a)])
parseVoice :: P.XParse Voice
parseVoice = 
      Voice
        <$> (P.xchild (P.name "voice") (P.xtext >>= return))

-- | Smart constructor for 'Voice'
mkVoice :: String -> Voice
mkVoice a = Voice a
