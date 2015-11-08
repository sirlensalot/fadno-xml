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


-- xs:ID: Simple
newtype ID = ID { iD :: NCName }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show ID where show (ID a) = show a
instance EmitXml ID where
    emitXml = emitXml . iD

-- xs:IDREF: Simple
newtype IDREF = IDREF { iDREF :: NCName }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show IDREF where show (IDREF a) = show a
instance EmitXml IDREF where
    emitXml = emitXml . iDREF

-- xs:NCName: Simple
newtype NCName = NCName { nCName :: Name }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show NCName where show (NCName a) = show a
instance EmitXml NCName where
    emitXml = emitXml . nCName

-- xs:NMTOKEN: Simple
newtype NMTOKEN = NMTOKEN { nMTOKEN :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show NMTOKEN where show (NMTOKEN a) = show a
instance EmitXml NMTOKEN where
    emitXml = emitXml . nMTOKEN

-- xs:Name: Simple
newtype Name = Name { name :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Name where show (Name a) = show a
instance EmitXml Name where
    emitXml = emitXml . name

-- above-below: Simple
data AboveBelow = 
      AboveBelowAbove
    | AboveBelowBelow
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml AboveBelow where
    emitXml AboveBelowAbove = XLit "above"
    emitXml AboveBelowBelow = XLit "below"

-- accidental-value: Simple
data AccidentalValue = 
      AccidentalValueSharp
    | AccidentalValueNatural
    | AccidentalValueFlat
    | AccidentalValueDoubleSharp
    | AccidentalValueSharpSharp
    | AccidentalValueFlatFlat
    | AccidentalValueNaturalSharp
    | AccidentalValueNaturalFlat
    | AccidentalValueQuarterFlat
    | AccidentalValueQuarterSharp
    | AccidentalValueThreeQuartersFlat
    | AccidentalValueThreeQuartersSharp
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

-- accordion-middle: Simple
newtype AccordionMiddle = AccordionMiddle { accordionMiddle :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show AccordionMiddle where show (AccordionMiddle a) = show a
instance EmitXml AccordionMiddle where
    emitXml = emitXml . accordionMiddle

-- xlink:actuate: Simple
data Actuate = 
      ActuateOnRequest
    | ActuateOnLoad
    | ActuateOther
    | ActuateNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Actuate where
    emitXml ActuateOnRequest = XLit "onRequest"
    emitXml ActuateOnLoad = XLit "onLoad"
    emitXml ActuateOther = XLit "other"
    emitXml ActuateNone = XLit "none"

-- backward-forward: Simple
data BackwardForward = 
      BackwardForwardBackward
    | BackwardForwardForward
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml BackwardForward where
    emitXml BackwardForwardBackward = XLit "backward"
    emitXml BackwardForwardForward = XLit "forward"

-- bar-style: Simple
data BarStyle = 
      BarStyleRegular
    | BarStyleDotted
    | BarStyleDashed
    | BarStyleHeavy
    | BarStyleLightLight
    | BarStyleLightHeavy
    | BarStyleHeavyLight
    | BarStyleHeavyHeavy
    | BarStyleTick
    | BarStyleShort
    | BarStyleNone
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

-- beam-level: Simple
newtype BeamLevel = BeamLevel { beamLevel :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show BeamLevel where show (BeamLevel a) = show a
instance EmitXml BeamLevel where
    emitXml = emitXml . beamLevel

-- beam-value: Simple
data BeamValue = 
      BeamValueBegin
    | BeamValueContinue
    | BeamValueEnd
    | BeamValueForwardHook
    | BeamValueBackwardHook
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml BeamValue where
    emitXml BeamValueBegin = XLit "begin"
    emitXml BeamValueContinue = XLit "continue"
    emitXml BeamValueEnd = XLit "end"
    emitXml BeamValueForwardHook = XLit "forward hook"
    emitXml BeamValueBackwardHook = XLit "backward hook"

-- clef-sign: Simple
data ClefSign = 
      ClefSignG
    | ClefSignF
    | ClefSignC
    | ClefSignPercussion
    | ClefSignTAB
    | ClefSignNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ClefSign where
    emitXml ClefSignG = XLit "G"
    emitXml ClefSignF = XLit "F"
    emitXml ClefSignC = XLit "C"
    emitXml ClefSignPercussion = XLit "percussion"
    emitXml ClefSignTAB = XLit "TAB"
    emitXml ClefSignNone = XLit "none"

-- color: Simple
newtype Color = Color { color :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Color where show (Color a) = show a
instance EmitXml Color where
    emitXml = emitXml . color

-- comma-separated-text: Simple
newtype CommaSeparatedText = CommaSeparatedText { commaSeparatedText :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show CommaSeparatedText where show (CommaSeparatedText a) = show a
instance EmitXml CommaSeparatedText where
    emitXml = emitXml . commaSeparatedText

-- css-font-size: Simple
data CssFontSize = 
      CssFontSizeXxSmall
    | CssFontSizeXSmall
    | CssFontSizeSmall
    | CssFontSizeMedium
    | CssFontSizeLarge
    | CssFontSizeXLarge
    | CssFontSizeXxLarge
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml CssFontSize where
    emitXml CssFontSizeXxSmall = XLit "xx-small"
    emitXml CssFontSizeXSmall = XLit "x-small"
    emitXml CssFontSizeSmall = XLit "small"
    emitXml CssFontSizeMedium = XLit "medium"
    emitXml CssFontSizeLarge = XLit "large"
    emitXml CssFontSizeXLarge = XLit "x-large"
    emitXml CssFontSizeXxLarge = XLit "xx-large"

-- degree-type-value: Simple
data DegreeTypeValue = 
      DegreeTypeValueAdd
    | DegreeTypeValueAlter
    | DegreeTypeValueSubtract
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml DegreeTypeValue where
    emitXml DegreeTypeValueAdd = XLit "add"
    emitXml DegreeTypeValueAlter = XLit "alter"
    emitXml DegreeTypeValueSubtract = XLit "subtract"

-- divisions: Simple
newtype Divisions = Divisions { divisions :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Divisions where show (Divisions a) = show a
instance EmitXml Divisions where
    emitXml = emitXml . divisions

-- enclosure: Simple
data Enclosure = 
      EnclosureRectangle
    | EnclosureOval
    | EnclosureNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Enclosure where
    emitXml EnclosureRectangle = XLit "rectangle"
    emitXml EnclosureOval = XLit "oval"
    emitXml EnclosureNone = XLit "none"

-- ending-number: Simple
newtype EndingNumber = EndingNumber { endingNumber :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show EndingNumber where show (EndingNumber a) = show a
instance EmitXml EndingNumber where
    emitXml = emitXml . endingNumber

-- fan: Simple
data Fan = 
      FanAccel
    | FanRit
    | FanNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Fan where
    emitXml FanAccel = XLit "accel"
    emitXml FanRit = XLit "rit"
    emitXml FanNone = XLit "none"

-- fermata-shape: Simple
data FermataShape = 
      FermataShapeNormal
    | FermataShapeAngled
    | FermataShapeSquare
    | FermataShape
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml FermataShape where
    emitXml FermataShapeNormal = XLit "normal"
    emitXml FermataShapeAngled = XLit "angled"
    emitXml FermataShapeSquare = XLit "square"
    emitXml FermataShape = XLit ""

-- fifths: Simple
newtype Fifths = Fifths { fifths :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Fifths where show (Fifths a) = show a
instance EmitXml Fifths where
    emitXml = emitXml . fifths

-- font-size: Simple
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

-- font-style: Simple
data FontStyle = 
      FontStyleNormal
    | FontStyleItalic
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml FontStyle where
    emitXml FontStyleNormal = XLit "normal"
    emitXml FontStyleItalic = XLit "italic"

-- font-weight: Simple
data FontWeight = 
      FontWeightNormal
    | FontWeightBold
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml FontWeight where
    emitXml FontWeightNormal = XLit "normal"
    emitXml FontWeightBold = XLit "bold"

-- group-barline-value: Simple
data GroupBarlineValue = 
      GroupBarlineValueYes
    | GroupBarlineValueNo
    | GroupBarlineValueMensurstrich
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml GroupBarlineValue where
    emitXml GroupBarlineValueYes = XLit "yes"
    emitXml GroupBarlineValueNo = XLit "no"
    emitXml GroupBarlineValueMensurstrich = XLit "Mensurstrich"

-- group-symbol-value: Simple
data GroupSymbolValue = 
      GroupSymbolValueNone
    | GroupSymbolValueBrace
    | GroupSymbolValueLine
    | GroupSymbolValueBracket
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml GroupSymbolValue where
    emitXml GroupSymbolValueNone = XLit "none"
    emitXml GroupSymbolValueBrace = XLit "brace"
    emitXml GroupSymbolValueLine = XLit "line"
    emitXml GroupSymbolValueBracket = XLit "bracket"

-- harmony-type: Simple
data HarmonyType = 
      HarmonyTypeExplicit
    | HarmonyTypeImplied
    | HarmonyTypeAlternate
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml HarmonyType where
    emitXml HarmonyTypeExplicit = XLit "explicit"
    emitXml HarmonyTypeImplied = XLit "implied"
    emitXml HarmonyTypeAlternate = XLit "alternate"

-- kind-value: Simple
data KindValue = 
      KindValueMajor
    | KindValueMinor
    | KindValueAugmented
    | KindValueDiminished
    | KindValueDominant
    | KindValueMajorSeventh
    | KindValueMinorSeventh
    | KindValueDiminishedSeventh
    | KindValueAugmentedSeventh
    | KindValueHalfDiminished
    | KindValueMajorMinor
    | KindValueMajorSixth
    | KindValueMinorSixth
    | KindValueDominantNinth
    | KindValueMajorNinth
    | KindValueMinorNinth
    | KindValueDominant11th
    | KindValueMajor11th
    | KindValueMinor11th
    | KindValueDominant13th
    | KindValueMajor13th
    | KindValueMinor13th
    | KindValueSuspendedSecond
    | KindValueSuspendedFourth
    | KindValueNeapolitan
    | KindValueItalian
    | KindValueFrench
    | KindValueGerman
    | KindValuePedal
    | KindValuePower
    | KindValueTristan
    | KindValueOther
    | KindValueNone
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

-- xml:lang: Simple
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

-- xs:language: Simple
newtype Language = Language { language :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Language where show (Language a) = show a
instance EmitXml Language where
    emitXml = emitXml . language

-- left-center-right: Simple
data LeftCenterRight = 
      LeftCenterRightLeft
    | LeftCenterRightCenter
    | LeftCenterRightRight
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LeftCenterRight where
    emitXml LeftCenterRightLeft = XLit "left"
    emitXml LeftCenterRightCenter = XLit "center"
    emitXml LeftCenterRightRight = XLit "right"

-- left-right: Simple
data LeftRight = 
      LeftRightLeft
    | LeftRightRight
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LeftRight where
    emitXml LeftRightLeft = XLit "left"
    emitXml LeftRightRight = XLit "right"

-- line-end: Simple
data LineEnd = 
      LineEndUp
    | LineEndDown
    | LineEndBoth
    | LineEndArrow
    | LineEndNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LineEnd where
    emitXml LineEndUp = XLit "up"
    emitXml LineEndDown = XLit "down"
    emitXml LineEndBoth = XLit "both"
    emitXml LineEndArrow = XLit "arrow"
    emitXml LineEndNone = XLit "none"

-- line-shape: Simple
data LineShape = 
      LineShapeStraight
    | LineShapeCurved
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LineShape where
    emitXml LineShapeStraight = XLit "straight"
    emitXml LineShapeCurved = XLit "curved"

-- line-type: Simple
data LineType = 
      LineTypeSolid
    | LineTypeDashed
    | LineTypeDotted
    | LineTypeWavy
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml LineType where
    emitXml LineTypeSolid = XLit "solid"
    emitXml LineTypeDashed = XLit "dashed"
    emitXml LineTypeDotted = XLit "dotted"
    emitXml LineTypeWavy = XLit "wavy"

-- line-width-type: Simple
newtype LineWidthType = LineWidthType { lineWidthType :: Token }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show LineWidthType where show (LineWidthType a) = show a
instance EmitXml LineWidthType where
    emitXml = emitXml . lineWidthType

-- margin-type: Simple
data MarginType = 
      MarginTypeOdd
    | MarginTypeEven
    | MarginTypeBoth
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml MarginType where
    emitXml MarginTypeOdd = XLit "odd"
    emitXml MarginTypeEven = XLit "even"
    emitXml MarginTypeBoth = XLit "both"

-- measure-numbering-value: Simple
data MeasureNumberingValue = 
      MeasureNumberingValueNone
    | MeasureNumberingValueMeasure
    | MeasureNumberingValueSystem
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml MeasureNumberingValue where
    emitXml MeasureNumberingValueNone = XLit "none"
    emitXml MeasureNumberingValueMeasure = XLit "measure"
    emitXml MeasureNumberingValueSystem = XLit "system"

-- midi-128: Simple
newtype Midi128 = Midi128 { midi128 :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Midi128 where show (Midi128 a) = show a
instance EmitXml Midi128 where
    emitXml = emitXml . midi128

-- midi-16: Simple
newtype Midi16 = Midi16 { midi16 :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Midi16 where show (Midi16 a) = show a
instance EmitXml Midi16 where
    emitXml = emitXml . midi16

-- midi-16384: Simple
newtype Midi16384 = Midi16384 { midi16384 :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Midi16384 where show (Midi16384 a) = show a
instance EmitXml Midi16384 where
    emitXml = emitXml . midi16384

-- millimeters: Simple
newtype Millimeters = Millimeters { millimeters :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Millimeters where show (Millimeters a) = show a
instance EmitXml Millimeters where
    emitXml = emitXml . millimeters

-- mode: Simple
newtype Mode = Mode { mode :: String }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Mode where show (Mode a) = show a
instance EmitXml Mode where
    emitXml = emitXml . mode

-- non-negative-decimal: Simple
newtype NonNegativeDecimal = NonNegativeDecimal { nonNegativeDecimal :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show NonNegativeDecimal where show (NonNegativeDecimal a) = show a
instance EmitXml NonNegativeDecimal where
    emitXml = emitXml . nonNegativeDecimal

-- xs:nonNegativeInteger: Simple
newtype NonNegativeInteger = NonNegativeInteger { nonNegativeInteger :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show NonNegativeInteger where show (NonNegativeInteger a) = show a
instance EmitXml NonNegativeInteger where
    emitXml = emitXml . nonNegativeInteger

-- xs:normalizedString: Simple
newtype NormalizedString = NormalizedString { normalizedString :: String }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show NormalizedString where show (NormalizedString a) = show a
instance EmitXml NormalizedString where
    emitXml = emitXml . normalizedString

-- note-size-type: Simple
data NoteSizeType = 
      NoteSizeTypeCue
    | NoteSizeTypeGrace
    | NoteSizeTypeLarge
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml NoteSizeType where
    emitXml NoteSizeTypeCue = XLit "cue"
    emitXml NoteSizeTypeGrace = XLit "grace"
    emitXml NoteSizeTypeLarge = XLit "large"

-- note-type-value: Simple
data NoteTypeValue = 
      NoteTypeValue256th
    | NoteTypeValue128th
    | NoteTypeValue64th
    | NoteTypeValue32nd
    | NoteTypeValue16th
    | NoteTypeValueEighth
    | NoteTypeValueQuarter
    | NoteTypeValueHalf
    | NoteTypeValueWhole
    | NoteTypeValueBreve
    | NoteTypeValueLong
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

-- notehead-value: Simple
data NoteheadValue = 
      NoteheadValueSlash
    | NoteheadValueTriangle
    | NoteheadValueDiamond
    | NoteheadValueSquare
    | NoteheadValueCross
    | NoteheadValueX
    | NoteheadValueCircleX
    | NoteheadValueInvertedTriangle
    | NoteheadValueArrowDown
    | NoteheadValueArrowUp
    | NoteheadValueSlashed
    | NoteheadValueBackSlashed
    | NoteheadValueNormal
    | NoteheadValueCluster
    | NoteheadValueNone
    | NoteheadValueDo
    | NoteheadValueRe
    | NoteheadValueMi
    | NoteheadValueFa
    | NoteheadValueSo
    | NoteheadValueLa
    | NoteheadValueTi
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

-- number-level: Simple
newtype NumberLevel = NumberLevel { numberLevel :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show NumberLevel where show (NumberLevel a) = show a
instance EmitXml NumberLevel where
    emitXml = emitXml . numberLevel

-- number-of-lines: Simple
newtype NumberOfLines = NumberOfLines { numberOfLines :: NonNegativeInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show NumberOfLines where show (NumberOfLines a) = show a
instance EmitXml NumberOfLines where
    emitXml = emitXml . numberOfLines

-- number-or-normal: Simple
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

-- octave: Simple
newtype Octave = Octave { octave :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show Octave where show (Octave a) = show a
instance EmitXml Octave where
    emitXml = emitXml . octave

-- over-under: Simple
data OverUnder = 
      OverUnderOver
    | OverUnderUnder
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml OverUnder where
    emitXml OverUnderOver = XLit "over"
    emitXml OverUnderUnder = XLit "under"

-- percent: Simple
newtype Percent = Percent { percent :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Percent where show (Percent a) = show a
instance EmitXml Percent where
    emitXml = emitXml . percent

-- positive-divisions: Simple
newtype PositiveDivisions = PositiveDivisions { positiveDivisions :: Divisions }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show PositiveDivisions where show (PositiveDivisions a) = show a
instance EmitXml PositiveDivisions where
    emitXml = emitXml . positiveDivisions

-- positive-integer-or-empty: Simple
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

-- xs:positiveInteger: Simple
newtype PositiveInteger = PositiveInteger { positiveInteger :: NonNegativeInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show PositiveInteger where show (PositiveInteger a) = show a
instance EmitXml PositiveInteger where
    emitXml = emitXml . positiveInteger

-- rehearsal-enclosure: Simple
data RehearsalEnclosure = 
      RehearsalEnclosureSquare
    | RehearsalEnclosureCircle
    | RehearsalEnclosureNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml RehearsalEnclosure where
    emitXml RehearsalEnclosureSquare = XLit "square"
    emitXml RehearsalEnclosureCircle = XLit "circle"
    emitXml RehearsalEnclosureNone = XLit "none"

-- right-left-middle: Simple
data RightLeftMiddle = 
      RightLeftMiddleRight
    | RightLeftMiddleLeft
    | RightLeftMiddleMiddle
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml RightLeftMiddle where
    emitXml RightLeftMiddleRight = XLit "right"
    emitXml RightLeftMiddleLeft = XLit "left"
    emitXml RightLeftMiddleMiddle = XLit "middle"

-- rotation-degrees: Simple
newtype RotationDegrees = RotationDegrees { rotationDegrees :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show RotationDegrees where show (RotationDegrees a) = show a
instance EmitXml RotationDegrees where
    emitXml = emitXml . rotationDegrees

-- semitones: Simple
newtype Semitones = Semitones { semitones :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Semitones where show (Semitones a) = show a
instance EmitXml Semitones where
    emitXml = emitXml . semitones

-- xlink:show: Simple
data SmpShow = 
      ShowNew
    | ShowReplace
    | ShowEmbed
    | ShowOther
    | ShowNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SmpShow where
    emitXml ShowNew = XLit "new"
    emitXml ShowReplace = XLit "replace"
    emitXml ShowEmbed = XLit "embed"
    emitXml ShowOther = XLit "other"
    emitXml ShowNone = XLit "none"

-- show-frets: Simple
data ShowFrets = 
      ShowFretsNumbers
    | ShowFretsLetters
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ShowFrets where
    emitXml ShowFretsNumbers = XLit "numbers"
    emitXml ShowFretsLetters = XLit "letters"

-- show-tuplet: Simple
data ShowTuplet = 
      ShowTupletActual
    | ShowTupletBoth
    | ShowTupletNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ShowTuplet where
    emitXml ShowTupletActual = XLit "actual"
    emitXml ShowTupletBoth = XLit "both"
    emitXml ShowTupletNone = XLit "none"

-- staff-line: Simple
newtype StaffLine = StaffLine { staffLine :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show StaffLine where show (StaffLine a) = show a
instance EmitXml StaffLine where
    emitXml = emitXml . staffLine

-- staff-number: Simple
newtype StaffNumber = StaffNumber { staffNumber :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show StaffNumber where show (StaffNumber a) = show a
instance EmitXml StaffNumber where
    emitXml = emitXml . staffNumber

-- staff-type: Simple
data StaffType = 
      StaffTypeOssia
    | StaffTypeCue
    | StaffTypeEditorial
    | StaffTypeRegular
    | StaffTypeAlternate
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StaffType where
    emitXml StaffTypeOssia = XLit "ossia"
    emitXml StaffTypeCue = XLit "cue"
    emitXml StaffTypeEditorial = XLit "editorial"
    emitXml StaffTypeRegular = XLit "regular"
    emitXml StaffTypeAlternate = XLit "alternate"

-- start-note: Simple
data StartNote = 
      StartNoteUpper
    | StartNoteMain
    | StartNoteBelow
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartNote where
    emitXml StartNoteUpper = XLit "upper"
    emitXml StartNoteMain = XLit "main"
    emitXml StartNoteBelow = XLit "below"

-- start-stop: Simple
data StartStop = 
      StartStopStart
    | StartStopStop
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStop where
    emitXml StartStopStart = XLit "start"
    emitXml StartStopStop = XLit "stop"

-- start-stop-change: Simple
data StartStopChange = 
      StartStopChangeStart
    | StartStopChangeStop
    | StartStopChangeChange
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopChange where
    emitXml StartStopChangeStart = XLit "start"
    emitXml StartStopChangeStop = XLit "stop"
    emitXml StartStopChangeChange = XLit "change"

-- start-stop-continue: Simple
data StartStopContinue = 
      StartStopContinueStart
    | StartStopContinueStop
    | StartStopContinueContinue
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopContinue where
    emitXml StartStopContinueStart = XLit "start"
    emitXml StartStopContinueStop = XLit "stop"
    emitXml StartStopContinueContinue = XLit "continue"

-- start-stop-discontinue: Simple
data StartStopDiscontinue = 
      StartStopDiscontinueStart
    | StartStopDiscontinueStop
    | StartStopDiscontinueDiscontinue
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopDiscontinue where
    emitXml StartStopDiscontinueStart = XLit "start"
    emitXml StartStopDiscontinueStop = XLit "stop"
    emitXml StartStopDiscontinueDiscontinue = XLit "discontinue"

-- start-stop-single: Simple
data StartStopSingle = 
      StartStopSingleStart
    | StartStopSingleStop
    | StartStopSingleSingle
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StartStopSingle where
    emitXml StartStopSingleStart = XLit "start"
    emitXml StartStopSingleStop = XLit "stop"
    emitXml StartStopSingleSingle = XLit "single"

-- stem-value: Simple
data StemValue = 
      StemValueDown
    | StemValueUp
    | StemValueDouble
    | StemValueNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml StemValue where
    emitXml StemValueDown = XLit "down"
    emitXml StemValueUp = XLit "up"
    emitXml StemValueDouble = XLit "double"
    emitXml StemValueNone = XLit "none"

-- step: Simple
data Step = 
      StepA
    | StepB
    | StepC
    | StepD
    | StepE
    | StepF
    | StepG
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Step where
    emitXml StepA = XLit "A"
    emitXml StepB = XLit "B"
    emitXml StepC = XLit "C"
    emitXml StepD = XLit "D"
    emitXml StepE = XLit "E"
    emitXml StepF = XLit "F"
    emitXml StepG = XLit "G"

-- string-number: Simple
newtype StringNumber = StringNumber { stringNumber :: PositiveInteger }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show StringNumber where show (StringNumber a) = show a
instance EmitXml StringNumber where
    emitXml = emitXml . stringNumber

-- syllabic: Simple
data Syllabic = 
      SyllabicSingle
    | SyllabicBegin
    | SyllabicEnd
    | SyllabicMiddle
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Syllabic where
    emitXml SyllabicSingle = XLit "single"
    emitXml SyllabicBegin = XLit "begin"
    emitXml SyllabicEnd = XLit "end"
    emitXml SyllabicMiddle = XLit "middle"

-- symbol-size: Simple
data SymbolSize = 
      SymbolSizeFull
    | SymbolSizeCue
    | SymbolSizeLarge
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SymbolSize where
    emitXml SymbolSizeFull = XLit "full"
    emitXml SymbolSizeCue = XLit "cue"
    emitXml SymbolSizeLarge = XLit "large"

-- tenths: Simple
newtype Tenths = Tenths { tenths :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show Tenths where show (Tenths a) = show a
instance EmitXml Tenths where
    emitXml = emitXml . tenths

-- text-direction: Simple
data TextDirection = 
      TextDirectionLtr
    | TextDirectionRtl
    | TextDirectionLro
    | TextDirectionRlo
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TextDirection where
    emitXml TextDirectionLtr = XLit "ltr"
    emitXml TextDirectionRtl = XLit "rtl"
    emitXml TextDirectionLro = XLit "lro"
    emitXml TextDirectionRlo = XLit "rlo"

-- time-symbol: Simple
data TimeSymbol = 
      TimeSymbolCommon
    | TimeSymbolCut
    | TimeSymbolSingleNumber
    | TimeSymbolNormal
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TimeSymbol where
    emitXml TimeSymbolCommon = XLit "common"
    emitXml TimeSymbolCut = XLit "cut"
    emitXml TimeSymbolSingleNumber = XLit "single-number"
    emitXml TimeSymbolNormal = XLit "normal"

-- xs:token: Simple
newtype Token = Token { token :: NormalizedString }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show Token where show (Token a) = show a
instance EmitXml Token where
    emitXml = emitXml . token

-- top-bottom: Simple
data TopBottom = 
      TopBottomTop
    | TopBottomBottom
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TopBottom where
    emitXml TopBottomTop = XLit "top"
    emitXml TopBottomBottom = XLit "bottom"

-- tremolo-marks: Simple
newtype TremoloMarks = TremoloMarks { tremoloMarks :: Int }
    deriving (Eq,Typeable,Generic,Ord,Bounded,Enum,Num,Real,Integral)
instance Show TremoloMarks where show (TremoloMarks a) = show a
instance EmitXml TremoloMarks where
    emitXml = emitXml . tremoloMarks

-- trill-beats: Simple
newtype TrillBeats = TrillBeats { trillBeats :: Decimal }
    deriving (Eq,Typeable,Generic,Ord,Num,Real,Fractional,RealFrac)
instance Show TrillBeats where show (TrillBeats a) = show a
instance EmitXml TrillBeats where
    emitXml = emitXml . trillBeats

-- trill-step: Simple
data TrillStep = 
      TrillStepWhole
    | TrillStepHalf
    | TrillStepUnison
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TrillStep where
    emitXml TrillStepWhole = XLit "whole"
    emitXml TrillStepHalf = XLit "half"
    emitXml TrillStepUnison = XLit "unison"

-- two-note-turn: Simple
data TwoNoteTurn = 
      TwoNoteTurnWhole
    | TwoNoteTurnHalf
    | TwoNoteTurnNone
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml TwoNoteTurn where
    emitXml TwoNoteTurnWhole = XLit "whole"
    emitXml TwoNoteTurnHalf = XLit "half"
    emitXml TwoNoteTurnNone = XLit "none"

-- xlink:type: Simple
data Type = 
      TypeSimple
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Type where
    emitXml TypeSimple = XLit "simple"

-- up-down: Simple
data UpDown = 
      UpDownUp
    | UpDownDown
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml UpDown where
    emitXml UpDownUp = XLit "up"
    emitXml UpDownDown = XLit "down"

-- up-down-stop: Simple
data UpDownStop = 
      UpDownStopUp
    | UpDownStopDown
    | UpDownStopStop
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml UpDownStop where
    emitXml UpDownStopUp = XLit "up"
    emitXml UpDownStopDown = XLit "down"
    emitXml UpDownStopStop = XLit "stop"

-- upright-inverted: Simple
data UprightInverted = 
      UprightInvertedUpright
    | UprightInvertedInverted
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml UprightInverted where
    emitXml UprightInvertedUpright = XLit "upright"
    emitXml UprightInvertedInverted = XLit "inverted"

-- valign: Simple
data Valign = 
      ValignTop
    | ValignMiddle
    | ValignBottom
    | ValignBaseline
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml Valign where
    emitXml ValignTop = XLit "top"
    emitXml ValignMiddle = XLit "middle"
    emitXml ValignBottom = XLit "bottom"
    emitXml ValignBaseline = XLit "baseline"

-- valign-image: Simple
data ValignImage = 
      ValignImageTop
    | ValignImageMiddle
    | ValignImageBottom
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml ValignImage where
    emitXml ValignImageTop = XLit "top"
    emitXml ValignImageMiddle = XLit "middle"
    emitXml ValignImageBottom = XLit "bottom"

-- wedge-type: Simple
data WedgeType = 
      WedgeTypeCrescendo
    | WedgeTypeDiminuendo
    | WedgeTypeStop
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml WedgeType where
    emitXml WedgeTypeCrescendo = XLit "crescendo"
    emitXml WedgeTypeDiminuendo = XLit "diminuendo"
    emitXml WedgeTypeStop = XLit "stop"

-- yes-no: Simple
data YesNo = 
      YesNoYes
    | YesNoNo
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml YesNo where
    emitXml YesNoYes = XLit "yes"
    emitXml YesNoNo = XLit "no"

-- yes-no-number: Simple
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

-- yyyy-mm-dd: Simple
newtype YyyyMmDd = YyyyMmDd { yyyyMmDd :: String }
    deriving (Eq,Typeable,Generic,Ord,IsString)
instance Show YyyyMmDd where show (YyyyMmDd a) = show a
instance EmitXml YyyyMmDd where
    emitXml = emitXml . yyyyMmDd

-- xml:lang: Union
data SumLang = 
      SumLang
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SumLang where
    emitXml SumLang = XLit ""

-- number-or-normal: Union
data SumNumberOrNormal = 
      NumberOrNormalNormal
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SumNumberOrNormal where
    emitXml NumberOrNormalNormal = XLit "normal"

-- positive-integer-or-empty: Union
data SumPositiveIntegerOrEmpty = 
      SumPositiveIntegerOrEmpty
    deriving (Eq,Typeable,Generic,Show,Ord,Enum,Bounded)
instance EmitXml SumPositiveIntegerOrEmpty where
    emitXml SumPositiveIntegerOrEmpty = XLit ""

-- accidental: Complex
data Accidental = 
      Accidental {
          accidentalAccidentalValue :: AccidentalValue
        , accidentalCautionary :: (Maybe YesNo)
        , accidentalEditorial :: (Maybe YesNo)
        , accidentalParentheses :: (Maybe YesNo)
        , accidentalBracket :: (Maybe YesNo)
        , accidentalSize :: (Maybe SymbolSize)
        , accidentalDefaultX :: (Maybe Tenths)
        , accidentalDefaultY :: (Maybe Tenths)
        , accidentalRelativeX :: (Maybe Tenths)
        , accidentalRelativeY :: (Maybe Tenths)
        , accidentalFontFamily :: (Maybe CommaSeparatedText)
        , accidentalFontStyle :: (Maybe FontStyle)
        , accidentalFontSize :: (Maybe FontSize)
        , accidentalFontWeight :: (Maybe FontWeight)
        , accidentalColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Accidental where
    emitXml (Accidental a b c d e f g h i j k l m n o) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "cautionary" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "editorial" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o])
        []
mkAccidental :: AccidentalValue -> Accidental
mkAccidental a = Accidental a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- accidental-mark: Complex
data AccidentalMark = 
      AccidentalMark {
          accidentalMarkAccidentalValue :: AccidentalValue
        , accidentalMarkDefaultX :: (Maybe Tenths)
        , accidentalMarkDefaultY :: (Maybe Tenths)
        , accidentalMarkRelativeX :: (Maybe Tenths)
        , accidentalMarkRelativeY :: (Maybe Tenths)
        , accidentalMarkFontFamily :: (Maybe CommaSeparatedText)
        , accidentalMarkFontStyle :: (Maybe FontStyle)
        , accidentalMarkFontSize :: (Maybe FontSize)
        , accidentalMarkFontWeight :: (Maybe FontWeight)
        , accidentalMarkColor :: (Maybe Color)
        , accidentalMarkPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AccidentalMark where
    emitXml (AccidentalMark a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        []
mkAccidentalMark :: AccidentalValue -> AccidentalMark
mkAccidentalMark a = AccidentalMark a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- accidental-text: Complex
data AccidentalText = 
      AccidentalText {
          accidentalTextAccidentalValue :: AccidentalValue
        , accidentalTextLang :: (Maybe Lang)
        , accidentalTextEnclosure :: (Maybe Enclosure)
        , accidentalTextJustify :: (Maybe LeftCenterRight)
        , accidentalTextHalign :: (Maybe LeftCenterRight)
        , accidentalTextValign :: (Maybe Valign)
        , accidentalTextDefaultX :: (Maybe Tenths)
        , accidentalTextDefaultY :: (Maybe Tenths)
        , accidentalTextRelativeX :: (Maybe Tenths)
        , accidentalTextRelativeY :: (Maybe Tenths)
        , accidentalTextFontFamily :: (Maybe CommaSeparatedText)
        , accidentalTextFontStyle :: (Maybe FontStyle)
        , accidentalTextFontSize :: (Maybe FontSize)
        , accidentalTextFontWeight :: (Maybe FontWeight)
        , accidentalTextColor :: (Maybe Color)
        , accidentalTextUnderline :: (Maybe NumberOfLines)
        , accidentalTextOverline :: (Maybe NumberOfLines)
        , accidentalTextLineThrough :: (Maybe NumberOfLines)
        , accidentalTextRotation :: (Maybe RotationDegrees)
        , accidentalTextLetterSpacing :: (Maybe NumberOrNormal)
        , accidentalTextLineHeight :: (Maybe NumberOrNormal)
        , accidentalTextDir :: (Maybe TextDirection)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AccidentalText where
    emitXml (AccidentalText a b c d e f g h i j k l m n o p q r s t u v) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "enclosure" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) q]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) r]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) s]++[maybe XEmpty (XAttr (QN "letter-spacing" Nothing).emitXml) t]++[maybe XEmpty (XAttr (QN "line-height" Nothing).emitXml) u]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) v])
        []
mkAccidentalText :: AccidentalValue -> AccidentalText
mkAccidentalText a = AccidentalText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- accord: Complex
data Accord = 
      Accord {
          accordString :: (Maybe StringNumber)
        , accordTuning :: Tuning
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Accord where
    emitXml (Accord a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "string" Nothing).emitXml) a])
        ([emitXml b])
mkAccord :: Tuning -> Accord
mkAccord b = Accord Nothing b

-- accordion-registration: Complex
data AccordionRegistration = 
      AccordionRegistration {
          accordionRegistrationDefaultX :: (Maybe Tenths)
        , accordionRegistrationDefaultY :: (Maybe Tenths)
        , accordionRegistrationRelativeX :: (Maybe Tenths)
        , accordionRegistrationRelativeY :: (Maybe Tenths)
        , accordionRegistrationFontFamily :: (Maybe CommaSeparatedText)
        , accordionRegistrationFontStyle :: (Maybe FontStyle)
        , accordionRegistrationFontSize :: (Maybe FontSize)
        , accordionRegistrationFontWeight :: (Maybe FontWeight)
        , accordionRegistrationColor :: (Maybe Color)
        , accordionRegistrationAccordionHigh :: (Maybe Empty)
        , accordionRegistrationAccordionMiddle :: (Maybe AccordionMiddle)
        , accordionRegistrationAccordionLow :: (Maybe Empty)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AccordionRegistration where
    emitXml (AccordionRegistration a b c d e f g h i j k l) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        ([maybe XEmpty (XElement (QN "accordion-high" Nothing).emitXml) j]++[maybe XEmpty (XElement (QN "accordion-middle" Nothing).emitXml) k]++[maybe XEmpty (XElement (QN "accordion-low" Nothing).emitXml) l])
mkAccordionRegistration :: AccordionRegistration
mkAccordionRegistration = AccordionRegistration Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- appearance: Complex
data Appearance = 
      Appearance {
          appearanceLineWidth :: [LineWidth]
        , appearanceNoteSize :: [NoteSize]
        , appearanceOtherAppearance :: [OtherAppearance]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Appearance where
    emitXml (Appearance a b c) =
      XContent XEmpty
        []
        (map (XElement (QN "line-width" Nothing).emitXml) a++map (XElement (QN "note-size" Nothing).emitXml) b++map (XElement (QN "other-appearance" Nothing).emitXml) c)
mkAppearance :: Appearance
mkAppearance = Appearance [] [] []

-- arpeggiate: Complex
data Arpeggiate = 
      Arpeggiate {
          arpeggiateNumber :: (Maybe NumberLevel)
        , arpeggiateDirection :: (Maybe UpDown)
        , arpeggiateDefaultX :: (Maybe Tenths)
        , arpeggiateDefaultY :: (Maybe Tenths)
        , arpeggiateRelativeX :: (Maybe Tenths)
        , arpeggiateRelativeY :: (Maybe Tenths)
        , arpeggiatePlacement :: (Maybe AboveBelow)
        , arpeggiateColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Arpeggiate where
    emitXml (Arpeggiate a b c d e f g h) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "direction" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
mkArpeggiate :: Arpeggiate
mkArpeggiate = Arpeggiate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- articulations: Complex
data Articulations = 
      Articulations {
          articulationsArticulations :: [ChxArticulations]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Articulations where
    emitXml (Articulations a) =
      XReps [emitXml a]
mkArticulations :: Articulations
mkArticulations = Articulations []

-- attributes: Complex
data Attributes = 
      Attributes {
          attributesEditorial :: Editorial
        , attributesDivisions :: (Maybe PositiveDivisions)
        , attributesKey :: [Key]
        , attributesTime :: [Time]
        , attributesStaves :: (Maybe NonNegativeInteger)
        , attributesPartSymbol :: (Maybe PartSymbol)
        , attributesInstruments :: (Maybe NonNegativeInteger)
        , attributesClef :: [Clef]
        , attributesStaffDetails :: [StaffDetails]
        , attributesTranspose :: (Maybe Transpose)
        , attributesDirective :: [Directive]
        , attributesMeasureStyle :: [MeasureStyle]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Attributes where
    emitXml (Attributes a b c d e f g h i j k l) =
      XContent XEmpty
        []
        ([emitXml a]++[maybe XEmpty (XElement (QN "divisions" Nothing).emitXml) b]++map (XElement (QN "key" Nothing).emitXml) c++map (XElement (QN "time" Nothing).emitXml) d++[maybe XEmpty (XElement (QN "staves" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "part-symbol" Nothing).emitXml) f]++[maybe XEmpty (XElement (QN "instruments" Nothing).emitXml) g]++map (XElement (QN "clef" Nothing).emitXml) h++map (XElement (QN "staff-details" Nothing).emitXml) i++[maybe XEmpty (XElement (QN "transpose" Nothing).emitXml) j]++map (XElement (QN "directive" Nothing).emitXml) k++map (XElement (QN "measure-style" Nothing).emitXml) l)
mkAttributes :: Editorial -> Attributes
mkAttributes a = Attributes a Nothing [] [] Nothing Nothing Nothing [] [] Nothing [] []

-- backup: Complex
data Backup = 
      Backup {
          backupDuration :: Duration
        , backupEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Backup where
    emitXml (Backup a b) =
      XReps [emitXml a,emitXml b]
mkBackup :: Duration -> Editorial -> Backup
mkBackup a b = Backup a b

-- bar-style-color: Complex
data BarStyleColor = 
      BarStyleColor {
          barStyleColorBarStyle :: BarStyle
        , barStyleColorColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BarStyleColor where
    emitXml (BarStyleColor a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "color" Nothing).emitXml) b])
        []
mkBarStyleColor :: BarStyle -> BarStyleColor
mkBarStyleColor a = BarStyleColor a Nothing

-- barline: Complex
data Barline = 
      Barline {
          barlineLocation :: (Maybe RightLeftMiddle)
        , barlineSegno :: (Maybe Token)
        , barlineCoda :: (Maybe Token)
        , barlineDivisions :: (Maybe Divisions)
        , barlineBarStyle :: (Maybe BarStyleColor)
        , barlineEditorial :: Editorial
        , barlineWavyLine :: (Maybe WavyLine)
        , barlineSegno1 :: (Maybe EmptyPrintStyle)
        , barlineCoda1 :: (Maybe EmptyPrintStyle)
        , barlineFermata :: [Fermata]
        , barlineEnding :: (Maybe Ending)
        , barlineRepeat :: (Maybe Repeat)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Barline where
    emitXml (Barline a b c d e f g h i j k l) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "segno" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "coda" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "divisions" Nothing).emitXml) d])
        ([maybe XEmpty (XElement (QN "bar-style" Nothing).emitXml) e]++[emitXml f]++[maybe XEmpty (XElement (QN "wavy-line" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "segno" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "coda" Nothing).emitXml) i]++map (XElement (QN "fermata" Nothing).emitXml) j++[maybe XEmpty (XElement (QN "ending" Nothing).emitXml) k]++[maybe XEmpty (XElement (QN "repeat" Nothing).emitXml) l])
mkBarline :: Editorial -> Barline
mkBarline f = Barline Nothing Nothing Nothing Nothing Nothing f Nothing Nothing Nothing [] Nothing Nothing

-- barre: Complex
data Barre = 
      Barre {
          barreType :: StartStop
        , barreColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Barre where
    emitXml (Barre a b) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) b])
        []
mkBarre :: StartStop -> Barre
mkBarre a = Barre a Nothing

-- bass: Complex
data Bass = 
      Bass {
          bassBassStep :: BassStep
        , bassBassAlter :: (Maybe BassAlter)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bass where
    emitXml (Bass a b) =
      XContent XEmpty
        []
        ([XElement (QN "bass-step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "bass-alter" Nothing).emitXml) b])
mkBass :: BassStep -> Bass
mkBass a = Bass a Nothing

-- bass-alter: Complex
data BassAlter = 
      BassAlter {
          bassAlterSemitones :: Semitones
        , bassAlterLocation :: (Maybe LeftRight)
        , bassAlterPrintObject :: (Maybe YesNo)
        , bassAlterDefaultX :: (Maybe Tenths)
        , bassAlterDefaultY :: (Maybe Tenths)
        , bassAlterRelativeX :: (Maybe Tenths)
        , bassAlterRelativeY :: (Maybe Tenths)
        , bassAlterFontFamily :: (Maybe CommaSeparatedText)
        , bassAlterFontStyle :: (Maybe FontStyle)
        , bassAlterFontSize :: (Maybe FontSize)
        , bassAlterFontWeight :: (Maybe FontWeight)
        , bassAlterColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BassAlter where
    emitXml (BassAlter a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l])
        []
mkBassAlter :: Semitones -> BassAlter
mkBassAlter a = BassAlter a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- bass-step: Complex
data BassStep = 
      BassStep {
          bassStepStep :: Step
        , bassStepText :: (Maybe Token)
        , bassStepDefaultX :: (Maybe Tenths)
        , bassStepDefaultY :: (Maybe Tenths)
        , bassStepRelativeX :: (Maybe Tenths)
        , bassStepRelativeY :: (Maybe Tenths)
        , bassStepFontFamily :: (Maybe CommaSeparatedText)
        , bassStepFontStyle :: (Maybe FontStyle)
        , bassStepFontSize :: (Maybe FontSize)
        , bassStepFontWeight :: (Maybe FontWeight)
        , bassStepColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BassStep where
    emitXml (BassStep a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkBassStep :: Step -> BassStep
mkBassStep a = BassStep a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- beam: Complex
data Beam = 
      Beam {
          beamBeamValue :: BeamValue
        , beamNumber :: (Maybe BeamLevel)
        , beamRepeater :: (Maybe YesNo)
        , beamFan :: (Maybe Fan)
        , beamColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Beam where
    emitXml (Beam a b c d e) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "repeater" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "fan" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) e])
        []
mkBeam :: BeamValue -> Beam
mkBeam a = Beam a Nothing Nothing Nothing Nothing

-- beat-repeat: Complex
data BeatRepeat = 
      BeatRepeat {
          beatRepeatType :: StartStop
        , beatRepeatSlashes :: (Maybe PositiveInteger)
        , beatRepeatUseDots :: (Maybe YesNo)
        , beatRepeatSlash :: (Maybe Slash)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BeatRepeat where
    emitXml (BeatRepeat a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "slashes" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "use-dots" Nothing).emitXml) c])
        ([emitXml d])
mkBeatRepeat :: StartStop -> BeatRepeat
mkBeatRepeat a = BeatRepeat a Nothing Nothing Nothing

-- bend: Complex
data Bend = 
      Bend {
          bendDefaultX :: (Maybe Tenths)
        , bendDefaultY :: (Maybe Tenths)
        , bendRelativeX :: (Maybe Tenths)
        , bendRelativeY :: (Maybe Tenths)
        , bendFontFamily :: (Maybe CommaSeparatedText)
        , bendFontStyle :: (Maybe FontStyle)
        , bendFontSize :: (Maybe FontSize)
        , bendFontWeight :: (Maybe FontWeight)
        , bendColor :: (Maybe Color)
        , bendAccelerate :: (Maybe YesNo)
        , bendBeats :: (Maybe TrillBeats)
        , bendFirstBeat :: (Maybe Percent)
        , bendLastBeat :: (Maybe Percent)
        , bendBendAlter :: Semitones
        , bendBend :: (Maybe ChxBend)
        , bendWithBar :: (Maybe PlacementText)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bend where
    emitXml (Bend a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "first-beat" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) m])
        ([XElement (QN "bend-alter" Nothing) (emitXml n)]++[emitXml o]++[maybe XEmpty (XElement (QN "with-bar" Nothing).emitXml) p])
mkBend :: Semitones -> Bend
mkBend n = Bend Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing n Nothing Nothing

-- bookmark: Complex
data Bookmark = 
      Bookmark {
          bookmarkId :: ID
        , bookmarkName :: (Maybe Token)
        , bookmarkElement :: (Maybe NMTOKEN)
        , bookmarkPosition :: (Maybe PositiveInteger)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bookmark where
    emitXml (Bookmark a b c d) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "element" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "position" Nothing).emitXml) d])
        []
mkBookmark :: ID -> Bookmark
mkBookmark a = Bookmark a Nothing Nothing Nothing

-- bracket: Complex
data Bracket = 
      Bracket {
          bracketType :: StartStop
        , bracketNumber :: (Maybe NumberLevel)
        , bracketLineEnd :: LineEnd
        , bracketEndLength :: (Maybe Tenths)
        , bracketLineType :: (Maybe LineType)
        , bracketDefaultX :: (Maybe Tenths)
        , bracketDefaultY :: (Maybe Tenths)
        , bracketRelativeX :: (Maybe Tenths)
        , bracketRelativeY :: (Maybe Tenths)
        , bracketColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Bracket where
    emitXml (Bracket a b c d e f g h i j) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[XAttr (QN "line-end" Nothing) (emitXml c)]++[maybe XEmpty (XAttr (QN "end-length" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
mkBracket :: StartStop -> LineEnd -> Bracket
mkBracket a c = Bracket a Nothing c Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- cancel: Complex
data Cancel = 
      Cancel {
          cancelFifths :: Fifths
        , cancelLocation :: (Maybe LeftRight)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Cancel where
    emitXml (Cancel a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) b])
        []
mkCancel :: Fifths -> Cancel
mkCancel a = Cancel a Nothing

-- clef: Complex
data Clef = 
      Clef {
          clefNumber :: (Maybe StaffNumber)
        , clefAdditional :: (Maybe YesNo)
        , clefSize :: (Maybe SymbolSize)
        , clefDefaultX :: (Maybe Tenths)
        , clefDefaultY :: (Maybe Tenths)
        , clefRelativeX :: (Maybe Tenths)
        , clefRelativeY :: (Maybe Tenths)
        , clefFontFamily :: (Maybe CommaSeparatedText)
        , clefFontStyle :: (Maybe FontStyle)
        , clefFontSize :: (Maybe FontSize)
        , clefFontWeight :: (Maybe FontWeight)
        , clefColor :: (Maybe Color)
        , clefPrintObject :: (Maybe YesNo)
        , clefSign :: ClefSign
        , clefLine :: (Maybe StaffLine)
        , clefClefOctaveChange :: (Maybe Int)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Clef where
    emitXml (Clef a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "additional" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) m])
        ([XElement (QN "sign" Nothing) (emitXml n)]++[maybe XEmpty (XElement (QN "line" Nothing).emitXml) o]++[maybe XEmpty (XElement (QN "clef-octave-change" Nothing).emitXml) p])
mkClef :: ClefSign -> Clef
mkClef n = Clef Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing n Nothing Nothing

-- credit: Complex
data Credit = 
      Credit {
          creditPage :: (Maybe PositiveInteger)
        , creditLink :: [Link]
        , creditBookmark :: [Bookmark]
        , creditCredit :: ChxCredit
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Credit where
    emitXml (Credit a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "page" Nothing).emitXml) a])
        (map (XElement (QN "link" Nothing).emitXml) b++map (XElement (QN "bookmark" Nothing).emitXml) c++[emitXml d])
mkCredit :: ChxCredit -> Credit
mkCredit d = Credit Nothing [] [] d

-- dashes: Complex
data Dashes = 
      Dashes {
          dashesType :: StartStop
        , dashesNumber :: (Maybe NumberLevel)
        , dashesDefaultX :: (Maybe Tenths)
        , dashesDefaultY :: (Maybe Tenths)
        , dashesRelativeX :: (Maybe Tenths)
        , dashesRelativeY :: (Maybe Tenths)
        , dashesColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Dashes where
    emitXml (Dashes a b c d e f g) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) g])
        []
mkDashes :: StartStop -> Dashes
mkDashes a = Dashes a Nothing Nothing Nothing Nothing Nothing Nothing

-- defaults: Complex
data Defaults = 
      Defaults {
          defaultsScaling :: (Maybe Scaling)
        , defaultsLayout :: Layout
        , defaultsAppearance :: (Maybe Appearance)
        , defaultsMusicFont :: (Maybe EmptyFont)
        , defaultsWordFont :: (Maybe EmptyFont)
        , defaultsLyricFont :: [LyricFont]
        , defaultsLyricLanguage :: [LyricLanguage]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Defaults where
    emitXml (Defaults a b c d e f g) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "scaling" Nothing).emitXml) a]++[emitXml b]++[maybe XEmpty (XElement (QN "appearance" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "music-font" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "word-font" Nothing).emitXml) e]++map (XElement (QN "lyric-font" Nothing).emitXml) f++map (XElement (QN "lyric-language" Nothing).emitXml) g)
mkDefaults :: Layout -> Defaults
mkDefaults b = Defaults Nothing b Nothing Nothing Nothing [] []

-- degree: Complex
data Degree = 
      Degree {
          degreePrintObject :: (Maybe YesNo)
        , degreeDegreeValue :: DegreeValue
        , degreeDegreeAlter :: DegreeAlter
        , degreeDegreeType :: DegreeType
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Degree where
    emitXml (Degree a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) a])
        ([XElement (QN "degree-value" Nothing) (emitXml b)]++[XElement (QN "degree-alter" Nothing) (emitXml c)]++[XElement (QN "degree-type" Nothing) (emitXml d)])
mkDegree :: DegreeValue -> DegreeAlter -> DegreeType -> Degree
mkDegree b c d = Degree Nothing b c d

-- degree-alter: Complex
data DegreeAlter = 
      DegreeAlter {
          degreeAlterSemitones :: Semitones
        , degreeAlterPlusMinus :: (Maybe YesNo)
        , degreeAlterDefaultX :: (Maybe Tenths)
        , degreeAlterDefaultY :: (Maybe Tenths)
        , degreeAlterRelativeX :: (Maybe Tenths)
        , degreeAlterRelativeY :: (Maybe Tenths)
        , degreeAlterFontFamily :: (Maybe CommaSeparatedText)
        , degreeAlterFontStyle :: (Maybe FontStyle)
        , degreeAlterFontSize :: (Maybe FontSize)
        , degreeAlterFontWeight :: (Maybe FontWeight)
        , degreeAlterColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DegreeAlter where
    emitXml (DegreeAlter a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "plus-minus" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkDegreeAlter :: Semitones -> DegreeAlter
mkDegreeAlter a = DegreeAlter a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- degree-type: Complex
data DegreeType = 
      DegreeType {
          degreeTypeDegreeTypeValue :: DegreeTypeValue
        , degreeTypeText :: (Maybe Token)
        , degreeTypeDefaultX :: (Maybe Tenths)
        , degreeTypeDefaultY :: (Maybe Tenths)
        , degreeTypeRelativeX :: (Maybe Tenths)
        , degreeTypeRelativeY :: (Maybe Tenths)
        , degreeTypeFontFamily :: (Maybe CommaSeparatedText)
        , degreeTypeFontStyle :: (Maybe FontStyle)
        , degreeTypeFontSize :: (Maybe FontSize)
        , degreeTypeFontWeight :: (Maybe FontWeight)
        , degreeTypeColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DegreeType where
    emitXml (DegreeType a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkDegreeType :: DegreeTypeValue -> DegreeType
mkDegreeType a = DegreeType a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- degree-value: Complex
data DegreeValue = 
      DegreeValue {
          degreeValuePositiveInteger :: PositiveInteger
        , degreeValueText :: (Maybe Token)
        , degreeValueDefaultX :: (Maybe Tenths)
        , degreeValueDefaultY :: (Maybe Tenths)
        , degreeValueRelativeX :: (Maybe Tenths)
        , degreeValueRelativeY :: (Maybe Tenths)
        , degreeValueFontFamily :: (Maybe CommaSeparatedText)
        , degreeValueFontStyle :: (Maybe FontStyle)
        , degreeValueFontSize :: (Maybe FontSize)
        , degreeValueFontWeight :: (Maybe FontWeight)
        , degreeValueColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DegreeValue where
    emitXml (DegreeValue a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkDegreeValue :: PositiveInteger -> DegreeValue
mkDegreeValue a = DegreeValue a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- direction: Complex
data Direction = 
      Direction {
          directionPlacement :: (Maybe AboveBelow)
        , directionDirective :: (Maybe YesNo)
        , directionDirectionType :: [DirectionType]
        , directionOffset :: (Maybe Offset)
        , directionEditorialVoiceDirection :: EditorialVoiceDirection
        , directionStaff :: (Maybe Staff)
        , directionSound :: (Maybe Sound)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Direction where
    emitXml (Direction a b c d e f g) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "directive" Nothing).emitXml) b])
        (map (XElement (QN "direction-type" Nothing).emitXml) c++[maybe XEmpty (XElement (QN "offset" Nothing).emitXml) d]++[emitXml e]++[emitXml f]++[maybe XEmpty (XElement (QN "sound" Nothing).emitXml) g])
mkDirection :: EditorialVoiceDirection -> Direction
mkDirection e = Direction Nothing Nothing [] Nothing e Nothing Nothing

-- direction-type: Complex
data DirectionType = 
      DirectionType {
          directionTypeDirectionType :: ChxDirectionType
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DirectionType where
    emitXml (DirectionType a) =
      XReps [emitXml a]
mkDirectionType :: ChxDirectionType -> DirectionType
mkDirectionType a = DirectionType a

-- directive: Complex
data Directive = 
      Directive {
          directiveString :: String
        , directiveLang :: (Maybe Lang)
        , directiveDefaultX :: (Maybe Tenths)
        , directiveDefaultY :: (Maybe Tenths)
        , directiveRelativeX :: (Maybe Tenths)
        , directiveRelativeY :: (Maybe Tenths)
        , directiveFontFamily :: (Maybe CommaSeparatedText)
        , directiveFontStyle :: (Maybe FontStyle)
        , directiveFontSize :: (Maybe FontSize)
        , directiveFontWeight :: (Maybe FontWeight)
        , directiveColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Directive where
    emitXml (Directive a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkDirective :: String -> Directive
mkDirective a = Directive a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- display-step-octave: Complex
data DisplayStepOctave = 
      DisplayStepOctave {
          displayStepOctaveDisplayStepOctave :: (Maybe SeqDisplayStepOctave)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml DisplayStepOctave where
    emitXml (DisplayStepOctave a) =
      XReps [emitXml a]
mkDisplayStepOctave :: DisplayStepOctave
mkDisplayStepOctave = DisplayStepOctave Nothing

-- dynamics: Complex
data Dynamics = 
      Dynamics {
          dynamicsDefaultX :: (Maybe Tenths)
        , dynamicsDefaultY :: (Maybe Tenths)
        , dynamicsRelativeX :: (Maybe Tenths)
        , dynamicsRelativeY :: (Maybe Tenths)
        , dynamicsFontFamily :: (Maybe CommaSeparatedText)
        , dynamicsFontStyle :: (Maybe FontStyle)
        , dynamicsFontSize :: (Maybe FontSize)
        , dynamicsFontWeight :: (Maybe FontWeight)
        , dynamicsColor :: (Maybe Color)
        , dynamicsPlacement :: (Maybe AboveBelow)
        , dynamicsDynamics :: [ChxDynamics]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Dynamics where
    emitXml (Dynamics a b c d e f g h i j k) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) j])
        ([emitXml k])
mkDynamics :: Dynamics
mkDynamics = Dynamics Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

-- elision: Complex
data Elision = 
      Elision {
          elisionString :: String
        , elisionFontFamily :: (Maybe CommaSeparatedText)
        , elisionFontStyle :: (Maybe FontStyle)
        , elisionFontSize :: (Maybe FontSize)
        , elisionFontWeight :: (Maybe FontWeight)
        , elisionColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Elision where
    emitXml (Elision a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
mkElision :: String -> Elision
mkElision a = Elision a Nothing Nothing Nothing Nothing Nothing

-- empty: Complex
data Empty = 
      Empty
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Empty where
    emitXml (Empty) =
      XReps []
mkEmpty :: Empty
mkEmpty = Empty 

-- empty-font: Complex
data EmptyFont = 
      EmptyFont {
          emptyFontFontFamily :: (Maybe CommaSeparatedText)
        , emptyFontFontStyle :: (Maybe FontStyle)
        , emptyFontFontSize :: (Maybe FontSize)
        , emptyFontFontWeight :: (Maybe FontWeight)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyFont where
    emitXml (EmptyFont a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) d])
        []
mkEmptyFont :: EmptyFont
mkEmptyFont = EmptyFont Nothing Nothing Nothing Nothing

-- empty-line: Complex
data EmptyLine = 
      EmptyLine {
          emptyLineLineShape :: (Maybe LineShape)
        , emptyLineLineType :: (Maybe LineType)
        , emptyLineDefaultX :: (Maybe Tenths)
        , emptyLineDefaultY :: (Maybe Tenths)
        , emptyLineRelativeX :: (Maybe Tenths)
        , emptyLineRelativeY :: (Maybe Tenths)
        , emptyLineFontFamily :: (Maybe CommaSeparatedText)
        , emptyLineFontStyle :: (Maybe FontStyle)
        , emptyLineFontSize :: (Maybe FontSize)
        , emptyLineFontWeight :: (Maybe FontWeight)
        , emptyLineColor :: (Maybe Color)
        , emptyLinePlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyLine where
    emitXml (EmptyLine a b c d e f g h i j k l) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "line-shape" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) l])
        []
mkEmptyLine :: EmptyLine
mkEmptyLine = EmptyLine Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- empty-placement: Complex
data EmptyPlacement = 
      EmptyPlacement {
          emptyPlacementDefaultX :: (Maybe Tenths)
        , emptyPlacementDefaultY :: (Maybe Tenths)
        , emptyPlacementRelativeX :: (Maybe Tenths)
        , emptyPlacementRelativeY :: (Maybe Tenths)
        , emptyPlacementFontFamily :: (Maybe CommaSeparatedText)
        , emptyPlacementFontStyle :: (Maybe FontStyle)
        , emptyPlacementFontSize :: (Maybe FontSize)
        , emptyPlacementFontWeight :: (Maybe FontWeight)
        , emptyPlacementColor :: (Maybe Color)
        , emptyPlacementPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyPlacement where
    emitXml (EmptyPlacement a b c d e f g h i j) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) j])
        []
mkEmptyPlacement :: EmptyPlacement
mkEmptyPlacement = EmptyPlacement Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- empty-print-style: Complex
data EmptyPrintStyle = 
      EmptyPrintStyle {
          emptyPrintStyleDefaultX :: (Maybe Tenths)
        , emptyPrintStyleDefaultY :: (Maybe Tenths)
        , emptyPrintStyleRelativeX :: (Maybe Tenths)
        , emptyPrintStyleRelativeY :: (Maybe Tenths)
        , emptyPrintStyleFontFamily :: (Maybe CommaSeparatedText)
        , emptyPrintStyleFontStyle :: (Maybe FontStyle)
        , emptyPrintStyleFontSize :: (Maybe FontSize)
        , emptyPrintStyleFontWeight :: (Maybe FontWeight)
        , emptyPrintStyleColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyPrintStyle where
    emitXml (EmptyPrintStyle a b c d e f g h i) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        []
mkEmptyPrintStyle :: EmptyPrintStyle
mkEmptyPrintStyle = EmptyPrintStyle Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- empty-trill-sound: Complex
data EmptyTrillSound = 
      EmptyTrillSound {
          emptyTrillSoundDefaultX :: (Maybe Tenths)
        , emptyTrillSoundDefaultY :: (Maybe Tenths)
        , emptyTrillSoundRelativeX :: (Maybe Tenths)
        , emptyTrillSoundRelativeY :: (Maybe Tenths)
        , emptyTrillSoundFontFamily :: (Maybe CommaSeparatedText)
        , emptyTrillSoundFontStyle :: (Maybe FontStyle)
        , emptyTrillSoundFontSize :: (Maybe FontSize)
        , emptyTrillSoundFontWeight :: (Maybe FontWeight)
        , emptyTrillSoundColor :: (Maybe Color)
        , emptyTrillSoundPlacement :: (Maybe AboveBelow)
        , emptyTrillSoundStartNote :: (Maybe StartNote)
        , emptyTrillSoundTrillStep :: (Maybe TrillStep)
        , emptyTrillSoundTwoNoteTurn :: (Maybe TwoNoteTurn)
        , emptyTrillSoundAccelerate :: (Maybe YesNo)
        , emptyTrillSoundBeats :: (Maybe TrillBeats)
        , emptyTrillSoundSecondBeat :: (Maybe Percent)
        , emptyTrillSoundLastBeat :: (Maybe Percent)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml EmptyTrillSound where
    emitXml (EmptyTrillSound a b c d e f g h i j k l m n o p q) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "start-note" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "trill-step" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "two-note-turn" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "second-beat" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) q])
        []
mkEmptyTrillSound :: EmptyTrillSound
mkEmptyTrillSound = EmptyTrillSound Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- encoding: Complex
data Encoding = 
      Encoding {
          encodingEncoding :: [ChxEncoding]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Encoding where
    emitXml (Encoding a) =
      XReps [emitXml a]
mkEncoding :: Encoding
mkEncoding = Encoding []

-- ending: Complex
data Ending = 
      Ending {
          endingString :: String
        , cmpendingNumber :: EndingNumber
        , endingType :: StartStopDiscontinue
        , endingEndLength :: (Maybe Tenths)
        , endingTextX :: (Maybe Tenths)
        , endingTextY :: (Maybe Tenths)
        , endingPrintObject :: (Maybe YesNo)
        , endingDefaultX :: (Maybe Tenths)
        , endingDefaultY :: (Maybe Tenths)
        , endingRelativeX :: (Maybe Tenths)
        , endingRelativeY :: (Maybe Tenths)
        , endingFontFamily :: (Maybe CommaSeparatedText)
        , endingFontStyle :: (Maybe FontStyle)
        , endingFontSize :: (Maybe FontSize)
        , endingFontWeight :: (Maybe FontWeight)
        , endingColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Ending where
    emitXml (Ending a b c d e f g h i j k l m n o p) =
      XContent (emitXml a)
        ([XAttr (QN "number" Nothing) (emitXml b)]++[XAttr (QN "type" Nothing) (emitXml c)]++[maybe XEmpty (XAttr (QN "end-length" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "text-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "text-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) p])
        []
mkEnding :: String -> EndingNumber -> StartStopDiscontinue -> Ending
mkEnding a b c = Ending a b c Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- extend: Complex
data Extend = 
      Extend {
          extendFontFamily :: (Maybe CommaSeparatedText)
        , extendFontStyle :: (Maybe FontStyle)
        , extendFontSize :: (Maybe FontSize)
        , extendFontWeight :: (Maybe FontWeight)
        , extendColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Extend where
    emitXml (Extend a b c d e) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) e])
        []
mkExtend :: Extend
mkExtend = Extend Nothing Nothing Nothing Nothing Nothing

-- feature: Complex
data Feature = 
      Feature {
          featureString :: String
        , featureType :: (Maybe Token)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Feature where
    emitXml (Feature a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b])
        []
mkFeature :: String -> Feature
mkFeature a = Feature a Nothing

-- fermata: Complex
data Fermata = 
      Fermata {
          fermataFermataShape :: FermataShape
        , fermataType :: (Maybe UprightInverted)
        , fermataDefaultX :: (Maybe Tenths)
        , fermataDefaultY :: (Maybe Tenths)
        , fermataRelativeX :: (Maybe Tenths)
        , fermataRelativeY :: (Maybe Tenths)
        , fermataFontFamily :: (Maybe CommaSeparatedText)
        , fermataFontStyle :: (Maybe FontStyle)
        , fermataFontSize :: (Maybe FontSize)
        , fermataFontWeight :: (Maybe FontWeight)
        , fermataColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Fermata where
    emitXml (Fermata a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkFermata :: FermataShape -> Fermata
mkFermata a = Fermata a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- figure: Complex
data Figure = 
      Figure {
          figurePrefix :: (Maybe StyleText)
        , figureFigureNumber :: (Maybe StyleText)
        , figureSuffix :: (Maybe StyleText)
        , figureExtend :: (Maybe Extend)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Figure where
    emitXml (Figure a b c d) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "prefix" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "figure-number" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "suffix" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "extend" Nothing).emitXml) d])
mkFigure :: Figure
mkFigure = Figure Nothing Nothing Nothing Nothing

-- figured-bass: Complex
data FiguredBass = 
      FiguredBass {
          figuredBassParentheses :: (Maybe YesNo)
        , figuredBassDefaultX :: (Maybe Tenths)
        , figuredBassDefaultY :: (Maybe Tenths)
        , figuredBassRelativeX :: (Maybe Tenths)
        , figuredBassRelativeY :: (Maybe Tenths)
        , figuredBassFontFamily :: (Maybe CommaSeparatedText)
        , figuredBassFontStyle :: (Maybe FontStyle)
        , figuredBassFontSize :: (Maybe FontSize)
        , figuredBassFontWeight :: (Maybe FontWeight)
        , figuredBassColor :: (Maybe Color)
        , figuredBassPrintDot :: (Maybe YesNo)
        , figuredBassPrintLyric :: (Maybe YesNo)
        , figuredBassPrintObject :: (Maybe YesNo)
        , figuredBassPrintSpacing :: (Maybe YesNo)
        , figuredBassFigure :: [Figure]
        , figuredBassDuration :: (Maybe Duration)
        , figuredBassEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FiguredBass where
    emitXml (FiguredBass a b c d e f g h i j k l m n o p q) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "print-dot" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "print-lyric" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "print-spacing" Nothing).emitXml) n])
        (map (XElement (QN "figure" Nothing).emitXml) o++[emitXml p]++[emitXml q])
mkFiguredBass :: Editorial -> FiguredBass
mkFiguredBass q = FiguredBass Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing q

-- fingering: Complex
data Fingering = 
      Fingering {
          fingeringString :: String
        , fingeringSubstitution :: (Maybe YesNo)
        , fingeringAlternate :: (Maybe YesNo)
        , fingeringDefaultX :: (Maybe Tenths)
        , fingeringDefaultY :: (Maybe Tenths)
        , fingeringRelativeX :: (Maybe Tenths)
        , fingeringRelativeY :: (Maybe Tenths)
        , fingeringFontFamily :: (Maybe CommaSeparatedText)
        , fingeringFontStyle :: (Maybe FontStyle)
        , fingeringFontSize :: (Maybe FontSize)
        , fingeringFontWeight :: (Maybe FontWeight)
        , fingeringColor :: (Maybe Color)
        , fingeringPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Fingering where
    emitXml (Fingering a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "substitution" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "alternate" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) m])
        []
mkFingering :: String -> Fingering
mkFingering a = Fingering a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- first-fret: Complex
data FirstFret = 
      FirstFret {
          firstFretPositiveInteger :: PositiveInteger
        , firstFretText :: (Maybe Token)
        , firstFretLocation :: (Maybe LeftRight)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FirstFret where
    emitXml (FirstFret a b c) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "location" Nothing).emitXml) c])
        []
mkFirstFret :: PositiveInteger -> FirstFret
mkFirstFret a = FirstFret a Nothing Nothing

-- formatted-text: Complex
data FormattedText = 
      FormattedText {
          formattedTextString :: String
        , formattedTextLang :: (Maybe Lang)
        , formattedTextEnclosure :: (Maybe Enclosure)
        , formattedTextJustify :: (Maybe LeftCenterRight)
        , formattedTextHalign :: (Maybe LeftCenterRight)
        , formattedTextValign :: (Maybe Valign)
        , formattedTextDefaultX :: (Maybe Tenths)
        , formattedTextDefaultY :: (Maybe Tenths)
        , formattedTextRelativeX :: (Maybe Tenths)
        , formattedTextRelativeY :: (Maybe Tenths)
        , formattedTextFontFamily :: (Maybe CommaSeparatedText)
        , formattedTextFontStyle :: (Maybe FontStyle)
        , formattedTextFontSize :: (Maybe FontSize)
        , formattedTextFontWeight :: (Maybe FontWeight)
        , formattedTextColor :: (Maybe Color)
        , formattedTextUnderline :: (Maybe NumberOfLines)
        , formattedTextOverline :: (Maybe NumberOfLines)
        , formattedTextLineThrough :: (Maybe NumberOfLines)
        , formattedTextRotation :: (Maybe RotationDegrees)
        , formattedTextLetterSpacing :: (Maybe NumberOrNormal)
        , formattedTextLineHeight :: (Maybe NumberOrNormal)
        , formattedTextDir :: (Maybe TextDirection)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FormattedText where
    emitXml (FormattedText a b c d e f g h i j k l m n o p q r s t u v) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "enclosure" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) q]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) r]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) s]++[maybe XEmpty (XAttr (QN "letter-spacing" Nothing).emitXml) t]++[maybe XEmpty (XAttr (QN "line-height" Nothing).emitXml) u]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) v])
        []
mkFormattedText :: String -> FormattedText
mkFormattedText a = FormattedText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- forward: Complex
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
mkForward :: Duration -> EditorialVoice -> Forward
mkForward a b = Forward a b Nothing

-- frame: Complex
data Frame = 
      Frame {
          frameHeight :: (Maybe Tenths)
        , frameWidth :: (Maybe Tenths)
        , frameDefaultX :: (Maybe Tenths)
        , frameDefaultY :: (Maybe Tenths)
        , frameRelativeX :: (Maybe Tenths)
        , frameRelativeY :: (Maybe Tenths)
        , frameColor :: (Maybe Color)
        , frameHalign :: (Maybe LeftCenterRight)
        , frameValign :: (Maybe Valign)
        , frameFrameStrings :: PositiveInteger
        , frameFrameFrets :: PositiveInteger
        , frameFirstFret :: (Maybe FirstFret)
        , frameFrameNote :: [FrameNote]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Frame where
    emitXml (Frame a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "height" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "width" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) i])
        ([XElement (QN "frame-strings" Nothing) (emitXml j)]++[XElement (QN "frame-frets" Nothing) (emitXml k)]++[maybe XEmpty (XElement (QN "first-fret" Nothing).emitXml) l]++map (XElement (QN "frame-note" Nothing).emitXml) m)
mkFrame :: PositiveInteger -> PositiveInteger -> Frame
mkFrame j k = Frame Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing j k Nothing []

-- frame-note: Complex
data FrameNote = 
      FrameNote {
          frameNoteString :: CmpString
        , frameNoteFret :: Fret
        , frameNoteFingering :: (Maybe Fingering)
        , frameNoteBarre :: (Maybe Barre)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml FrameNote where
    emitXml (FrameNote a b c d) =
      XContent XEmpty
        []
        ([XElement (QN "string" Nothing) (emitXml a)]++[XElement (QN "fret" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "fingering" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "barre" Nothing).emitXml) d])
mkFrameNote :: CmpString -> Fret -> FrameNote
mkFrameNote a b = FrameNote a b Nothing Nothing

-- fret: Complex
data Fret = 
      Fret {
          fretNonNegativeInteger :: NonNegativeInteger
        , fretFontFamily :: (Maybe CommaSeparatedText)
        , fretFontStyle :: (Maybe FontStyle)
        , fretFontSize :: (Maybe FontSize)
        , fretFontWeight :: (Maybe FontWeight)
        , fretColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Fret where
    emitXml (Fret a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
mkFret :: NonNegativeInteger -> Fret
mkFret a = Fret a Nothing Nothing Nothing Nothing Nothing

-- glissando: Complex
data Glissando = 
      Glissando {
          glissandoString :: String
        , glissandoType :: StartStop
        , glissandoNumber :: (Maybe NumberLevel)
        , glissandoLineType :: (Maybe LineType)
        , glissandoDefaultX :: (Maybe Tenths)
        , glissandoDefaultY :: (Maybe Tenths)
        , glissandoRelativeX :: (Maybe Tenths)
        , glissandoRelativeY :: (Maybe Tenths)
        , glissandoFontFamily :: (Maybe CommaSeparatedText)
        , glissandoFontStyle :: (Maybe FontStyle)
        , glissandoFontSize :: (Maybe FontSize)
        , glissandoFontWeight :: (Maybe FontWeight)
        , glissandoColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Glissando where
    emitXml (Glissando a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) m])
        []
mkGlissando :: String -> StartStop -> Glissando
mkGlissando a b = Glissando a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- grace: Complex
data Grace = 
      Grace {
          graceStealTimePrevious :: (Maybe Percent)
        , graceStealTimeFollowing :: (Maybe Percent)
        , graceMakeTime :: (Maybe Divisions)
        , graceSlash :: (Maybe YesNo)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Grace where
    emitXml (Grace a b c d) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "steal-time-previous" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "steal-time-following" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "make-time" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "slash" Nothing).emitXml) d])
        []
mkGrace :: Grace
mkGrace = Grace Nothing Nothing Nothing Nothing

-- group-barline: Complex
data GroupBarline = 
      GroupBarline {
          groupBarlineGroupBarlineValue :: GroupBarlineValue
        , groupBarlineColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GroupBarline where
    emitXml (GroupBarline a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "color" Nothing).emitXml) b])
        []
mkGroupBarline :: GroupBarlineValue -> GroupBarline
mkGroupBarline a = GroupBarline a Nothing

-- group-name: Complex
data GroupName = 
      GroupName {
          groupNameString :: String
        , groupNameDefaultX :: (Maybe Tenths)
        , groupNameDefaultY :: (Maybe Tenths)
        , groupNameRelativeX :: (Maybe Tenths)
        , groupNameRelativeY :: (Maybe Tenths)
        , groupNameFontFamily :: (Maybe CommaSeparatedText)
        , groupNameFontStyle :: (Maybe FontStyle)
        , groupNameFontSize :: (Maybe FontSize)
        , groupNameFontWeight :: (Maybe FontWeight)
        , groupNameColor :: (Maybe Color)
        , groupNameJustify :: (Maybe LeftCenterRight)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GroupName where
    emitXml (GroupName a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) k])
        []
mkGroupName :: String -> GroupName
mkGroupName a = GroupName a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- group-symbol: Complex
data GroupSymbol = 
      GroupSymbol {
          groupSymbolGroupSymbolValue :: GroupSymbolValue
        , groupSymbolDefaultX :: (Maybe Tenths)
        , groupSymbolDefaultY :: (Maybe Tenths)
        , groupSymbolRelativeX :: (Maybe Tenths)
        , groupSymbolRelativeY :: (Maybe Tenths)
        , groupSymbolColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GroupSymbol where
    emitXml (GroupSymbol a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
mkGroupSymbol :: GroupSymbolValue -> GroupSymbol
mkGroupSymbol a = GroupSymbol a Nothing Nothing Nothing Nothing Nothing

-- grouping: Complex
data Grouping = 
      Grouping {
          groupingType :: StartStopSingle
        , groupingNumber :: (Maybe Token)
        , groupingMemberOf :: (Maybe Token)
        , groupingFeature :: [Feature]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Grouping where
    emitXml (Grouping a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "member-of" Nothing).emitXml) c])
        (map (XElement (QN "feature" Nothing).emitXml) d)
mkGrouping :: StartStopSingle -> Grouping
mkGrouping a = Grouping a Nothing Nothing []

-- hammer-on-pull-off: Complex
data HammerOnPullOff = 
      HammerOnPullOff {
          hammerOnPullOffString :: String
        , hammerOnPullOffType :: StartStop
        , hammerOnPullOffNumber :: (Maybe NumberLevel)
        , hammerOnPullOffDefaultX :: (Maybe Tenths)
        , hammerOnPullOffDefaultY :: (Maybe Tenths)
        , hammerOnPullOffRelativeX :: (Maybe Tenths)
        , hammerOnPullOffRelativeY :: (Maybe Tenths)
        , hammerOnPullOffFontFamily :: (Maybe CommaSeparatedText)
        , hammerOnPullOffFontStyle :: (Maybe FontStyle)
        , hammerOnPullOffFontSize :: (Maybe FontSize)
        , hammerOnPullOffFontWeight :: (Maybe FontWeight)
        , hammerOnPullOffColor :: (Maybe Color)
        , hammerOnPullOffPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HammerOnPullOff where
    emitXml (HammerOnPullOff a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) m])
        []
mkHammerOnPullOff :: String -> StartStop -> HammerOnPullOff
mkHammerOnPullOff a b = HammerOnPullOff a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- harmonic: Complex
data Harmonic = 
      Harmonic {
          harmonicPrintObject :: (Maybe YesNo)
        , harmonicDefaultX :: (Maybe Tenths)
        , harmonicDefaultY :: (Maybe Tenths)
        , harmonicRelativeX :: (Maybe Tenths)
        , harmonicRelativeY :: (Maybe Tenths)
        , harmonicFontFamily :: (Maybe CommaSeparatedText)
        , harmonicFontStyle :: (Maybe FontStyle)
        , harmonicFontSize :: (Maybe FontSize)
        , harmonicFontWeight :: (Maybe FontWeight)
        , harmonicColor :: (Maybe Color)
        , harmonicPlacement :: (Maybe AboveBelow)
        , harmonicHarmonic :: (Maybe ChxHarmonic)
        , harmonicHarmonic1 :: (Maybe ChxHarmonic1)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Harmonic where
    emitXml (Harmonic a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        ([emitXml l]++[emitXml m])
mkHarmonic :: Harmonic
mkHarmonic = Harmonic Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- harmony: Complex
data Harmony = 
      Harmony {
          harmonyType :: (Maybe HarmonyType)
        , harmonyPrintFrame :: (Maybe YesNo)
        , harmonyPrintObject :: (Maybe YesNo)
        , harmonyDefaultX :: (Maybe Tenths)
        , harmonyDefaultY :: (Maybe Tenths)
        , harmonyRelativeX :: (Maybe Tenths)
        , harmonyRelativeY :: (Maybe Tenths)
        , harmonyFontFamily :: (Maybe CommaSeparatedText)
        , harmonyFontStyle :: (Maybe FontStyle)
        , harmonyFontSize :: (Maybe FontSize)
        , harmonyFontWeight :: (Maybe FontWeight)
        , harmonyColor :: (Maybe Color)
        , harmonyPlacement :: (Maybe AboveBelow)
        , harmonyHarmonyChord :: [HarmonyChord]
        , harmonyFrame :: (Maybe Frame)
        , harmonyOffset :: (Maybe Offset)
        , harmonyEditorial :: Editorial
        , harmonyStaff :: (Maybe Staff)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Harmony where
    emitXml (Harmony a b c d e f g h i j k l m n o p q r) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "print-frame" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) m])
        ([emitXml n]++[maybe XEmpty (XElement (QN "frame" Nothing).emitXml) o]++[maybe XEmpty (XElement (QN "offset" Nothing).emitXml) p]++[emitXml q]++[emitXml r])
mkHarmony :: Editorial -> Harmony
mkHarmony q = Harmony Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing q Nothing

-- harp-pedals: Complex
data HarpPedals = 
      HarpPedals {
          harpPedalsDefaultX :: (Maybe Tenths)
        , harpPedalsDefaultY :: (Maybe Tenths)
        , harpPedalsRelativeX :: (Maybe Tenths)
        , harpPedalsRelativeY :: (Maybe Tenths)
        , harpPedalsFontFamily :: (Maybe CommaSeparatedText)
        , harpPedalsFontStyle :: (Maybe FontStyle)
        , harpPedalsFontSize :: (Maybe FontSize)
        , harpPedalsFontWeight :: (Maybe FontWeight)
        , harpPedalsColor :: (Maybe Color)
        , harpPedalsPedalTuning :: [PedalTuning]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HarpPedals where
    emitXml (HarpPedals a b c d e f g h i j) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        (map (XElement (QN "pedal-tuning" Nothing).emitXml) j)
mkHarpPedals :: HarpPedals
mkHarpPedals = HarpPedals Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

-- heel-toe: Complex
data HeelToe = 
      HeelToe {
          heelToeEmptyPlacement :: HeelToe
        , heelToeSubstitution :: (Maybe YesNo)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HeelToe where
    emitXml (HeelToe a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "substitution" Nothing).emitXml) b])
        ([emitXml a])
mkHeelToe :: HeelToe -> HeelToe
mkHeelToe a = HeelToe a Nothing

-- identification: Complex
data Identification = 
      Identification {
          identificationCreator :: [TypedText]
        , identificationRights :: [TypedText]
        , identificationEncoding :: (Maybe Encoding)
        , identificationSource :: (Maybe String)
        , identificationRelation :: [TypedText]
        , identificationMiscellaneous :: (Maybe Miscellaneous)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Identification where
    emitXml (Identification a b c d e f) =
      XContent XEmpty
        []
        (map (XElement (QN "creator" Nothing).emitXml) a++map (XElement (QN "rights" Nothing).emitXml) b++[maybe XEmpty (XElement (QN "encoding" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "source" Nothing).emitXml) d]++map (XElement (QN "relation" Nothing).emitXml) e++[maybe XEmpty (XElement (QN "miscellaneous" Nothing).emitXml) f])
mkIdentification :: Identification
mkIdentification = Identification [] [] Nothing Nothing [] Nothing

-- image: Complex
data Image = 
      Image {
          imageSource :: String
        , imageType :: Token
        , imageDefaultX :: (Maybe Tenths)
        , imageDefaultY :: (Maybe Tenths)
        , imageRelativeX :: (Maybe Tenths)
        , imageRelativeY :: (Maybe Tenths)
        , imageHalign :: (Maybe LeftCenterRight)
        , imageValign :: (Maybe ValignImage)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Image where
    emitXml (Image a b c d e f g h) =
      XContent XEmpty
        ([XAttr (QN "source" Nothing) (emitXml a)]++[XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) h])
        []
mkImage :: String -> Token -> Image
mkImage a b = Image a b Nothing Nothing Nothing Nothing Nothing Nothing

-- instrument: Complex
data Instrument = 
      Instrument {
          instrumentId :: IDREF
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Instrument where
    emitXml (Instrument a) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        []
mkInstrument :: IDREF -> Instrument
mkInstrument a = Instrument a

-- inversion: Complex
data Inversion = 
      Inversion {
          inversionNonNegativeInteger :: NonNegativeInteger
        , inversionDefaultX :: (Maybe Tenths)
        , inversionDefaultY :: (Maybe Tenths)
        , inversionRelativeX :: (Maybe Tenths)
        , inversionRelativeY :: (Maybe Tenths)
        , inversionFontFamily :: (Maybe CommaSeparatedText)
        , inversionFontStyle :: (Maybe FontStyle)
        , inversionFontSize :: (Maybe FontSize)
        , inversionFontWeight :: (Maybe FontWeight)
        , inversionColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Inversion where
    emitXml (Inversion a b c d e f g h i j) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
mkInversion :: NonNegativeInteger -> Inversion
mkInversion a = Inversion a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- key: Complex
data Key = 
      Key {
          keyNumber :: (Maybe StaffNumber)
        , keyDefaultX :: (Maybe Tenths)
        , keyDefaultY :: (Maybe Tenths)
        , keyRelativeX :: (Maybe Tenths)
        , keyRelativeY :: (Maybe Tenths)
        , keyFontFamily :: (Maybe CommaSeparatedText)
        , keyFontStyle :: (Maybe FontStyle)
        , keyFontSize :: (Maybe FontSize)
        , keyFontWeight :: (Maybe FontWeight)
        , keyColor :: (Maybe Color)
        , keyPrintObject :: (Maybe YesNo)
        , keyKey :: ChxKey
        , keyKeyOctave :: [KeyOctave]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Key where
    emitXml (Key a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) k])
        ([emitXml l]++map (XElement (QN "key-octave" Nothing).emitXml) m)
mkKey :: ChxKey -> Key
mkKey l = Key Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing l []

-- key-octave: Complex
data KeyOctave = 
      KeyOctave {
          keyOctaveOctave :: Octave
        , keyOctaveNumber :: PositiveInteger
        , keyOctaveCancel :: (Maybe YesNo)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml KeyOctave where
    emitXml (KeyOctave a b c) =
      XContent (emitXml a)
        ([XAttr (QN "number" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "cancel" Nothing).emitXml) c])
        []
mkKeyOctave :: Octave -> PositiveInteger -> KeyOctave
mkKeyOctave a b = KeyOctave a b Nothing

-- kind: Complex
data Kind = 
      Kind {
          kindKindValue :: KindValue
        , kindUseSymbols :: (Maybe YesNo)
        , kindText :: (Maybe Token)
        , kindStackDegrees :: (Maybe YesNo)
        , kindParenthesesDegrees :: (Maybe YesNo)
        , kindBracketDegrees :: (Maybe YesNo)
        , kindDefaultX :: (Maybe Tenths)
        , kindDefaultY :: (Maybe Tenths)
        , kindRelativeX :: (Maybe Tenths)
        , kindRelativeY :: (Maybe Tenths)
        , kindFontFamily :: (Maybe CommaSeparatedText)
        , kindFontStyle :: (Maybe FontStyle)
        , kindFontSize :: (Maybe FontSize)
        , kindFontWeight :: (Maybe FontWeight)
        , kindColor :: (Maybe Color)
        , kindHalign :: (Maybe LeftCenterRight)
        , kindValign :: (Maybe Valign)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Kind where
    emitXml (Kind a b c d e f g h i j k l m n o p q) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "use-symbols" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "text" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "stack-degrees" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "parentheses-degrees" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "bracket-degrees" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "halign" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "valign" Nothing).emitXml) q])
        []
mkKind :: KindValue -> Kind
mkKind a = Kind a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- level: Complex
data Level = 
      Level {
          levelString :: String
        , levelReference :: (Maybe YesNo)
        , levelParentheses :: (Maybe YesNo)
        , levelBracket :: (Maybe YesNo)
        , levelSize :: (Maybe SymbolSize)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Level where
    emitXml (Level a b c d e) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "reference" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) e])
        []
mkLevel :: String -> Level
mkLevel a = Level a Nothing Nothing Nothing Nothing

-- line-width: Complex
data LineWidth = 
      LineWidth {
          lineWidthTenths :: Tenths
        , cmplineWidthType :: LineWidthType
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LineWidth where
    emitXml (LineWidth a b) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)])
        []
mkLineWidth :: Tenths -> LineWidthType -> LineWidth
mkLineWidth a b = LineWidth a b

-- link: Complex
data Link = 
      Link {
          linkName :: (Maybe Token)
        , linkHref :: String
        , linkType :: (Maybe Type)
        , linkRole :: (Maybe Token)
        , linkTitle :: (Maybe Token)
        , linkShow :: (Maybe SmpShow)
        , linkActuate :: (Maybe Actuate)
        , linkElement :: (Maybe NMTOKEN)
        , linkPosition :: (Maybe PositiveInteger)
        , linkDefaultX :: (Maybe Tenths)
        , linkDefaultY :: (Maybe Tenths)
        , linkRelativeX :: (Maybe Tenths)
        , linkRelativeY :: (Maybe Tenths)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Link where
    emitXml (Link a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "name" Nothing).emitXml) a]++[XAttr (QN "href" (Just "xlink")) (emitXml b)]++[maybe XEmpty (XAttr (QN "type" (Just "xlink")).emitXml) c]++[maybe XEmpty (XAttr (QN "role" (Just "xlink")).emitXml) d]++[maybe XEmpty (XAttr (QN "title" (Just "xlink")).emitXml) e]++[maybe XEmpty (XAttr (QN "show" (Just "xlink")).emitXml) f]++[maybe XEmpty (XAttr (QN "actuate" (Just "xlink")).emitXml) g]++[maybe XEmpty (XAttr (QN "element" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "position" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) m])
        []
mkLink :: String -> Link
mkLink b = Link Nothing b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- lyric: Complex
data Lyric = 
      Lyric {
          lyricNumber :: (Maybe NMTOKEN)
        , lyricName :: (Maybe Token)
        , lyricJustify :: (Maybe LeftCenterRight)
        , lyricDefaultX :: (Maybe Tenths)
        , lyricDefaultY :: (Maybe Tenths)
        , lyricRelativeX :: (Maybe Tenths)
        , lyricRelativeY :: (Maybe Tenths)
        , lyricPlacement :: (Maybe AboveBelow)
        , lyricColor :: (Maybe Color)
        , lyricLyric :: ChxLyric
        , lyricEndLine :: (Maybe Empty)
        , lyricEndParagraph :: (Maybe Empty)
        , lyricEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Lyric where
    emitXml (Lyric a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) i])
        ([emitXml j]++[maybe XEmpty (XElement (QN "end-line" Nothing).emitXml) k]++[maybe XEmpty (XElement (QN "end-paragraph" Nothing).emitXml) l]++[emitXml m])
mkLyric :: ChxLyric -> Editorial -> Lyric
mkLyric j m = Lyric Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing j Nothing Nothing m

-- lyric-font: Complex
data LyricFont = 
      LyricFont {
          lyricFontNumber :: (Maybe NMTOKEN)
        , lyricFontName :: (Maybe Token)
        , lyricFontFontFamily :: (Maybe CommaSeparatedText)
        , lyricFontFontStyle :: (Maybe FontStyle)
        , lyricFontFontSize :: (Maybe FontSize)
        , lyricFontFontWeight :: (Maybe FontWeight)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LyricFont where
    emitXml (LyricFont a b c d e f) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) f])
        []
mkLyricFont :: LyricFont
mkLyricFont = LyricFont Nothing Nothing Nothing Nothing Nothing Nothing

-- lyric-language: Complex
data LyricLanguage = 
      LyricLanguage {
          lyricLanguageNumber :: (Maybe NMTOKEN)
        , lyricLanguageName :: (Maybe Token)
        , lyricLanguageLang :: Lang
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LyricLanguage where
    emitXml (LyricLanguage a b c) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "name" Nothing).emitXml) b]++[XAttr (QN "lang" (Just "xml")) (emitXml c)])
        []
mkLyricLanguage :: Lang -> LyricLanguage
mkLyricLanguage c = LyricLanguage Nothing Nothing c

-- measure: Complex
data Measure = 
      Measure {
          measureNumber :: Token
        , measureImplicit :: (Maybe YesNo)
        , measureNonControlling :: (Maybe YesNo)
        , measureWidth :: (Maybe Tenths)
        , measureMusicData :: MusicData
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Measure where
    emitXml (Measure a b c d e) =
      XContent XEmpty
        ([XAttr (QN "number" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "implicit" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "non-controlling" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "width" Nothing).emitXml) d])
        ([emitXml e])
mkMeasure :: Token -> MusicData -> Measure
mkMeasure a e = Measure a Nothing Nothing Nothing e

-- measure: Complex,1
data CmpMeasure = 
      CmpMeasure {
          cmpmeasureNumber :: Token
        , cmpmeasureImplicit :: (Maybe YesNo)
        , cmpmeasureNonControlling :: (Maybe YesNo)
        , cmpmeasureWidth :: (Maybe Tenths)
        , measurePart :: [Part]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpMeasure where
    emitXml (CmpMeasure a b c d e) =
      XContent XEmpty
        ([XAttr (QN "number" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "implicit" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "non-controlling" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "width" Nothing).emitXml) d])
        (map (XElement (QN "part" Nothing).emitXml) e)
mkCmpMeasure :: Token -> CmpMeasure
mkCmpMeasure a = CmpMeasure a Nothing Nothing Nothing []

-- measure-layout: Complex
data MeasureLayout = 
      MeasureLayout {
          measureLayoutMeasureDistance :: (Maybe Tenths)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureLayout where
    emitXml (MeasureLayout a) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "measure-distance" Nothing).emitXml) a])
mkMeasureLayout :: MeasureLayout
mkMeasureLayout = MeasureLayout Nothing

-- measure-numbering: Complex
data MeasureNumbering = 
      MeasureNumbering {
          measureNumberingMeasureNumberingValue :: MeasureNumberingValue
        , measureNumberingDefaultX :: (Maybe Tenths)
        , measureNumberingDefaultY :: (Maybe Tenths)
        , measureNumberingRelativeX :: (Maybe Tenths)
        , measureNumberingRelativeY :: (Maybe Tenths)
        , measureNumberingFontFamily :: (Maybe CommaSeparatedText)
        , measureNumberingFontStyle :: (Maybe FontStyle)
        , measureNumberingFontSize :: (Maybe FontSize)
        , measureNumberingFontWeight :: (Maybe FontWeight)
        , measureNumberingColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureNumbering where
    emitXml (MeasureNumbering a b c d e f g h i j) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
mkMeasureNumbering :: MeasureNumberingValue -> MeasureNumbering
mkMeasureNumbering a = MeasureNumbering a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- measure-repeat: Complex
data MeasureRepeat = 
      MeasureRepeat {
          measureRepeatPositiveIntegerOrEmpty :: PositiveIntegerOrEmpty
        , measureRepeatType :: StartStop
        , measureRepeatSlashes :: (Maybe PositiveInteger)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureRepeat where
    emitXml (MeasureRepeat a b c) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "slashes" Nothing).emitXml) c])
        []
mkMeasureRepeat :: PositiveIntegerOrEmpty -> StartStop -> MeasureRepeat
mkMeasureRepeat a b = MeasureRepeat a b Nothing

-- measure-style: Complex
data MeasureStyle = 
      MeasureStyle {
          measureStyleNumber :: (Maybe StaffNumber)
        , measureStyleFontFamily :: (Maybe CommaSeparatedText)
        , measureStyleFontStyle :: (Maybe FontStyle)
        , measureStyleFontSize :: (Maybe FontSize)
        , measureStyleFontWeight :: (Maybe FontWeight)
        , measureStyleColor :: (Maybe Color)
        , measureStyleMeasureStyle :: ChxMeasureStyle
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MeasureStyle where
    emitXml (MeasureStyle a b c d e f g) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        ([emitXml g])
mkMeasureStyle :: ChxMeasureStyle -> MeasureStyle
mkMeasureStyle g = MeasureStyle Nothing Nothing Nothing Nothing Nothing Nothing g

-- metronome: Complex
data Metronome = 
      Metronome {
          metronomeParentheses :: (Maybe YesNo)
        , metronomeDefaultX :: (Maybe Tenths)
        , metronomeDefaultY :: (Maybe Tenths)
        , metronomeRelativeX :: (Maybe Tenths)
        , metronomeRelativeY :: (Maybe Tenths)
        , metronomeFontFamily :: (Maybe CommaSeparatedText)
        , metronomeFontStyle :: (Maybe FontStyle)
        , metronomeFontSize :: (Maybe FontSize)
        , metronomeFontWeight :: (Maybe FontWeight)
        , metronomeColor :: (Maybe Color)
        , metronomeMetronome :: ChxMetronome
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Metronome where
    emitXml (Metronome a b c d e f g h i j k) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        ([emitXml k])
mkMetronome :: ChxMetronome -> Metronome
mkMetronome k = Metronome Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing k

-- metronome-beam: Complex
data MetronomeBeam = 
      MetronomeBeam {
          metronomeBeamBeamValue :: BeamValue
        , metronomeBeamNumber :: (Maybe BeamLevel)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MetronomeBeam where
    emitXml (MetronomeBeam a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b])
        []
mkMetronomeBeam :: BeamValue -> MetronomeBeam
mkMetronomeBeam a = MetronomeBeam a Nothing

-- metronome-note: Complex
data MetronomeNote = 
      MetronomeNote {
          metronomeNoteMetronomeType :: NoteTypeValue
        , metronomeNoteMetronomeDot :: [Empty]
        , metronomeNoteMetronomeBeam :: [MetronomeBeam]
        , metronomeNoteMetronomeTuplet :: (Maybe MetronomeTuplet)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MetronomeNote where
    emitXml (MetronomeNote a b c d) =
      XContent XEmpty
        []
        ([XElement (QN "metronome-type" Nothing) (emitXml a)]++map (XElement (QN "metronome-dot" Nothing).emitXml) b++map (XElement (QN "metronome-beam" Nothing).emitXml) c++[maybe XEmpty (XElement (QN "metronome-tuplet" Nothing).emitXml) d])
mkMetronomeNote :: NoteTypeValue -> MetronomeNote
mkMetronomeNote a = MetronomeNote a [] [] Nothing

-- metronome-tuplet: Complex
data MetronomeTuplet = 
      MetronomeTuplet {
          metronomeTupletTimeModification :: MetronomeTuplet
        , metronomeTupletType :: StartStop
        , metronomeTupletBracket :: (Maybe YesNo)
        , metronomeTupletShowNumber :: (Maybe ShowTuplet)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MetronomeTuplet where
    emitXml (MetronomeTuplet a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "show-number" Nothing).emitXml) d])
        ([emitXml a])
mkMetronomeTuplet :: MetronomeTuplet -> StartStop -> MetronomeTuplet
mkMetronomeTuplet a b = MetronomeTuplet a b Nothing Nothing

-- midi-device: Complex
data MidiDevice = 
      MidiDevice {
          midiDeviceString :: String
        , midiDevicePort :: (Maybe Midi16)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MidiDevice where
    emitXml (MidiDevice a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "port" Nothing).emitXml) b])
        []
mkMidiDevice :: String -> MidiDevice
mkMidiDevice a = MidiDevice a Nothing

-- midi-instrument: Complex
data MidiInstrument = 
      MidiInstrument {
          midiInstrumentId :: IDREF
        , midiInstrumentMidiChannel :: (Maybe Midi16)
        , midiInstrumentMidiName :: (Maybe String)
        , midiInstrumentMidiBank :: (Maybe Midi16384)
        , midiInstrumentMidiProgram :: (Maybe Midi128)
        , midiInstrumentMidiUnpitched :: (Maybe Midi128)
        , midiInstrumentVolume :: (Maybe Percent)
        , midiInstrumentPan :: (Maybe RotationDegrees)
        , midiInstrumentElevation :: (Maybe RotationDegrees)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MidiInstrument where
    emitXml (MidiInstrument a b c d e f g h i) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([maybe XEmpty (XElement (QN "midi-channel" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "midi-name" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "midi-bank" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "midi-program" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "midi-unpitched" Nothing).emitXml) f]++[maybe XEmpty (XElement (QN "volume" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "pan" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "elevation" Nothing).emitXml) i])
mkMidiInstrument :: IDREF -> MidiInstrument
mkMidiInstrument a = MidiInstrument a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- miscellaneous: Complex
data Miscellaneous = 
      Miscellaneous {
          miscellaneousMiscellaneousField :: [MiscellaneousField]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Miscellaneous where
    emitXml (Miscellaneous a) =
      XContent XEmpty
        []
        (map (XElement (QN "miscellaneous-field" Nothing).emitXml) a)
mkMiscellaneous :: Miscellaneous
mkMiscellaneous = Miscellaneous []

-- miscellaneous-field: Complex
data MiscellaneousField = 
      MiscellaneousField {
          miscellaneousFieldString :: String
        , miscellaneousFieldName :: Token
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MiscellaneousField where
    emitXml (MiscellaneousField a b) =
      XContent (emitXml a)
        ([XAttr (QN "name" Nothing) (emitXml b)])
        []
mkMiscellaneousField :: String -> Token -> MiscellaneousField
mkMiscellaneousField a b = MiscellaneousField a b

-- mordent: Complex
data Mordent = 
      Mordent {
          mordentEmptyTrillSound :: Mordent
        , mordentLong :: (Maybe YesNo)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Mordent where
    emitXml (Mordent a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "long" Nothing).emitXml) b])
        ([emitXml a])
mkMordent :: Mordent -> Mordent
mkMordent a = Mordent a Nothing

-- multiple-rest: Complex
data MultipleRest = 
      MultipleRest {
          multipleRestPositiveIntegerOrEmpty :: PositiveIntegerOrEmpty
        , multipleRestUseSymbols :: (Maybe YesNo)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MultipleRest where
    emitXml (MultipleRest a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "use-symbols" Nothing).emitXml) b])
        []
mkMultipleRest :: PositiveIntegerOrEmpty -> MultipleRest
mkMultipleRest a = MultipleRest a Nothing

-- name-display: Complex
data NameDisplay = 
      NameDisplay {
          nameDisplayPrintObject :: (Maybe YesNo)
        , nameDisplayNameDisplay :: [ChxNameDisplay]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NameDisplay where
    emitXml (NameDisplay a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) a])
        ([emitXml b])
mkNameDisplay :: NameDisplay
mkNameDisplay = NameDisplay Nothing []

-- non-arpeggiate: Complex
data NonArpeggiate = 
      NonArpeggiate {
          nonArpeggiateType :: TopBottom
        , nonArpeggiateNumber :: (Maybe NumberLevel)
        , nonArpeggiateDefaultX :: (Maybe Tenths)
        , nonArpeggiateDefaultY :: (Maybe Tenths)
        , nonArpeggiateRelativeX :: (Maybe Tenths)
        , nonArpeggiateRelativeY :: (Maybe Tenths)
        , nonArpeggiatePlacement :: (Maybe AboveBelow)
        , nonArpeggiateColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NonArpeggiate where
    emitXml (NonArpeggiate a b c d e f g h) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
mkNonArpeggiate :: TopBottom -> NonArpeggiate
mkNonArpeggiate a = NonArpeggiate a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- notations: Complex
data Notations = 
      Notations {
          notationsEditorial :: Editorial
        , notationsNotations :: [ChxNotations]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Notations where
    emitXml (Notations a b) =
      XReps [emitXml a,emitXml b]
mkNotations :: Editorial -> Notations
mkNotations a = Notations a []

-- note: Complex
data Note = 
      Note {
          noteDynamics :: (Maybe NonNegativeDecimal)
        , noteEndDynamics :: (Maybe NonNegativeDecimal)
        , noteAttack :: (Maybe Divisions)
        , noteRelease :: (Maybe Divisions)
        , noteTimeOnly :: (Maybe Token)
        , notePizzicato :: (Maybe YesNo)
        , noteDefaultX :: (Maybe Tenths)
        , noteDefaultY :: (Maybe Tenths)
        , noteRelativeX :: (Maybe Tenths)
        , noteRelativeY :: (Maybe Tenths)
        , noteFontFamily :: (Maybe CommaSeparatedText)
        , noteFontStyle :: (Maybe FontStyle)
        , noteFontSize :: (Maybe FontSize)
        , noteFontWeight :: (Maybe FontWeight)
        , noteColor :: (Maybe Color)
        , notePrintDot :: (Maybe YesNo)
        , notePrintLyric :: (Maybe YesNo)
        , notePrintObject :: (Maybe YesNo)
        , notePrintSpacing :: (Maybe YesNo)
        , noteNote :: ChxNote
        , noteInstrument :: (Maybe Instrument)
        , noteEditorialVoice :: EditorialVoice
        , noteType :: (Maybe NoteType)
        , noteDot :: [EmptyPlacement]
        , noteAccidental :: (Maybe Accidental)
        , noteTimeModification :: (Maybe TimeModification)
        , noteStem :: (Maybe Stem)
        , noteNotehead :: (Maybe Notehead)
        , noteStaff :: (Maybe Staff)
        , noteBeam :: [Beam]
        , noteNotations :: [Notations]
        , noteLyric :: [Lyric]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Note where
    emitXml (Note a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "dynamics" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "end-dynamics" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "attack" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "release" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "time-only" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "pizzicato" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "print-dot" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "print-lyric" Nothing).emitXml) q]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) r]++[maybe XEmpty (XAttr (QN "print-spacing" Nothing).emitXml) s])
        ([emitXml t]++[maybe XEmpty (XElement (QN "instrument" Nothing).emitXml) u]++[emitXml v]++[maybe XEmpty (XElement (QN "type" Nothing).emitXml) w]++map (XElement (QN "dot" Nothing).emitXml) x++[maybe XEmpty (XElement (QN "accidental" Nothing).emitXml) y]++[maybe XEmpty (XElement (QN "time-modification" Nothing).emitXml) z]++[maybe XEmpty (XElement (QN "stem" Nothing).emitXml) a1]++[maybe XEmpty (XElement (QN "notehead" Nothing).emitXml) b1]++[emitXml c1]++map (XElement (QN "beam" Nothing).emitXml) d1++map (XElement (QN "notations" Nothing).emitXml) e1++map (XElement (QN "lyric" Nothing).emitXml) f1)
mkNote :: ChxNote -> EditorialVoice -> Note
mkNote t v = Note Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing t Nothing v Nothing [] Nothing Nothing Nothing Nothing Nothing [] [] []

-- note-size: Complex
data NoteSize = 
      NoteSize {
          noteSizeNonNegativeDecimal :: NonNegativeDecimal
        , noteSizeType :: NoteSizeType
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NoteSize where
    emitXml (NoteSize a b) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)])
        []
mkNoteSize :: NonNegativeDecimal -> NoteSizeType -> NoteSize
mkNoteSize a b = NoteSize a b

-- note-type: Complex
data NoteType = 
      NoteType {
          noteTypeNoteTypeValue :: NoteTypeValue
        , noteTypeSize :: (Maybe SymbolSize)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NoteType where
    emitXml (NoteType a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "size" Nothing).emitXml) b])
        []
mkNoteType :: NoteTypeValue -> NoteType
mkNoteType a = NoteType a Nothing

-- notehead: Complex
data Notehead = 
      Notehead {
          noteheadNoteheadValue :: NoteheadValue
        , noteheadFilled :: (Maybe YesNo)
        , noteheadParentheses :: (Maybe YesNo)
        , noteheadFontFamily :: (Maybe CommaSeparatedText)
        , noteheadFontStyle :: (Maybe FontStyle)
        , noteheadFontSize :: (Maybe FontSize)
        , noteheadFontWeight :: (Maybe FontWeight)
        , noteheadColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Notehead where
    emitXml (Notehead a b c d e f g h) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "filled" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "parentheses" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
mkNotehead :: NoteheadValue -> Notehead
mkNotehead a = Notehead a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- octave-shift: Complex
data OctaveShift = 
      OctaveShift {
          octaveShiftType :: UpDownStop
        , octaveShiftNumber :: (Maybe NumberLevel)
        , octaveShiftSize :: (Maybe PositiveInteger)
        , octaveShiftDefaultX :: (Maybe Tenths)
        , octaveShiftDefaultY :: (Maybe Tenths)
        , octaveShiftRelativeX :: (Maybe Tenths)
        , octaveShiftRelativeY :: (Maybe Tenths)
        , octaveShiftFontFamily :: (Maybe CommaSeparatedText)
        , octaveShiftFontStyle :: (Maybe FontStyle)
        , octaveShiftFontSize :: (Maybe FontSize)
        , octaveShiftFontWeight :: (Maybe FontWeight)
        , octaveShiftColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OctaveShift where
    emitXml (OctaveShift a b c d e f g h i j k l) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l])
        []
mkOctaveShift :: UpDownStop -> OctaveShift
mkOctaveShift a = OctaveShift a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- offset: Complex
data Offset = 
      Offset {
          offsetDivisions :: Divisions
        , offsetSound :: (Maybe YesNo)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Offset where
    emitXml (Offset a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "sound" Nothing).emitXml) b])
        []
mkOffset :: Divisions -> Offset
mkOffset a = Offset a Nothing

-- opus: Complex
data Opus = 
      Opus {
          opusHref :: String
        , opusType :: (Maybe Type)
        , opusRole :: (Maybe Token)
        , opusTitle :: (Maybe Token)
        , opusShow :: (Maybe SmpShow)
        , opusActuate :: (Maybe Actuate)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Opus where
    emitXml (Opus a b c d e f) =
      XContent XEmpty
        ([XAttr (QN "href" (Just "xlink")) (emitXml a)]++[maybe XEmpty (XAttr (QN "type" (Just "xlink")).emitXml) b]++[maybe XEmpty (XAttr (QN "role" (Just "xlink")).emitXml) c]++[maybe XEmpty (XAttr (QN "title" (Just "xlink")).emitXml) d]++[maybe XEmpty (XAttr (QN "show" (Just "xlink")).emitXml) e]++[maybe XEmpty (XAttr (QN "actuate" (Just "xlink")).emitXml) f])
        []
mkOpus :: String -> Opus
mkOpus a = Opus a Nothing Nothing Nothing Nothing Nothing

-- ornaments: Complex
data Ornaments = 
      Ornaments {
          ornamentsOrnaments :: [SeqOrnaments]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Ornaments where
    emitXml (Ornaments a) =
      XReps [emitXml a]
mkOrnaments :: Ornaments
mkOrnaments = Ornaments []

-- other-appearance: Complex
data OtherAppearance = 
      OtherAppearance {
          otherAppearanceString :: String
        , otherAppearanceType :: Token
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OtherAppearance where
    emitXml (OtherAppearance a b) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)])
        []
mkOtherAppearance :: String -> Token -> OtherAppearance
mkOtherAppearance a b = OtherAppearance a b

-- other-direction: Complex
data OtherDirection = 
      OtherDirection {
          otherDirectionString :: String
        , otherDirectionPrintObject :: (Maybe YesNo)
        , otherDirectionDefaultX :: (Maybe Tenths)
        , otherDirectionDefaultY :: (Maybe Tenths)
        , otherDirectionRelativeX :: (Maybe Tenths)
        , otherDirectionRelativeY :: (Maybe Tenths)
        , otherDirectionFontFamily :: (Maybe CommaSeparatedText)
        , otherDirectionFontStyle :: (Maybe FontStyle)
        , otherDirectionFontSize :: (Maybe FontSize)
        , otherDirectionFontWeight :: (Maybe FontWeight)
        , otherDirectionColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OtherDirection where
    emitXml (OtherDirection a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkOtherDirection :: String -> OtherDirection
mkOtherDirection a = OtherDirection a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- other-notation: Complex
data OtherNotation = 
      OtherNotation {
          otherNotationString :: String
        , otherNotationType :: StartStopSingle
        , otherNotationNumber :: (Maybe NumberLevel)
        , otherNotationPrintObject :: (Maybe YesNo)
        , otherNotationDefaultX :: (Maybe Tenths)
        , otherNotationDefaultY :: (Maybe Tenths)
        , otherNotationRelativeX :: (Maybe Tenths)
        , otherNotationRelativeY :: (Maybe Tenths)
        , otherNotationFontFamily :: (Maybe CommaSeparatedText)
        , otherNotationFontStyle :: (Maybe FontStyle)
        , otherNotationFontSize :: (Maybe FontSize)
        , otherNotationFontWeight :: (Maybe FontWeight)
        , otherNotationColor :: (Maybe Color)
        , otherNotationPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml OtherNotation where
    emitXml (OtherNotation a b c d e f g h i j k l m n) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) n])
        []
mkOtherNotation :: String -> StartStopSingle -> OtherNotation
mkOtherNotation a b = OtherNotation a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- page-layout: Complex
data PageLayout = 
      PageLayout {
          pageLayoutPageLayout :: (Maybe SeqPageLayout)
        , pageLayoutPageMargins :: [PageMargins]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PageLayout where
    emitXml (PageLayout a b) =
      XContent XEmpty
        []
        ([emitXml a]++map (XElement (QN "page-margins" Nothing).emitXml) b)
mkPageLayout :: PageLayout
mkPageLayout = PageLayout Nothing []

-- page-margins: Complex
data PageMargins = 
      PageMargins {
          pageMarginsType :: (Maybe MarginType)
        , pageMarginsAllMargins :: AllMargins
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PageMargins where
    emitXml (PageMargins a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) a])
        ([emitXml b])
mkPageMargins :: AllMargins -> PageMargins
mkPageMargins b = PageMargins Nothing b

-- part: Complex
data CmpPart = 
      CmpPart {
          partId :: IDREF
        , partMeasure :: [Measure]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpPart where
    emitXml (CmpPart a b) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        (map (XElement (QN "measure" Nothing).emitXml) b)
mkCmpPart :: IDREF -> CmpPart
mkCmpPart a = CmpPart a []

-- part: Complex,1
data Part = 
      Part {
          cmppartId :: IDREF
        , partMusicData :: MusicData
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Part where
    emitXml (Part a b) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([emitXml b])
mkPart :: IDREF -> MusicData -> Part
mkPart a b = Part a b

-- part-group: Complex
data PartGroup = 
      PartGroup {
          partGroupType :: StartStop
        , partGroupNumber :: (Maybe Token)
        , partGroupGroupName :: (Maybe GroupName)
        , partGroupGroupNameDisplay :: (Maybe NameDisplay)
        , partGroupGroupAbbreviation :: (Maybe GroupName)
        , partGroupGroupAbbreviationDisplay :: (Maybe NameDisplay)
        , partGroupGroupSymbol :: (Maybe GroupSymbol)
        , partGroupGroupBarline :: (Maybe GroupBarline)
        , partGroupGroupTime :: (Maybe Empty)
        , partGroupEditorial :: Editorial
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PartGroup where
    emitXml (PartGroup a b c d e f g h i j) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b])
        ([maybe XEmpty (XElement (QN "group-name" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "group-name-display" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "group-abbreviation" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "group-abbreviation-display" Nothing).emitXml) f]++[maybe XEmpty (XElement (QN "group-symbol" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "group-barline" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "group-time" Nothing).emitXml) i]++[emitXml j])
mkPartGroup :: StartStop -> Editorial -> PartGroup
mkPartGroup a j = PartGroup a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing j

-- part-list: Complex
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
mkPartList :: ScorePart -> PartList
mkPartList b = PartList [] b []

-- part-name: Complex
data PartName = 
      PartName {
          partNameString :: String
        , partNameDefaultX :: (Maybe Tenths)
        , partNameDefaultY :: (Maybe Tenths)
        , partNameRelativeX :: (Maybe Tenths)
        , partNameRelativeY :: (Maybe Tenths)
        , partNameFontFamily :: (Maybe CommaSeparatedText)
        , partNameFontStyle :: (Maybe FontStyle)
        , partNameFontSize :: (Maybe FontSize)
        , partNameFontWeight :: (Maybe FontWeight)
        , partNameColor :: (Maybe Color)
        , partNamePrintObject :: (Maybe YesNo)
        , partNameJustify :: (Maybe LeftCenterRight)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PartName where
    emitXml (PartName a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "justify" Nothing).emitXml) l])
        []
mkPartName :: String -> PartName
mkPartName a = PartName a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- part-symbol: Complex
data PartSymbol = 
      PartSymbol {
          partSymbolGroupSymbolValue :: GroupSymbolValue
        , partSymbolTopStaff :: (Maybe StaffNumber)
        , partSymbolBottomStaff :: (Maybe StaffNumber)
        , partSymbolDefaultX :: (Maybe Tenths)
        , partSymbolDefaultY :: (Maybe Tenths)
        , partSymbolRelativeX :: (Maybe Tenths)
        , partSymbolRelativeY :: (Maybe Tenths)
        , partSymbolColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PartSymbol where
    emitXml (PartSymbol a b c d e f g h) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "top-staff" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "bottom-staff" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
mkPartSymbol :: GroupSymbolValue -> PartSymbol
mkPartSymbol a = PartSymbol a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- pedal: Complex
data Pedal = 
      Pedal {
          pedalType :: StartStopChange
        , pedalLine :: (Maybe YesNo)
        , pedalDefaultX :: (Maybe Tenths)
        , pedalDefaultY :: (Maybe Tenths)
        , pedalRelativeX :: (Maybe Tenths)
        , pedalRelativeY :: (Maybe Tenths)
        , pedalFontFamily :: (Maybe CommaSeparatedText)
        , pedalFontStyle :: (Maybe FontStyle)
        , pedalFontSize :: (Maybe FontSize)
        , pedalFontWeight :: (Maybe FontWeight)
        , pedalColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Pedal where
    emitXml (Pedal a b c d e f g h i j k) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "line" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkPedal :: StartStopChange -> Pedal
mkPedal a = Pedal a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- pedal-tuning: Complex
data PedalTuning = 
      PedalTuning {
          pedalTuningPedalStep :: Step
        , pedalTuningPedalAlter :: Semitones
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PedalTuning where
    emitXml (PedalTuning a b) =
      XContent XEmpty
        []
        ([XElement (QN "pedal-step" Nothing) (emitXml a)]++[XElement (QN "pedal-alter" Nothing) (emitXml b)])
mkPedalTuning :: Step -> Semitones -> PedalTuning
mkPedalTuning a b = PedalTuning a b

-- per-minute: Complex
data PerMinute = 
      PerMinute {
          perMinuteString :: String
        , perMinuteFontFamily :: (Maybe CommaSeparatedText)
        , perMinuteFontStyle :: (Maybe FontStyle)
        , perMinuteFontSize :: (Maybe FontSize)
        , perMinuteFontWeight :: (Maybe FontWeight)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PerMinute where
    emitXml (PerMinute a b c d e) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e])
        []
mkPerMinute :: String -> PerMinute
mkPerMinute a = PerMinute a Nothing Nothing Nothing Nothing

-- pitch: Complex
data Pitch = 
      Pitch {
          pitchStep :: Step
        , pitchAlter :: (Maybe Semitones)
        , pitchOctave :: Octave
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Pitch where
    emitXml (Pitch a b c) =
      XContent XEmpty
        []
        ([XElement (QN "step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "alter" Nothing).emitXml) b]++[XElement (QN "octave" Nothing) (emitXml c)])
mkPitch :: Step -> Octave -> Pitch
mkPitch a c = Pitch a Nothing c

-- placement-text: Complex
data PlacementText = 
      PlacementText {
          placementTextString :: String
        , placementTextDefaultX :: (Maybe Tenths)
        , placementTextDefaultY :: (Maybe Tenths)
        , placementTextRelativeX :: (Maybe Tenths)
        , placementTextRelativeY :: (Maybe Tenths)
        , placementTextFontFamily :: (Maybe CommaSeparatedText)
        , placementTextFontStyle :: (Maybe FontStyle)
        , placementTextFontSize :: (Maybe FontSize)
        , placementTextFontWeight :: (Maybe FontWeight)
        , placementTextColor :: (Maybe Color)
        , placementTextPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml PlacementText where
    emitXml (PlacementText a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        []
mkPlacementText :: String -> PlacementText
mkPlacementText a = PlacementText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- print: Complex
data Print = 
      Print {
          printStaffSpacing :: (Maybe Tenths)
        , printNewSystem :: (Maybe YesNo)
        , printNewPage :: (Maybe YesNo)
        , printBlankPage :: (Maybe PositiveInteger)
        , printPageNumber :: (Maybe Token)
        , printLayout :: Layout
        , printMeasureLayout :: (Maybe MeasureLayout)
        , printMeasureNumbering :: (Maybe MeasureNumbering)
        , printPartNameDisplay :: (Maybe NameDisplay)
        , printPartAbbreviationDisplay :: (Maybe NameDisplay)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Print where
    emitXml (Print a b c d e f g h i j) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "staff-spacing" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "new-system" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "new-page" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "blank-page" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "page-number" Nothing).emitXml) e])
        ([emitXml f]++[maybe XEmpty (XElement (QN "measure-layout" Nothing).emitXml) g]++[maybe XEmpty (XElement (QN "measure-numbering" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "part-name-display" Nothing).emitXml) i]++[maybe XEmpty (XElement (QN "part-abbreviation-display" Nothing).emitXml) j])
mkPrint :: Layout -> Print
mkPrint f = Print Nothing Nothing Nothing Nothing Nothing f Nothing Nothing Nothing Nothing

-- rehearsal: Complex
data Rehearsal = 
      Rehearsal {
          rehearsalString :: String
        , rehearsalLang :: (Maybe Lang)
        , rehearsalEnclosure :: (Maybe RehearsalEnclosure)
        , rehearsalDefaultX :: (Maybe Tenths)
        , rehearsalDefaultY :: (Maybe Tenths)
        , rehearsalRelativeX :: (Maybe Tenths)
        , rehearsalRelativeY :: (Maybe Tenths)
        , rehearsalFontFamily :: (Maybe CommaSeparatedText)
        , rehearsalFontStyle :: (Maybe FontStyle)
        , rehearsalFontSize :: (Maybe FontSize)
        , rehearsalFontWeight :: (Maybe FontWeight)
        , rehearsalColor :: (Maybe Color)
        , rehearsalUnderline :: (Maybe NumberOfLines)
        , rehearsalOverline :: (Maybe NumberOfLines)
        , rehearsalLineThrough :: (Maybe NumberOfLines)
        , rehearsalDir :: (Maybe TextDirection)
        , rehearsalRotation :: (Maybe RotationDegrees)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Rehearsal where
    emitXml (Rehearsal a b c d e f g h i j k l m n o p q) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "enclosure" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) q])
        []
mkRehearsal :: String -> Rehearsal
mkRehearsal a = Rehearsal a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- repeat: Complex
data Repeat = 
      Repeat {
          repeatDirection :: BackwardForward
        , repeatTimes :: (Maybe NonNegativeInteger)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Repeat where
    emitXml (Repeat a b) =
      XContent XEmpty
        ([XAttr (QN "direction" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "times" Nothing).emitXml) b])
        []
mkRepeat :: BackwardForward -> Repeat
mkRepeat a = Repeat a Nothing

-- root: Complex
data Root = 
      Root {
          rootRootStep :: RootStep
        , rootRootAlter :: (Maybe RootAlter)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Root where
    emitXml (Root a b) =
      XContent XEmpty
        []
        ([XElement (QN "root-step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "root-alter" Nothing).emitXml) b])
mkRoot :: RootStep -> Root
mkRoot a = Root a Nothing

-- root-alter: Complex
data RootAlter = 
      RootAlter {
          rootAlterSemitones :: Semitones
        , rootAlterLocation :: (Maybe LeftRight)
        , rootAlterPrintObject :: (Maybe YesNo)
        , rootAlterDefaultX :: (Maybe Tenths)
        , rootAlterDefaultY :: (Maybe Tenths)
        , rootAlterRelativeX :: (Maybe Tenths)
        , rootAlterRelativeY :: (Maybe Tenths)
        , rootAlterFontFamily :: (Maybe CommaSeparatedText)
        , rootAlterFontStyle :: (Maybe FontStyle)
        , rootAlterFontSize :: (Maybe FontSize)
        , rootAlterFontWeight :: (Maybe FontWeight)
        , rootAlterColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml RootAlter where
    emitXml (RootAlter a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "location" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) l])
        []
mkRootAlter :: Semitones -> RootAlter
mkRootAlter a = RootAlter a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- root-step: Complex
data RootStep = 
      RootStep {
          rootStepStep :: Step
        , rootStepText :: (Maybe Token)
        , rootStepDefaultX :: (Maybe Tenths)
        , rootStepDefaultY :: (Maybe Tenths)
        , rootStepRelativeX :: (Maybe Tenths)
        , rootStepRelativeY :: (Maybe Tenths)
        , rootStepFontFamily :: (Maybe CommaSeparatedText)
        , rootStepFontStyle :: (Maybe FontStyle)
        , rootStepFontSize :: (Maybe FontSize)
        , rootStepFontWeight :: (Maybe FontWeight)
        , rootStepColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml RootStep where
    emitXml (RootStep a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "text" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k])
        []
mkRootStep :: Step -> RootStep
mkRootStep a = RootStep a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- scaling: Complex
data Scaling = 
      Scaling {
          scalingMillimeters :: Millimeters
        , scalingTenths :: Tenths
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Scaling where
    emitXml (Scaling a b) =
      XContent XEmpty
        []
        ([XElement (QN "millimeters" Nothing) (emitXml a)]++[XElement (QN "tenths" Nothing) (emitXml b)])
mkScaling :: Millimeters -> Tenths -> Scaling
mkScaling a b = Scaling a b

-- scordatura: Complex
data Scordatura = 
      Scordatura {
          scordaturaAccord :: [Accord]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Scordatura where
    emitXml (Scordatura a) =
      XContent XEmpty
        []
        (map (XElement (QN "accord" Nothing).emitXml) a)
mkScordatura :: Scordatura
mkScordatura = Scordatura []

-- score-instrument: Complex
data ScoreInstrument = 
      ScoreInstrument {
          scoreInstrumentId :: ID
        , scoreInstrumentInstrumentName :: String
        , scoreInstrumentInstrumentAbbreviation :: (Maybe String)
        , scoreInstrumentScoreInstrument :: (Maybe ChxScoreInstrument)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScoreInstrument where
    emitXml (ScoreInstrument a b c d) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([XElement (QN "instrument-name" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "instrument-abbreviation" Nothing).emitXml) c]++[emitXml d])
mkScoreInstrument :: ID -> String -> ScoreInstrument
mkScoreInstrument a b = ScoreInstrument a b Nothing Nothing

-- score-part: Complex
data CmpScorePart = 
      CmpScorePart {
          scorePartId :: ID
        , scorePartIdentification :: (Maybe Identification)
        , scorePartPartName :: PartName
        , scorePartPartNameDisplay :: (Maybe NameDisplay)
        , scorePartPartAbbreviation :: (Maybe PartName)
        , scorePartPartAbbreviationDisplay :: (Maybe NameDisplay)
        , scorePartGroup :: [String]
        , scorePartScoreInstrument :: [ScoreInstrument]
        , scorePartMidiDevice :: (Maybe MidiDevice)
        , scorePartMidiInstrument :: [MidiInstrument]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpScorePart where
    emitXml (CmpScorePart a b c d e f g h i j) =
      XContent XEmpty
        ([XAttr (QN "id" Nothing) (emitXml a)])
        ([maybe XEmpty (XElement (QN "identification" Nothing).emitXml) b]++[XElement (QN "part-name" Nothing) (emitXml c)]++[maybe XEmpty (XElement (QN "part-name-display" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "part-abbreviation" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "part-abbreviation-display" Nothing).emitXml) f]++map (XElement (QN "group" Nothing).emitXml) g++map (XElement (QN "score-instrument" Nothing).emitXml) h++[maybe XEmpty (XElement (QN "midi-device" Nothing).emitXml) i]++map (XElement (QN "midi-instrument" Nothing).emitXml) j)
mkCmpScorePart :: ID -> PartName -> CmpScorePart
mkCmpScorePart a c = CmpScorePart a Nothing c Nothing Nothing Nothing [] [] Nothing []

-- score-partwise: Complex
data ScorePartwise = 
      ScorePartwise {
          scorePartwiseVersion :: (Maybe Token)
        , scorePartwiseScoreHeader :: ScoreHeader
        , scorePartwisePart :: [CmpPart]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScorePartwise where
    emitXml (ScorePartwise a b c) =
      XElement (QN "score-partwise" Nothing) $ XContent XEmpty
        ([maybe XEmpty (XAttr (QN "version" Nothing).emitXml) a])
        ([emitXml b]++map (XElement (QN "part" Nothing).emitXml) c)
mkScorePartwise :: ScoreHeader -> ScorePartwise
mkScorePartwise b = ScorePartwise Nothing b []

-- score-timewise: Complex
data ScoreTimewise = 
      ScoreTimewise {
          scoreTimewiseVersion :: (Maybe Token)
        , scoreTimewiseScoreHeader :: ScoreHeader
        , scoreTimewiseMeasure :: [CmpMeasure]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScoreTimewise where
    emitXml (ScoreTimewise a b c) =
      XElement (QN "score-timewise" Nothing) $ XContent XEmpty
        ([maybe XEmpty (XAttr (QN "version" Nothing).emitXml) a])
        ([emitXml b]++map (XElement (QN "measure" Nothing).emitXml) c)
mkScoreTimewise :: ScoreHeader -> ScoreTimewise
mkScoreTimewise b = ScoreTimewise Nothing b []

-- slash: Complex
data CmpSlash = 
      CmpSlash {
          slashType :: StartStop
        , slashUseDots :: (Maybe YesNo)
        , slashUseStems :: (Maybe YesNo)
        , slashSlash :: (Maybe Slash)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpSlash where
    emitXml (CmpSlash a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "use-dots" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "use-stems" Nothing).emitXml) c])
        ([emitXml d])
mkCmpSlash :: StartStop -> CmpSlash
mkCmpSlash a = CmpSlash a Nothing Nothing Nothing

-- slide: Complex
data Slide = 
      Slide {
          slideString :: String
        , slideType :: StartStop
        , slideNumber :: (Maybe NumberLevel)
        , slideLineType :: (Maybe LineType)
        , slideDefaultX :: (Maybe Tenths)
        , slideDefaultY :: (Maybe Tenths)
        , slideRelativeX :: (Maybe Tenths)
        , slideRelativeY :: (Maybe Tenths)
        , slideFontFamily :: (Maybe CommaSeparatedText)
        , slideFontStyle :: (Maybe FontStyle)
        , slideFontSize :: (Maybe FontSize)
        , slideFontWeight :: (Maybe FontWeight)
        , slideColor :: (Maybe Color)
        , slideAccelerate :: (Maybe YesNo)
        , slideBeats :: (Maybe TrillBeats)
        , slideFirstBeat :: (Maybe Percent)
        , slideLastBeat :: (Maybe Percent)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Slide where
    emitXml (Slide a b c d e f g h i j k l m n o p q) =
      XContent (emitXml a)
        ([XAttr (QN "type" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "first-beat" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) q])
        []
mkSlide :: String -> StartStop -> Slide
mkSlide a b = Slide a b Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- slur: Complex
data Slur = 
      Slur {
          slurType :: StartStopContinue
        , slurNumber :: (Maybe NumberLevel)
        , slurLineType :: (Maybe LineType)
        , slurDefaultX :: (Maybe Tenths)
        , slurDefaultY :: (Maybe Tenths)
        , slurRelativeX :: (Maybe Tenths)
        , slurRelativeY :: (Maybe Tenths)
        , slurPlacement :: (Maybe AboveBelow)
        , slurOrientation :: (Maybe OverUnder)
        , slurBezierOffset :: (Maybe Divisions)
        , slurBezierOffset2 :: (Maybe Divisions)
        , slurBezierX :: (Maybe Tenths)
        , slurBezierY :: (Maybe Tenths)
        , slurBezierX2 :: (Maybe Tenths)
        , slurBezierY2 :: (Maybe Tenths)
        , slurColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Slur where
    emitXml (Slur a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "orientation" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "bezier-offset" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "bezier-offset2" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "bezier-x" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "bezier-y" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "bezier-x2" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "bezier-y2" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) p])
        []
mkSlur :: StartStopContinue -> Slur
mkSlur a = Slur a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- sound: Complex
data Sound = 
      Sound {
          soundTempo :: (Maybe NonNegativeDecimal)
        , soundDynamics :: (Maybe NonNegativeDecimal)
        , soundDacapo :: (Maybe YesNo)
        , soundSegno :: (Maybe Token)
        , soundDalsegno :: (Maybe Token)
        , soundCoda :: (Maybe Token)
        , soundTocoda :: (Maybe Token)
        , soundDivisions :: (Maybe Divisions)
        , soundForwardRepeat :: (Maybe YesNo)
        , soundFine :: (Maybe Token)
        , soundTimeOnly :: (Maybe Token)
        , soundPizzicato :: (Maybe YesNo)
        , soundPan :: (Maybe RotationDegrees)
        , soundElevation :: (Maybe RotationDegrees)
        , soundDamperPedal :: (Maybe YesNoNumber)
        , soundSoftPedal :: (Maybe YesNoNumber)
        , soundSostenutoPedal :: (Maybe YesNoNumber)
        , soundMidiInstrument :: [MidiInstrument]
        , soundOffset :: (Maybe Offset)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Sound where
    emitXml (Sound a b c d e f g h i j k l m n o p q r s) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "tempo" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "dynamics" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "dacapo" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "segno" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "dalsegno" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "coda" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "tocoda" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "divisions" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "forward-repeat" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "fine" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "time-only" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "pizzicato" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "pan" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "elevation" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "damper-pedal" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "soft-pedal" Nothing).emitXml) p]++[maybe XEmpty (XAttr (QN "sostenuto-pedal" Nothing).emitXml) q])
        (map (XElement (QN "midi-instrument" Nothing).emitXml) r++[maybe XEmpty (XElement (QN "offset" Nothing).emitXml) s])
mkSound :: Sound
mkSound = Sound Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing

-- staff-details: Complex
data StaffDetails = 
      StaffDetails {
          staffDetailsNumber :: (Maybe StaffNumber)
        , staffDetailsShowFrets :: (Maybe ShowFrets)
        , staffDetailsPrintObject :: (Maybe YesNo)
        , staffDetailsPrintSpacing :: (Maybe YesNo)
        , staffDetailsStaffType :: (Maybe StaffType)
        , staffDetailsStaffLines :: (Maybe NonNegativeInteger)
        , staffDetailsStaffTuning :: [StaffTuning]
        , staffDetailsCapo :: (Maybe NonNegativeInteger)
        , staffDetailsStaffSize :: (Maybe NonNegativeDecimal)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StaffDetails where
    emitXml (StaffDetails a b c d e f g h i) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "show-frets" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "print-spacing" Nothing).emitXml) d])
        ([maybe XEmpty (XElement (QN "staff-type" Nothing).emitXml) e]++[maybe XEmpty (XElement (QN "staff-lines" Nothing).emitXml) f]++map (XElement (QN "staff-tuning" Nothing).emitXml) g++[maybe XEmpty (XElement (QN "capo" Nothing).emitXml) h]++[maybe XEmpty (XElement (QN "staff-size" Nothing).emitXml) i])
mkStaffDetails :: StaffDetails
mkStaffDetails = StaffDetails Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing

-- staff-layout: Complex
data StaffLayout = 
      StaffLayout {
          staffLayoutNumber :: (Maybe StaffNumber)
        , staffLayoutStaffDistance :: (Maybe Tenths)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StaffLayout where
    emitXml (StaffLayout a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a])
        ([maybe XEmpty (XElement (QN "staff-distance" Nothing).emitXml) b])
mkStaffLayout :: StaffLayout
mkStaffLayout = StaffLayout Nothing Nothing

-- staff-tuning: Complex
data StaffTuning = 
      StaffTuning {
          staffTuningLine :: (Maybe StaffLine)
        , staffTuningTuning :: Tuning
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StaffTuning where
    emitXml (StaffTuning a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "line" Nothing).emitXml) a])
        ([emitXml b])
mkStaffTuning :: Tuning -> StaffTuning
mkStaffTuning b = StaffTuning Nothing b

-- stem: Complex
data Stem = 
      Stem {
          stemStemValue :: StemValue
        , stemDefaultX :: (Maybe Tenths)
        , stemDefaultY :: (Maybe Tenths)
        , stemRelativeX :: (Maybe Tenths)
        , stemRelativeY :: (Maybe Tenths)
        , stemColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Stem where
    emitXml (Stem a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
mkStem :: StemValue -> Stem
mkStem a = Stem a Nothing Nothing Nothing Nothing Nothing

-- string: Complex
data CmpString = 
      CmpString {
          stringStringNumber :: StringNumber
        , stringDefaultX :: (Maybe Tenths)
        , stringDefaultY :: (Maybe Tenths)
        , stringRelativeX :: (Maybe Tenths)
        , stringRelativeY :: (Maybe Tenths)
        , stringFontFamily :: (Maybe CommaSeparatedText)
        , stringFontStyle :: (Maybe FontStyle)
        , stringFontSize :: (Maybe FontSize)
        , stringFontWeight :: (Maybe FontWeight)
        , stringColor :: (Maybe Color)
        , stringPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml CmpString where
    emitXml (CmpString a b c d e f g h i j k) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        []
mkCmpString :: StringNumber -> CmpString
mkCmpString a = CmpString a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- strong-accent: Complex
data StrongAccent = 
      StrongAccent {
          strongAccentEmptyPlacement :: StrongAccent
        , strongAccentType :: (Maybe UpDown)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StrongAccent where
    emitXml (StrongAccent a b) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b])
        ([emitXml a])
mkStrongAccent :: StrongAccent -> StrongAccent
mkStrongAccent a = StrongAccent a Nothing

-- style-text: Complex
data StyleText = 
      StyleText {
          styleTextString :: String
        , styleTextDefaultX :: (Maybe Tenths)
        , styleTextDefaultY :: (Maybe Tenths)
        , styleTextRelativeX :: (Maybe Tenths)
        , styleTextRelativeY :: (Maybe Tenths)
        , styleTextFontFamily :: (Maybe CommaSeparatedText)
        , styleTextFontStyle :: (Maybe FontStyle)
        , styleTextFontSize :: (Maybe FontSize)
        , styleTextFontWeight :: (Maybe FontWeight)
        , styleTextColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml StyleText where
    emitXml (StyleText a b c d e f g h i j) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) j])
        []
mkStyleText :: String -> StyleText
mkStyleText a = StyleText a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- supports: Complex
data Supports = 
      Supports {
          supportsType :: YesNo
        , supportsElement :: NMTOKEN
        , supportsAttribute :: (Maybe NMTOKEN)
        , supportsValue :: (Maybe Token)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Supports where
    emitXml (Supports a b c d) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[XAttr (QN "element" Nothing) (emitXml b)]++[maybe XEmpty (XAttr (QN "attribute" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "value" Nothing).emitXml) d])
        []
mkSupports :: YesNo -> NMTOKEN -> Supports
mkSupports a b = Supports a b Nothing Nothing

-- system-layout: Complex
data SystemLayout = 
      SystemLayout {
          systemLayoutSystemMargins :: (Maybe SystemMargins)
        , systemLayoutSystemDistance :: (Maybe Tenths)
        , systemLayoutTopSystemDistance :: (Maybe Tenths)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SystemLayout where
    emitXml (SystemLayout a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "system-margins" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "system-distance" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "top-system-distance" Nothing).emitXml) c])
mkSystemLayout :: SystemLayout
mkSystemLayout = SystemLayout Nothing Nothing Nothing

-- system-margins: Complex
data SystemMargins = 
      SystemMargins {
          systemMarginsLeftRightMargins :: LeftRightMargins
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SystemMargins where
    emitXml (SystemMargins a) =
      XReps [emitXml a]
mkSystemMargins :: LeftRightMargins -> SystemMargins
mkSystemMargins a = SystemMargins a

-- technical: Complex
data Technical = 
      Technical {
          technicalTechnical :: [ChxTechnical]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Technical where
    emitXml (Technical a) =
      XReps [emitXml a]
mkTechnical :: Technical
mkTechnical = Technical []

-- text-element-data: Complex
data TextElementData = 
      TextElementData {
          textElementDataString :: String
        , textElementDataLang :: (Maybe Lang)
        , textElementDataFontFamily :: (Maybe CommaSeparatedText)
        , textElementDataFontStyle :: (Maybe FontStyle)
        , textElementDataFontSize :: (Maybe FontSize)
        , textElementDataFontWeight :: (Maybe FontWeight)
        , textElementDataColor :: (Maybe Color)
        , textElementDataUnderline :: (Maybe NumberOfLines)
        , textElementDataOverline :: (Maybe NumberOfLines)
        , textElementDataLineThrough :: (Maybe NumberOfLines)
        , textElementDataRotation :: (Maybe RotationDegrees)
        , textElementDataLetterSpacing :: (Maybe NumberOrNormal)
        , textElementDataDir :: (Maybe TextDirection)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TextElementData where
    emitXml (TextElementData a b c d e f g h i j k l m) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "lang" (Just "xml")).emitXml) b]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "underline" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "overline" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "line-through" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "rotation" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "letter-spacing" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "dir" Nothing).emitXml) m])
        []
mkTextElementData :: String -> TextElementData
mkTextElementData a = TextElementData a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- tie: Complex
data Tie = 
      Tie {
          tieType :: StartStop
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tie where
    emitXml (Tie a) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)])
        []
mkTie :: StartStop -> Tie
mkTie a = Tie a

-- tied: Complex
data Tied = 
      Tied {
          tiedType :: StartStop
        , tiedNumber :: (Maybe NumberLevel)
        , tiedLineType :: (Maybe LineType)
        , tiedDefaultX :: (Maybe Tenths)
        , tiedDefaultY :: (Maybe Tenths)
        , tiedRelativeX :: (Maybe Tenths)
        , tiedRelativeY :: (Maybe Tenths)
        , tiedPlacement :: (Maybe AboveBelow)
        , tiedOrientation :: (Maybe OverUnder)
        , tiedBezierOffset :: (Maybe Divisions)
        , tiedBezierOffset2 :: (Maybe Divisions)
        , tiedBezierX :: (Maybe Tenths)
        , tiedBezierY :: (Maybe Tenths)
        , tiedBezierX2 :: (Maybe Tenths)
        , tiedBezierY2 :: (Maybe Tenths)
        , tiedColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tied where
    emitXml (Tied a b c d e f g h i j k l m n o p) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "line-type" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "orientation" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "bezier-offset" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "bezier-offset2" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "bezier-x" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "bezier-y" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "bezier-x2" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "bezier-y2" Nothing).emitXml) o]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) p])
        []
mkTied :: StartStop -> Tied
mkTied a = Tied a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- time: Complex
data Time = 
      Time {
          timeNumber :: (Maybe StaffNumber)
        , timeSymbol :: (Maybe TimeSymbol)
        , timeDefaultX :: (Maybe Tenths)
        , timeDefaultY :: (Maybe Tenths)
        , timeRelativeX :: (Maybe Tenths)
        , timeRelativeY :: (Maybe Tenths)
        , timeFontFamily :: (Maybe CommaSeparatedText)
        , timeFontStyle :: (Maybe FontStyle)
        , timeFontSize :: (Maybe FontSize)
        , timeFontWeight :: (Maybe FontWeight)
        , timeColor :: (Maybe Color)
        , timePrintObject :: (Maybe YesNo)
        , timeTime :: ChxTime
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Time where
    emitXml (Time a b c d e f g h i j k l m) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "number" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "symbol" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "print-object" Nothing).emitXml) l])
        ([emitXml m])
mkTime :: ChxTime -> Time
mkTime m = Time Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing m

-- time-modification: Complex
data TimeModification = 
      TimeModification {
          timeModificationActualNotes :: NonNegativeInteger
        , timeModificationNormalNotes :: NonNegativeInteger
        , timeModificationTimeModification :: (Maybe SeqTimeModification)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TimeModification where
    emitXml (TimeModification a b c) =
      XContent XEmpty
        []
        ([XElement (QN "actual-notes" Nothing) (emitXml a)]++[XElement (QN "normal-notes" Nothing) (emitXml b)]++[emitXml c])
mkTimeModification :: NonNegativeInteger -> NonNegativeInteger -> TimeModification
mkTimeModification a b = TimeModification a b Nothing

-- transpose: Complex
data Transpose = 
      Transpose {
          transposeDiatonic :: (Maybe Int)
        , transposeChromatic :: Semitones
        , transposeOctaveChange :: (Maybe Int)
        , transposeDouble :: (Maybe Empty)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Transpose where
    emitXml (Transpose a b c d) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "diatonic" Nothing).emitXml) a]++[XElement (QN "chromatic" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "octave-change" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "double" Nothing).emitXml) d])
mkTranspose :: Semitones -> Transpose
mkTranspose b = Transpose Nothing b Nothing Nothing

-- tremolo: Complex
data Tremolo = 
      Tremolo {
          tremoloTremoloMarks :: TremoloMarks
        , tremoloType :: (Maybe StartStopSingle)
        , tremoloDefaultX :: (Maybe Tenths)
        , tremoloDefaultY :: (Maybe Tenths)
        , tremoloRelativeX :: (Maybe Tenths)
        , tremoloRelativeY :: (Maybe Tenths)
        , tremoloFontFamily :: (Maybe CommaSeparatedText)
        , tremoloFontStyle :: (Maybe FontStyle)
        , tremoloFontSize :: (Maybe FontSize)
        , tremoloFontWeight :: (Maybe FontWeight)
        , tremoloColor :: (Maybe Color)
        , tremoloPlacement :: (Maybe AboveBelow)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tremolo where
    emitXml (Tremolo a b c d e f g h i j k l) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) l])
        []
mkTremolo :: TremoloMarks -> Tremolo
mkTremolo a = Tremolo a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- tuplet: Complex
data Tuplet = 
      Tuplet {
          tupletType :: StartStop
        , tupletNumber :: (Maybe NumberLevel)
        , tupletBracket :: (Maybe YesNo)
        , tupletShowNumber :: (Maybe ShowTuplet)
        , tupletShowType :: (Maybe ShowTuplet)
        , tupletLineShape :: (Maybe LineShape)
        , tupletDefaultX :: (Maybe Tenths)
        , tupletDefaultY :: (Maybe Tenths)
        , tupletRelativeX :: (Maybe Tenths)
        , tupletRelativeY :: (Maybe Tenths)
        , tupletPlacement :: (Maybe AboveBelow)
        , tupletTupletActual :: (Maybe TupletPortion)
        , tupletTupletNormal :: (Maybe TupletPortion)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tuplet where
    emitXml (Tuplet a b c d e f g h i j k l m) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "bracket" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "show-number" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "show-type" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "line-shape" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) k])
        ([maybe XEmpty (XElement (QN "tuplet-actual" Nothing).emitXml) l]++[maybe XEmpty (XElement (QN "tuplet-normal" Nothing).emitXml) m])
mkTuplet :: StartStop -> Tuplet
mkTuplet a = Tuplet a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- tuplet-dot: Complex
data TupletDot = 
      TupletDot {
          tupletDotFontFamily :: (Maybe CommaSeparatedText)
        , tupletDotFontStyle :: (Maybe FontStyle)
        , tupletDotFontSize :: (Maybe FontSize)
        , tupletDotFontWeight :: (Maybe FontWeight)
        , tupletDotColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletDot where
    emitXml (TupletDot a b c d e) =
      XContent XEmpty
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) a]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) e])
        []
mkTupletDot :: TupletDot
mkTupletDot = TupletDot Nothing Nothing Nothing Nothing Nothing

-- tuplet-number: Complex
data TupletNumber = 
      TupletNumber {
          tupletNumberNonNegativeInteger :: NonNegativeInteger
        , tupletNumberFontFamily :: (Maybe CommaSeparatedText)
        , tupletNumberFontStyle :: (Maybe FontStyle)
        , tupletNumberFontSize :: (Maybe FontSize)
        , tupletNumberFontWeight :: (Maybe FontWeight)
        , tupletNumberColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletNumber where
    emitXml (TupletNumber a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
mkTupletNumber :: NonNegativeInteger -> TupletNumber
mkTupletNumber a = TupletNumber a Nothing Nothing Nothing Nothing Nothing

-- tuplet-portion: Complex
data TupletPortion = 
      TupletPortion {
          tupletPortionTupletNumber :: (Maybe TupletNumber)
        , tupletPortionTupletType :: (Maybe TupletType)
        , tupletPortionTupletDot :: [TupletDot]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletPortion where
    emitXml (TupletPortion a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "tuplet-number" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "tuplet-type" Nothing).emitXml) b]++map (XElement (QN "tuplet-dot" Nothing).emitXml) c)
mkTupletPortion :: TupletPortion
mkTupletPortion = TupletPortion Nothing Nothing []

-- tuplet-type: Complex
data TupletType = 
      TupletType {
          tupletTypeNoteTypeValue :: NoteTypeValue
        , tupletTypeFontFamily :: (Maybe CommaSeparatedText)
        , tupletTypeFontStyle :: (Maybe FontStyle)
        , tupletTypeFontSize :: (Maybe FontSize)
        , tupletTypeFontWeight :: (Maybe FontWeight)
        , tupletTypeColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TupletType where
    emitXml (TupletType a b c d e f) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "font-family" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "font-style" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "font-size" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "font-weight" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) f])
        []
mkTupletType :: NoteTypeValue -> TupletType
mkTupletType a = TupletType a Nothing Nothing Nothing Nothing Nothing

-- typed-text: Complex
data TypedText = 
      TypedText {
          typedTextString :: String
        , typedTextType :: (Maybe Token)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TypedText where
    emitXml (TypedText a b) =
      XContent (emitXml a)
        ([maybe XEmpty (XAttr (QN "type" Nothing).emitXml) b])
        []
mkTypedText :: String -> TypedText
mkTypedText a = TypedText a Nothing

-- wavy-line: Complex
data WavyLine = 
      WavyLine {
          wavyLineType :: StartStopContinue
        , wavyLineNumber :: (Maybe NumberLevel)
        , wavyLineDefaultX :: (Maybe Tenths)
        , wavyLineDefaultY :: (Maybe Tenths)
        , wavyLineRelativeX :: (Maybe Tenths)
        , wavyLineRelativeY :: (Maybe Tenths)
        , wavyLinePlacement :: (Maybe AboveBelow)
        , wavyLineColor :: (Maybe Color)
        , wavyLineStartNote :: (Maybe StartNote)
        , wavyLineTrillStep :: (Maybe TrillStep)
        , wavyLineTwoNoteTurn :: (Maybe TwoNoteTurn)
        , wavyLineAccelerate :: (Maybe YesNo)
        , wavyLineBeats :: (Maybe TrillBeats)
        , wavyLineSecondBeat :: (Maybe Percent)
        , wavyLineLastBeat :: (Maybe Percent)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml WavyLine where
    emitXml (WavyLine a b c d e f g h i j k l m n o) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "placement" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h]++[maybe XEmpty (XAttr (QN "start-note" Nothing).emitXml) i]++[maybe XEmpty (XAttr (QN "trill-step" Nothing).emitXml) j]++[maybe XEmpty (XAttr (QN "two-note-turn" Nothing).emitXml) k]++[maybe XEmpty (XAttr (QN "accelerate" Nothing).emitXml) l]++[maybe XEmpty (XAttr (QN "beats" Nothing).emitXml) m]++[maybe XEmpty (XAttr (QN "second-beat" Nothing).emitXml) n]++[maybe XEmpty (XAttr (QN "last-beat" Nothing).emitXml) o])
        []
mkWavyLine :: StartStopContinue -> WavyLine
mkWavyLine a = WavyLine a Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- wedge: Complex
data Wedge = 
      Wedge {
          wedgeType :: WedgeType
        , wedgeNumber :: (Maybe NumberLevel)
        , wedgeSpread :: (Maybe Tenths)
        , wedgeDefaultX :: (Maybe Tenths)
        , wedgeDefaultY :: (Maybe Tenths)
        , wedgeRelativeX :: (Maybe Tenths)
        , wedgeRelativeY :: (Maybe Tenths)
        , wedgeColor :: (Maybe Color)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Wedge where
    emitXml (Wedge a b c d e f g h) =
      XContent XEmpty
        ([XAttr (QN "type" Nothing) (emitXml a)]++[maybe XEmpty (XAttr (QN "number" Nothing).emitXml) b]++[maybe XEmpty (XAttr (QN "spread" Nothing).emitXml) c]++[maybe XEmpty (XAttr (QN "default-x" Nothing).emitXml) d]++[maybe XEmpty (XAttr (QN "default-y" Nothing).emitXml) e]++[maybe XEmpty (XAttr (QN "relative-x" Nothing).emitXml) f]++[maybe XEmpty (XAttr (QN "relative-y" Nothing).emitXml) g]++[maybe XEmpty (XAttr (QN "color" Nothing).emitXml) h])
        []
mkWedge :: WedgeType -> Wedge
mkWedge a = Wedge a Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- work: Complex
data Work = 
      Work {
          workWorkNumber :: (Maybe String)
        , workWorkTitle :: (Maybe String)
        , workOpus :: (Maybe Opus)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Work where
    emitXml (Work a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "work-number" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "work-title" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "opus" Nothing).emitXml) c])
mkWork :: Work
mkWork = Work Nothing Nothing Nothing

-- articulations: Choice
data ChxArticulations = 
      ArticulationsAccent {
          articulationsAccent :: EmptyPlacement
       }
    | ArticulationsStrongAccent {
          articulationsStrongAccent :: StrongAccent
       }
    | ArticulationsStaccato {
          articulationsStaccato :: EmptyPlacement
       }
    | ArticulationsTenuto {
          articulationsTenuto :: EmptyPlacement
       }
    | ArticulationsDetachedLegato {
          articulationsDetachedLegato :: EmptyPlacement
       }
    | ArticulationsStaccatissimo {
          articulationsStaccatissimo :: EmptyPlacement
       }
    | ArticulationsSpiccato {
          articulationsSpiccato :: EmptyPlacement
       }
    | ArticulationsScoop {
          articulationsScoop :: EmptyLine
       }
    | ArticulationsPlop {
          articulationsPlop :: EmptyLine
       }
    | ArticulationsDoit {
          articulationsDoit :: EmptyLine
       }
    | ArticulationsFalloff {
          articulationsFalloff :: EmptyLine
       }
    | ArticulationsBreathMark {
          articulationsBreathMark :: EmptyPlacement
       }
    | ArticulationsCaesura {
          articulationsCaesura :: EmptyPlacement
       }
    | ArticulationsStress {
          articulationsStress :: EmptyPlacement
       }
    | ArticulationsUnstress {
          articulationsUnstress :: EmptyPlacement
       }
    | ArticulationsOtherArticulation {
          articulationsOtherArticulation :: PlacementText
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
mkArticulationsAccent :: EmptyPlacement -> ChxArticulations
mkArticulationsAccent a = ArticulationsAccent a
mkArticulationsStrongAccent :: StrongAccent -> ChxArticulations
mkArticulationsStrongAccent a = ArticulationsStrongAccent a
mkArticulationsStaccato :: EmptyPlacement -> ChxArticulations
mkArticulationsStaccato a = ArticulationsStaccato a
mkArticulationsTenuto :: EmptyPlacement -> ChxArticulations
mkArticulationsTenuto a = ArticulationsTenuto a
mkArticulationsDetachedLegato :: EmptyPlacement -> ChxArticulations
mkArticulationsDetachedLegato a = ArticulationsDetachedLegato a
mkArticulationsStaccatissimo :: EmptyPlacement -> ChxArticulations
mkArticulationsStaccatissimo a = ArticulationsStaccatissimo a
mkArticulationsSpiccato :: EmptyPlacement -> ChxArticulations
mkArticulationsSpiccato a = ArticulationsSpiccato a
mkArticulationsScoop :: EmptyLine -> ChxArticulations
mkArticulationsScoop a = ArticulationsScoop a
mkArticulationsPlop :: EmptyLine -> ChxArticulations
mkArticulationsPlop a = ArticulationsPlop a
mkArticulationsDoit :: EmptyLine -> ChxArticulations
mkArticulationsDoit a = ArticulationsDoit a
mkArticulationsFalloff :: EmptyLine -> ChxArticulations
mkArticulationsFalloff a = ArticulationsFalloff a
mkArticulationsBreathMark :: EmptyPlacement -> ChxArticulations
mkArticulationsBreathMark a = ArticulationsBreathMark a
mkArticulationsCaesura :: EmptyPlacement -> ChxArticulations
mkArticulationsCaesura a = ArticulationsCaesura a
mkArticulationsStress :: EmptyPlacement -> ChxArticulations
mkArticulationsStress a = ArticulationsStress a
mkArticulationsUnstress :: EmptyPlacement -> ChxArticulations
mkArticulationsUnstress a = ArticulationsUnstress a
mkArticulationsOtherArticulation :: PlacementText -> ChxArticulations
mkArticulationsOtherArticulation a = ArticulationsOtherArticulation a

-- bend: Choice
data ChxBend = 
      BendPreBend {
          bendPreBend :: Empty
       }
    | BendRelease {
          bendRelease :: Empty
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
mkBendPreBend :: Empty -> ChxBend
mkBendPreBend a = BendPreBend a
mkBendRelease :: Empty -> ChxBend
mkBendRelease a = BendRelease a

-- credit: Choice
data ChxCredit = 
      CreditCreditImage {
          creditCreditImage :: Image
       }
    | CreditCreditWords {
          creditCreditWords :: FormattedText
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
mkCreditCreditImage :: Image -> ChxCredit
mkCreditCreditImage a = CreditCreditImage a
mkCreditCreditWords :: FormattedText -> ChxCredit
mkCreditCreditWords a = CreditCreditWords a []

-- direction-type: Choice
data ChxDirectionType = 
      DirectionTypeRehearsal {
          directionTypeRehearsal :: [Rehearsal]
       }
    | DirectionTypeSegno {
          directionTypeSegno :: [EmptyPrintStyle]
       }
    | DirectionTypeWords {
          directionTypeWords :: [FormattedText]
       }
    | DirectionTypeCoda {
          directionTypeCoda :: [EmptyPrintStyle]
       }
    | DirectionTypeWedge {
          directionTypeWedge :: Wedge
       }
    | DirectionTypeDynamics {
          directionTypeDynamics :: [Dynamics]
       }
    | DirectionTypeDashes {
          directionTypeDashes :: Dashes
       }
    | DirectionTypeBracket {
          directionTypeBracket :: Bracket
       }
    | DirectionTypePedal {
          directionTypePedal :: Pedal
       }
    | DirectionTypeMetronome {
          directionTypeMetronome :: Metronome
       }
    | DirectionTypeOctaveShift {
          directionTypeOctaveShift :: OctaveShift
       }
    | DirectionTypeHarpPedals {
          directionTypeHarpPedals :: HarpPedals
       }
    | DirectionTypeDamp {
          directionTypeDamp :: EmptyPrintStyle
       }
    | DirectionTypeDampAll {
          directionTypeDampAll :: EmptyPrintStyle
       }
    | DirectionTypeEyeglasses {
          directionTypeEyeglasses :: EmptyPrintStyle
       }
    | DirectionTypeScordatura {
          directionTypeScordatura :: Scordatura
       }
    | DirectionTypeImage {
          directionTypeImage :: Image
       }
    | DirectionTypeAccordionRegistration {
          directionTypeAccordionRegistration :: AccordionRegistration
       }
    | DirectionTypeOtherDirection {
          directionTypeOtherDirection :: OtherDirection
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
mkDirectionTypeRehearsal :: ChxDirectionType
mkDirectionTypeRehearsal = DirectionTypeRehearsal []
mkDirectionTypeSegno :: ChxDirectionType
mkDirectionTypeSegno = DirectionTypeSegno []
mkDirectionTypeWords :: ChxDirectionType
mkDirectionTypeWords = DirectionTypeWords []
mkDirectionTypeCoda :: ChxDirectionType
mkDirectionTypeCoda = DirectionTypeCoda []
mkDirectionTypeWedge :: Wedge -> ChxDirectionType
mkDirectionTypeWedge a = DirectionTypeWedge a
mkDirectionTypeDynamics :: ChxDirectionType
mkDirectionTypeDynamics = DirectionTypeDynamics []
mkDirectionTypeDashes :: Dashes -> ChxDirectionType
mkDirectionTypeDashes a = DirectionTypeDashes a
mkDirectionTypeBracket :: Bracket -> ChxDirectionType
mkDirectionTypeBracket a = DirectionTypeBracket a
mkDirectionTypePedal :: Pedal -> ChxDirectionType
mkDirectionTypePedal a = DirectionTypePedal a
mkDirectionTypeMetronome :: Metronome -> ChxDirectionType
mkDirectionTypeMetronome a = DirectionTypeMetronome a
mkDirectionTypeOctaveShift :: OctaveShift -> ChxDirectionType
mkDirectionTypeOctaveShift a = DirectionTypeOctaveShift a
mkDirectionTypeHarpPedals :: HarpPedals -> ChxDirectionType
mkDirectionTypeHarpPedals a = DirectionTypeHarpPedals a
mkDirectionTypeDamp :: EmptyPrintStyle -> ChxDirectionType
mkDirectionTypeDamp a = DirectionTypeDamp a
mkDirectionTypeDampAll :: EmptyPrintStyle -> ChxDirectionType
mkDirectionTypeDampAll a = DirectionTypeDampAll a
mkDirectionTypeEyeglasses :: EmptyPrintStyle -> ChxDirectionType
mkDirectionTypeEyeglasses a = DirectionTypeEyeglasses a
mkDirectionTypeScordatura :: Scordatura -> ChxDirectionType
mkDirectionTypeScordatura a = DirectionTypeScordatura a
mkDirectionTypeImage :: Image -> ChxDirectionType
mkDirectionTypeImage a = DirectionTypeImage a
mkDirectionTypeAccordionRegistration :: AccordionRegistration -> ChxDirectionType
mkDirectionTypeAccordionRegistration a = DirectionTypeAccordionRegistration a
mkDirectionTypeOtherDirection :: OtherDirection -> ChxDirectionType
mkDirectionTypeOtherDirection a = DirectionTypeOtherDirection a

-- dynamics: Choice
data ChxDynamics = 
      DynamicsP {
          dynamicsP :: Empty
       }
    | DynamicsPp {
          dynamicsPp :: Empty
       }
    | DynamicsPpp {
          dynamicsPpp :: Empty
       }
    | DynamicsPppp {
          dynamicsPppp :: Empty
       }
    | DynamicsPpppp {
          dynamicsPpppp :: Empty
       }
    | DynamicsPppppp {
          dynamicsPppppp :: Empty
       }
    | DynamicsF {
          dynamicsF :: Empty
       }
    | DynamicsFf {
          dynamicsFf :: Empty
       }
    | DynamicsFff {
          dynamicsFff :: Empty
       }
    | DynamicsFfff {
          dynamicsFfff :: Empty
       }
    | DynamicsFffff {
          dynamicsFffff :: Empty
       }
    | DynamicsFfffff {
          dynamicsFfffff :: Empty
       }
    | DynamicsMp {
          dynamicsMp :: Empty
       }
    | DynamicsMf {
          dynamicsMf :: Empty
       }
    | DynamicsSf {
          dynamicsSf :: Empty
       }
    | DynamicsSfp {
          dynamicsSfp :: Empty
       }
    | DynamicsSfpp {
          dynamicsSfpp :: Empty
       }
    | DynamicsFp {
          dynamicsFp :: Empty
       }
    | DynamicsRf {
          dynamicsRf :: Empty
       }
    | DynamicsRfz {
          dynamicsRfz :: Empty
       }
    | DynamicsSfz {
          dynamicsSfz :: Empty
       }
    | DynamicsSffz {
          dynamicsSffz :: Empty
       }
    | DynamicsFz {
          dynamicsFz :: Empty
       }
    | DynamicsOtherDynamics {
          dynamicsOtherDynamics :: String
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
mkDynamicsP :: Empty -> ChxDynamics
mkDynamicsP a = DynamicsP a
mkDynamicsPp :: Empty -> ChxDynamics
mkDynamicsPp a = DynamicsPp a
mkDynamicsPpp :: Empty -> ChxDynamics
mkDynamicsPpp a = DynamicsPpp a
mkDynamicsPppp :: Empty -> ChxDynamics
mkDynamicsPppp a = DynamicsPppp a
mkDynamicsPpppp :: Empty -> ChxDynamics
mkDynamicsPpppp a = DynamicsPpppp a
mkDynamicsPppppp :: Empty -> ChxDynamics
mkDynamicsPppppp a = DynamicsPppppp a
mkDynamicsF :: Empty -> ChxDynamics
mkDynamicsF a = DynamicsF a
mkDynamicsFf :: Empty -> ChxDynamics
mkDynamicsFf a = DynamicsFf a
mkDynamicsFff :: Empty -> ChxDynamics
mkDynamicsFff a = DynamicsFff a
mkDynamicsFfff :: Empty -> ChxDynamics
mkDynamicsFfff a = DynamicsFfff a
mkDynamicsFffff :: Empty -> ChxDynamics
mkDynamicsFffff a = DynamicsFffff a
mkDynamicsFfffff :: Empty -> ChxDynamics
mkDynamicsFfffff a = DynamicsFfffff a
mkDynamicsMp :: Empty -> ChxDynamics
mkDynamicsMp a = DynamicsMp a
mkDynamicsMf :: Empty -> ChxDynamics
mkDynamicsMf a = DynamicsMf a
mkDynamicsSf :: Empty -> ChxDynamics
mkDynamicsSf a = DynamicsSf a
mkDynamicsSfp :: Empty -> ChxDynamics
mkDynamicsSfp a = DynamicsSfp a
mkDynamicsSfpp :: Empty -> ChxDynamics
mkDynamicsSfpp a = DynamicsSfpp a
mkDynamicsFp :: Empty -> ChxDynamics
mkDynamicsFp a = DynamicsFp a
mkDynamicsRf :: Empty -> ChxDynamics
mkDynamicsRf a = DynamicsRf a
mkDynamicsRfz :: Empty -> ChxDynamics
mkDynamicsRfz a = DynamicsRfz a
mkDynamicsSfz :: Empty -> ChxDynamics
mkDynamicsSfz a = DynamicsSfz a
mkDynamicsSffz :: Empty -> ChxDynamics
mkDynamicsSffz a = DynamicsSffz a
mkDynamicsFz :: Empty -> ChxDynamics
mkDynamicsFz a = DynamicsFz a
mkDynamicsOtherDynamics :: String -> ChxDynamics
mkDynamicsOtherDynamics a = DynamicsOtherDynamics a

-- encoding: Choice
data ChxEncoding = 
      EncodingEncodingDate {
          encodingEncodingDate :: YyyyMmDd
       }
    | EncodingEncoder {
          encodingEncoder :: TypedText
       }
    | EncodingSoftware {
          encodingSoftware :: String
       }
    | EncodingEncodingDescription {
          encodingEncodingDescription :: String
       }
    | EncodingSupports {
          encodingSupports :: Supports
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
mkEncodingEncodingDate :: YyyyMmDd -> ChxEncoding
mkEncodingEncodingDate a = EncodingEncodingDate a
mkEncodingEncoder :: TypedText -> ChxEncoding
mkEncodingEncoder a = EncodingEncoder a
mkEncodingSoftware :: String -> ChxEncoding
mkEncodingSoftware a = EncodingSoftware a
mkEncodingEncodingDescription :: String -> ChxEncoding
mkEncodingEncodingDescription a = EncodingEncodingDescription a
mkEncodingSupports :: Supports -> ChxEncoding
mkEncodingSupports a = EncodingSupports a

-- full-note: Choice
data FullNote = 
      FullNotePitch {
          fullNotePitch :: Pitch
       }
    | FullNoteUnpitched {
          fullNoteUnpitched :: DisplayStepOctave
       }
    | FullNoteRest {
          fullNoteRest :: DisplayStepOctave
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
mkFullNotePitch :: Pitch -> FullNote
mkFullNotePitch a = FullNotePitch a
mkFullNoteUnpitched :: DisplayStepOctave -> FullNote
mkFullNoteUnpitched a = FullNoteUnpitched a
mkFullNoteRest :: DisplayStepOctave -> FullNote
mkFullNoteRest a = FullNoteRest a

-- harmonic: Choice
data ChxHarmonic = 
      HarmonicNatural {
          harmonicNatural :: Empty
       }
    | HarmonicArtificial {
          harmonicArtificial :: Empty
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
mkHarmonicNatural :: Empty -> ChxHarmonic
mkHarmonicNatural a = HarmonicNatural a
mkHarmonicArtificial :: Empty -> ChxHarmonic
mkHarmonicArtificial a = HarmonicArtificial a

-- harmonic: Choice,1
data ChxHarmonic1 = 
      HarmonicBasePitch {
          harmonicBasePitch :: Empty
       }
    | HarmonicTouchingPitch {
          harmonicTouchingPitch :: Empty
       }
    | HarmonicSoundingPitch {
          harmonicSoundingPitch :: Empty
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
mkHarmonicBasePitch :: Empty -> ChxHarmonic1
mkHarmonicBasePitch a = HarmonicBasePitch a
mkHarmonicTouchingPitch :: Empty -> ChxHarmonic1
mkHarmonicTouchingPitch a = HarmonicTouchingPitch a
mkHarmonicSoundingPitch :: Empty -> ChxHarmonic1
mkHarmonicSoundingPitch a = HarmonicSoundingPitch a

-- harmony-chord: Choice
data ChxHarmonyChord = 
      HarmonyChordRoot {
          harmonyChordRoot :: Root
       }
    | HarmonyChordFunction {
          harmonyChordFunction :: StyleText
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
mkHarmonyChordRoot :: Root -> ChxHarmonyChord
mkHarmonyChordRoot a = HarmonyChordRoot a
mkHarmonyChordFunction :: StyleText -> ChxHarmonyChord
mkHarmonyChordFunction a = HarmonyChordFunction a

-- key: Choice
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
mkKeyTraditionalKey :: TraditionalKey -> ChxKey
mkKeyTraditionalKey a = KeyTraditionalKey a
mkKeyNonTraditionalKey :: ChxKey
mkKeyNonTraditionalKey = KeyNonTraditionalKey []

-- lyric: Choice
data ChxLyric = 
      LyricSyllabic {
          lyricSyllabic :: (Maybe Syllabic)
        , lyricText :: TextElementData
        , chxlyricLyric :: [SeqLyric]
        , lyricExtend :: (Maybe Extend)
       }
    | LyricExtend {
          lyricExtend1 :: Extend
       }
    | LyricLaughing {
          lyricLaughing :: Empty
       }
    | LyricHumming {
          lyricHumming :: Empty
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
mkLyricSyllabic :: TextElementData -> ChxLyric
mkLyricSyllabic b = LyricSyllabic Nothing b [] Nothing
mkLyricExtend :: Extend -> ChxLyric
mkLyricExtend a = LyricExtend a
mkLyricLaughing :: Empty -> ChxLyric
mkLyricLaughing a = LyricLaughing a
mkLyricHumming :: Empty -> ChxLyric
mkLyricHumming a = LyricHumming a

-- measure-style: Choice
data ChxMeasureStyle = 
      MeasureStyleMultipleRest {
          measureStyleMultipleRest :: MultipleRest
       }
    | MeasureStyleMeasureRepeat {
          measureStyleMeasureRepeat :: MeasureRepeat
       }
    | MeasureStyleBeatRepeat {
          measureStyleBeatRepeat :: BeatRepeat
       }
    | MeasureStyleSlash {
          measureStyleSlash :: CmpSlash
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
mkMeasureStyleMultipleRest :: MultipleRest -> ChxMeasureStyle
mkMeasureStyleMultipleRest a = MeasureStyleMultipleRest a
mkMeasureStyleMeasureRepeat :: MeasureRepeat -> ChxMeasureStyle
mkMeasureStyleMeasureRepeat a = MeasureStyleMeasureRepeat a
mkMeasureStyleBeatRepeat :: BeatRepeat -> ChxMeasureStyle
mkMeasureStyleBeatRepeat a = MeasureStyleBeatRepeat a
mkMeasureStyleSlash :: CmpSlash -> ChxMeasureStyle
mkMeasureStyleSlash a = MeasureStyleSlash a

-- metronome: Choice
data ChxMetronome0 = 
      MetronomePerMinute {
          metronomePerMinute :: PerMinute
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
mkMetronomePerMinute :: PerMinute -> ChxMetronome0
mkMetronomePerMinute a = MetronomePerMinute a
mkMetronomeBeatUnit :: BeatUnit -> ChxMetronome0
mkMetronomeBeatUnit a = MetronomeBeatUnit a

-- metronome: Choice,1
data ChxMetronome = 
      ChxMetronomeBeatUnit {
          chxmetronomeBeatUnit :: BeatUnit
        , chxmetronomeMetronome :: ChxMetronome0
       }
    | MetronomeMetronomeNote {
          metronomeMetronomeNote :: [MetronomeNote]
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
mkChxMetronomeBeatUnit :: BeatUnit -> ChxMetronome0 -> ChxMetronome
mkChxMetronomeBeatUnit a b = ChxMetronomeBeatUnit a b
mkMetronomeMetronomeNote :: ChxMetronome
mkMetronomeMetronomeNote = MetronomeMetronomeNote [] Nothing

-- music-data: Choice
data ChxMusicData = 
      MusicDataNote {
          musicDataNote :: Note
       }
    | MusicDataBackup {
          musicDataBackup :: Backup
       }
    | MusicDataForward {
          musicDataForward :: Forward
       }
    | MusicDataDirection {
          musicDataDirection :: Direction
       }
    | MusicDataAttributes {
          musicDataAttributes :: Attributes
       }
    | MusicDataHarmony {
          musicDataHarmony :: Harmony
       }
    | MusicDataFiguredBass {
          musicDataFiguredBass :: FiguredBass
       }
    | MusicDataPrint {
          musicDataPrint :: Print
       }
    | MusicDataSound {
          musicDataSound :: Sound
       }
    | MusicDataBarline {
          musicDataBarline :: Barline
       }
    | MusicDataGrouping {
          musicDataGrouping :: Grouping
       }
    | MusicDataLink {
          musicDataLink :: Link
       }
    | MusicDataBookmark {
          musicDataBookmark :: Bookmark
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
mkMusicDataNote :: Note -> ChxMusicData
mkMusicDataNote a = MusicDataNote a
mkMusicDataBackup :: Backup -> ChxMusicData
mkMusicDataBackup a = MusicDataBackup a
mkMusicDataForward :: Forward -> ChxMusicData
mkMusicDataForward a = MusicDataForward a
mkMusicDataDirection :: Direction -> ChxMusicData
mkMusicDataDirection a = MusicDataDirection a
mkMusicDataAttributes :: Attributes -> ChxMusicData
mkMusicDataAttributes a = MusicDataAttributes a
mkMusicDataHarmony :: Harmony -> ChxMusicData
mkMusicDataHarmony a = MusicDataHarmony a
mkMusicDataFiguredBass :: FiguredBass -> ChxMusicData
mkMusicDataFiguredBass a = MusicDataFiguredBass a
mkMusicDataPrint :: Print -> ChxMusicData
mkMusicDataPrint a = MusicDataPrint a
mkMusicDataSound :: Sound -> ChxMusicData
mkMusicDataSound a = MusicDataSound a
mkMusicDataBarline :: Barline -> ChxMusicData
mkMusicDataBarline a = MusicDataBarline a
mkMusicDataGrouping :: Grouping -> ChxMusicData
mkMusicDataGrouping a = MusicDataGrouping a
mkMusicDataLink :: Link -> ChxMusicData
mkMusicDataLink a = MusicDataLink a
mkMusicDataBookmark :: Bookmark -> ChxMusicData
mkMusicDataBookmark a = MusicDataBookmark a

-- name-display: Choice
data ChxNameDisplay = 
      NameDisplayDisplayText {
          nameDisplayDisplayText :: FormattedText
       }
    | NameDisplayAccidentalText {
          nameDisplayAccidentalText :: AccidentalText
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
mkNameDisplayDisplayText :: FormattedText -> ChxNameDisplay
mkNameDisplayDisplayText a = NameDisplayDisplayText a
mkNameDisplayAccidentalText :: AccidentalText -> ChxNameDisplay
mkNameDisplayAccidentalText a = NameDisplayAccidentalText a

-- notations: Choice
data ChxNotations = 
      NotationsTied {
          notationsTied :: Tied
       }
    | NotationsSlur {
          notationsSlur :: Slur
       }
    | NotationsTuplet {
          notationsTuplet :: Tuplet
       }
    | NotationsGlissando {
          notationsGlissando :: Glissando
       }
    | NotationsSlide {
          notationsSlide :: Slide
       }
    | NotationsOrnaments {
          notationsOrnaments :: Ornaments
       }
    | NotationsTechnical {
          notationsTechnical :: Technical
       }
    | NotationsArticulations {
          notationsArticulations :: Articulations
       }
    | NotationsDynamics {
          notationsDynamics :: Dynamics
       }
    | NotationsFermata {
          notationsFermata :: Fermata
       }
    | NotationsArpeggiate {
          notationsArpeggiate :: Arpeggiate
       }
    | NotationsNonArpeggiate {
          notationsNonArpeggiate :: NonArpeggiate
       }
    | NotationsAccidentalMark {
          notationsAccidentalMark :: AccidentalMark
       }
    | NotationsOtherNotation {
          notationsOtherNotation :: OtherNotation
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
mkNotationsTied :: Tied -> ChxNotations
mkNotationsTied a = NotationsTied a
mkNotationsSlur :: Slur -> ChxNotations
mkNotationsSlur a = NotationsSlur a
mkNotationsTuplet :: Tuplet -> ChxNotations
mkNotationsTuplet a = NotationsTuplet a
mkNotationsGlissando :: Glissando -> ChxNotations
mkNotationsGlissando a = NotationsGlissando a
mkNotationsSlide :: Slide -> ChxNotations
mkNotationsSlide a = NotationsSlide a
mkNotationsOrnaments :: Ornaments -> ChxNotations
mkNotationsOrnaments a = NotationsOrnaments a
mkNotationsTechnical :: Technical -> ChxNotations
mkNotationsTechnical a = NotationsTechnical a
mkNotationsArticulations :: Articulations -> ChxNotations
mkNotationsArticulations a = NotationsArticulations a
mkNotationsDynamics :: Dynamics -> ChxNotations
mkNotationsDynamics a = NotationsDynamics a
mkNotationsFermata :: Fermata -> ChxNotations
mkNotationsFermata a = NotationsFermata a
mkNotationsArpeggiate :: Arpeggiate -> ChxNotations
mkNotationsArpeggiate a = NotationsArpeggiate a
mkNotationsNonArpeggiate :: NonArpeggiate -> ChxNotations
mkNotationsNonArpeggiate a = NotationsNonArpeggiate a
mkNotationsAccidentalMark :: AccidentalMark -> ChxNotations
mkNotationsAccidentalMark a = NotationsAccidentalMark a
mkNotationsOtherNotation :: OtherNotation -> ChxNotations
mkNotationsOtherNotation a = NotationsOtherNotation a

-- note: Choice
data ChxNote = 
      NoteGrace {
          noteGrace :: Grace
        , noteFullNote :: GrpFullNote
        , noteTie :: [Tie]
       }
    | NoteCue {
          noteCue :: Empty
        , noteFullNote1 :: GrpFullNote
        , noteDuration :: Duration
       }
    | NoteFullNote {
          noteFullNote2 :: GrpFullNote
        , noteDuration1 :: Duration
        , noteTie1 :: [Tie]
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
mkNoteGrace :: Grace -> GrpFullNote -> ChxNote
mkNoteGrace a b = NoteGrace a b []
mkNoteCue :: Empty -> GrpFullNote -> Duration -> ChxNote
mkNoteCue a b c = NoteCue a b c
mkNoteFullNote :: GrpFullNote -> Duration -> ChxNote
mkNoteFullNote a b = NoteFullNote a b []

-- ornaments: Choice
data ChxOrnaments = 
      OrnamentsTrillMark {
          ornamentsTrillMark :: EmptyTrillSound
       }
    | OrnamentsTurn {
          ornamentsTurn :: EmptyTrillSound
       }
    | OrnamentsDelayedTurn {
          ornamentsDelayedTurn :: EmptyTrillSound
       }
    | OrnamentsInvertedTurn {
          ornamentsInvertedTurn :: EmptyTrillSound
       }
    | OrnamentsShake {
          ornamentsShake :: EmptyTrillSound
       }
    | OrnamentsWavyLine {
          ornamentsWavyLine :: WavyLine
       }
    | OrnamentsMordent {
          ornamentsMordent :: Mordent
       }
    | OrnamentsInvertedMordent {
          ornamentsInvertedMordent :: Mordent
       }
    | OrnamentsSchleifer {
          ornamentsSchleifer :: EmptyPlacement
       }
    | OrnamentsTremolo {
          ornamentsTremolo :: Tremolo
       }
    | OrnamentsOtherOrnament {
          ornamentsOtherOrnament :: PlacementText
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
mkOrnamentsTrillMark :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsTrillMark a = OrnamentsTrillMark a
mkOrnamentsTurn :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsTurn a = OrnamentsTurn a
mkOrnamentsDelayedTurn :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsDelayedTurn a = OrnamentsDelayedTurn a
mkOrnamentsInvertedTurn :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsInvertedTurn a = OrnamentsInvertedTurn a
mkOrnamentsShake :: EmptyTrillSound -> ChxOrnaments
mkOrnamentsShake a = OrnamentsShake a
mkOrnamentsWavyLine :: WavyLine -> ChxOrnaments
mkOrnamentsWavyLine a = OrnamentsWavyLine a
mkOrnamentsMordent :: Mordent -> ChxOrnaments
mkOrnamentsMordent a = OrnamentsMordent a
mkOrnamentsInvertedMordent :: Mordent -> ChxOrnaments
mkOrnamentsInvertedMordent a = OrnamentsInvertedMordent a
mkOrnamentsSchleifer :: EmptyPlacement -> ChxOrnaments
mkOrnamentsSchleifer a = OrnamentsSchleifer a
mkOrnamentsTremolo :: Tremolo -> ChxOrnaments
mkOrnamentsTremolo a = OrnamentsTremolo a
mkOrnamentsOtherOrnament :: PlacementText -> ChxOrnaments
mkOrnamentsOtherOrnament a = OrnamentsOtherOrnament a

-- part-list: Choice
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
mkPartListPartGroup :: GrpPartGroup -> ChxPartList
mkPartListPartGroup a = PartListPartGroup a
mkPartListScorePart :: ScorePart -> ChxPartList
mkPartListScorePart a = PartListScorePart a

-- score-instrument: Choice
data ChxScoreInstrument = 
      ScoreInstrumentSolo {
          scoreInstrumentSolo :: Empty
       }
    | ScoreInstrumentEnsemble {
          scoreInstrumentEnsemble :: PositiveIntegerOrEmpty
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
mkScoreInstrumentSolo :: Empty -> ChxScoreInstrument
mkScoreInstrumentSolo a = ScoreInstrumentSolo a
mkScoreInstrumentEnsemble :: PositiveIntegerOrEmpty -> ChxScoreInstrument
mkScoreInstrumentEnsemble a = ScoreInstrumentEnsemble a

-- technical: Choice
data ChxTechnical = 
      TechnicalUpBow {
          technicalUpBow :: EmptyPlacement
       }
    | TechnicalDownBow {
          technicalDownBow :: EmptyPlacement
       }
    | TechnicalHarmonic {
          technicalHarmonic :: Harmonic
       }
    | TechnicalOpenString {
          technicalOpenString :: EmptyPlacement
       }
    | TechnicalThumbPosition {
          technicalThumbPosition :: EmptyPlacement
       }
    | TechnicalFingering {
          technicalFingering :: Fingering
       }
    | TechnicalPluck {
          technicalPluck :: PlacementText
       }
    | TechnicalDoubleTongue {
          technicalDoubleTongue :: EmptyPlacement
       }
    | TechnicalTripleTongue {
          technicalTripleTongue :: EmptyPlacement
       }
    | TechnicalStopped {
          technicalStopped :: EmptyPlacement
       }
    | TechnicalSnapPizzicato {
          technicalSnapPizzicato :: EmptyPlacement
       }
    | TechnicalFret {
          technicalFret :: Fret
       }
    | TechnicalString {
          technicalString :: CmpString
       }
    | TechnicalHammerOn {
          technicalHammerOn :: HammerOnPullOff
       }
    | TechnicalPullOff {
          technicalPullOff :: HammerOnPullOff
       }
    | TechnicalBend {
          technicalBend :: Bend
       }
    | TechnicalTap {
          technicalTap :: PlacementText
       }
    | TechnicalHeel {
          technicalHeel :: HeelToe
       }
    | TechnicalToe {
          technicalToe :: HeelToe
       }
    | TechnicalFingernails {
          technicalFingernails :: EmptyPlacement
       }
    | TechnicalOtherTechnical {
          technicalOtherTechnical :: PlacementText
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
mkTechnicalUpBow :: EmptyPlacement -> ChxTechnical
mkTechnicalUpBow a = TechnicalUpBow a
mkTechnicalDownBow :: EmptyPlacement -> ChxTechnical
mkTechnicalDownBow a = TechnicalDownBow a
mkTechnicalHarmonic :: Harmonic -> ChxTechnical
mkTechnicalHarmonic a = TechnicalHarmonic a
mkTechnicalOpenString :: EmptyPlacement -> ChxTechnical
mkTechnicalOpenString a = TechnicalOpenString a
mkTechnicalThumbPosition :: EmptyPlacement -> ChxTechnical
mkTechnicalThumbPosition a = TechnicalThumbPosition a
mkTechnicalFingering :: Fingering -> ChxTechnical
mkTechnicalFingering a = TechnicalFingering a
mkTechnicalPluck :: PlacementText -> ChxTechnical
mkTechnicalPluck a = TechnicalPluck a
mkTechnicalDoubleTongue :: EmptyPlacement -> ChxTechnical
mkTechnicalDoubleTongue a = TechnicalDoubleTongue a
mkTechnicalTripleTongue :: EmptyPlacement -> ChxTechnical
mkTechnicalTripleTongue a = TechnicalTripleTongue a
mkTechnicalStopped :: EmptyPlacement -> ChxTechnical
mkTechnicalStopped a = TechnicalStopped a
mkTechnicalSnapPizzicato :: EmptyPlacement -> ChxTechnical
mkTechnicalSnapPizzicato a = TechnicalSnapPizzicato a
mkTechnicalFret :: Fret -> ChxTechnical
mkTechnicalFret a = TechnicalFret a
mkTechnicalString :: CmpString -> ChxTechnical
mkTechnicalString a = TechnicalString a
mkTechnicalHammerOn :: HammerOnPullOff -> ChxTechnical
mkTechnicalHammerOn a = TechnicalHammerOn a
mkTechnicalPullOff :: HammerOnPullOff -> ChxTechnical
mkTechnicalPullOff a = TechnicalPullOff a
mkTechnicalBend :: Bend -> ChxTechnical
mkTechnicalBend a = TechnicalBend a
mkTechnicalTap :: PlacementText -> ChxTechnical
mkTechnicalTap a = TechnicalTap a
mkTechnicalHeel :: HeelToe -> ChxTechnical
mkTechnicalHeel a = TechnicalHeel a
mkTechnicalToe :: HeelToe -> ChxTechnical
mkTechnicalToe a = TechnicalToe a
mkTechnicalFingernails :: EmptyPlacement -> ChxTechnical
mkTechnicalFingernails a = TechnicalFingernails a
mkTechnicalOtherTechnical :: PlacementText -> ChxTechnical
mkTechnicalOtherTechnical a = TechnicalOtherTechnical a

-- time: Choice
data ChxTime = 
      TimeTime {
          chxtimeTime :: [SeqTime]
       }
    | TimeSenzaMisura {
          timeSenzaMisura :: Empty
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ChxTime where
    emitXml (TimeTime a) =
      XReps [emitXml a]
    emitXml (TimeSenzaMisura a) =
      XContent XEmpty
        []
        ([XElement (QN "senza-misura" Nothing) (emitXml a)])
mkTimeTime :: ChxTime
mkTimeTime = TimeTime []
mkTimeSenzaMisura :: Empty -> ChxTime
mkTimeSenzaMisura a = TimeSenzaMisura a

-- credit: Sequence
data SeqCredit = 
      SeqCredit {
          seqcreditLink :: [Link]
        , seqcreditBookmark :: [Bookmark]
        , seqcreditCreditWords :: FormattedText
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqCredit where
    emitXml (SeqCredit a b c) =
      XContent XEmpty
        []
        (map (XElement (QN "link" Nothing).emitXml) a++map (XElement (QN "bookmark" Nothing).emitXml) b++[XElement (QN "credit-words" Nothing) (emitXml c)])
mkSeqCredit :: FormattedText -> SeqCredit
mkSeqCredit c = SeqCredit [] [] c

-- display-step-octave: Sequence
data SeqDisplayStepOctave = 
      SeqDisplayStepOctave {
          displayStepOctaveDisplayStep :: Step
        , displayStepOctaveDisplayOctave :: Octave
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqDisplayStepOctave where
    emitXml (SeqDisplayStepOctave a b) =
      XContent XEmpty
        []
        ([XElement (QN "display-step" Nothing) (emitXml a)]++[XElement (QN "display-octave" Nothing) (emitXml b)])
mkSeqDisplayStepOctave :: Step -> Octave -> SeqDisplayStepOctave
mkSeqDisplayStepOctave a b = SeqDisplayStepOctave a b

-- lyric: Sequence
data SeqLyric0 = 
      SeqLyric0 {
          lyricElision :: Elision
        , seqlyricSyllabic :: (Maybe Syllabic)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqLyric0 where
    emitXml (SeqLyric0 a b) =
      XContent XEmpty
        []
        ([XElement (QN "elision" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "syllabic" Nothing).emitXml) b])
mkSeqLyric0 :: Elision -> SeqLyric0
mkSeqLyric0 a = SeqLyric0 a Nothing

-- lyric: Sequence,1
data SeqLyric = 
      SeqLyric {
          seqlyricLyric :: (Maybe SeqLyric0)
        , seqlyricText :: TextElementData
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqLyric where
    emitXml (SeqLyric a b) =
      XContent XEmpty
        []
        ([emitXml a]++[XElement (QN "text" Nothing) (emitXml b)])
mkSeqLyric :: TextElementData -> SeqLyric
mkSeqLyric b = SeqLyric Nothing b

-- metronome: Sequence
data SeqMetronome = 
      SeqMetronome {
          metronomeMetronomeRelation :: String
        , seqmetronomeMetronomeNote :: [MetronomeNote]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqMetronome where
    emitXml (SeqMetronome a b) =
      XContent XEmpty
        []
        ([XElement (QN "metronome-relation" Nothing) (emitXml a)]++map (XElement (QN "metronome-note" Nothing).emitXml) b)
mkSeqMetronome :: String -> SeqMetronome
mkSeqMetronome a = SeqMetronome a []

-- metronome-tuplet: Sequence
data SeqMetronomeTuplet = 
      SeqMetronomeTuplet {
          metronomeTupletNormalType :: NoteTypeValue
        , metronomeTupletNormalDot :: [Empty]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqMetronomeTuplet where
    emitXml (SeqMetronomeTuplet a b) =
      XContent XEmpty
        []
        ([XElement (QN "normal-type" Nothing) (emitXml a)]++map (XElement (QN "normal-dot" Nothing).emitXml) b)
mkSeqMetronomeTuplet :: NoteTypeValue -> SeqMetronomeTuplet
mkSeqMetronomeTuplet a = SeqMetronomeTuplet a []

-- ornaments: Sequence
data SeqOrnaments = 
      SeqOrnaments {
          seqornamentsOrnaments :: ChxOrnaments
        , ornamentsAccidentalMark :: [AccidentalMark]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqOrnaments where
    emitXml (SeqOrnaments a b) =
      XContent XEmpty
        []
        ([emitXml a]++map (XElement (QN "accidental-mark" Nothing).emitXml) b)
mkSeqOrnaments :: ChxOrnaments -> SeqOrnaments
mkSeqOrnaments a = SeqOrnaments a []

-- page-layout: Sequence
data SeqPageLayout = 
      SeqPageLayout {
          pageLayoutPageHeight :: Tenths
        , pageLayoutPageWidth :: Tenths
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqPageLayout where
    emitXml (SeqPageLayout a b) =
      XContent XEmpty
        []
        ([XElement (QN "page-height" Nothing) (emitXml a)]++[XElement (QN "page-width" Nothing) (emitXml b)])
mkSeqPageLayout :: Tenths -> Tenths -> SeqPageLayout
mkSeqPageLayout a b = SeqPageLayout a b

-- time: Sequence
data SeqTime = 
      SeqTime {
          timeBeats :: String
        , timeBeatType :: String
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqTime where
    emitXml (SeqTime a b) =
      XContent XEmpty
        []
        ([XElement (QN "beats" Nothing) (emitXml a)]++[XElement (QN "beat-type" Nothing) (emitXml b)])
mkSeqTime :: String -> String -> SeqTime
mkSeqTime a b = SeqTime a b

-- time-modification: Sequence
data SeqTimeModification = 
      SeqTimeModification {
          timeModificationNormalType :: NoteTypeValue
        , timeModificationNormalDot :: [Empty]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml SeqTimeModification where
    emitXml (SeqTimeModification a b) =
      XContent XEmpty
        []
        ([XElement (QN "normal-type" Nothing) (emitXml a)]++map (XElement (QN "normal-dot" Nothing).emitXml) b)
mkSeqTimeModification :: NoteTypeValue -> SeqTimeModification
mkSeqTimeModification a = SeqTimeModification a []

-- all-margins: Group
data AllMargins = 
      AllMargins {
          allMarginsLeftRightMargins :: LeftRightMargins
        , allMarginsTopMargin :: Tenths
        , allMarginsBottomMargin :: Tenths
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml AllMargins where
    emitXml (AllMargins a b c) =
      XContent XEmpty
        []
        ([emitXml a]++[XElement (QN "top-margin" Nothing) (emitXml b)]++[XElement (QN "bottom-margin" Nothing) (emitXml c)])
mkAllMargins :: LeftRightMargins -> Tenths -> Tenths -> AllMargins
mkAllMargins a b c = AllMargins a b c

-- beat-unit: Group
data BeatUnit = 
      BeatUnit {
          beatUnitBeatUnit :: NoteTypeValue
        , beatUnitBeatUnitDot :: [Empty]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml BeatUnit where
    emitXml (BeatUnit a b) =
      XContent XEmpty
        []
        ([XElement (QN "beat-unit" Nothing) (emitXml a)]++map (XElement (QN "beat-unit-dot" Nothing).emitXml) b)
mkBeatUnit :: NoteTypeValue -> BeatUnit
mkBeatUnit a = BeatUnit a []

-- duration: Group
data Duration = 
      Duration {
          durationDuration :: PositiveDivisions
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Duration where
    emitXml (Duration a) =
      XContent XEmpty
        []
        ([XElement (QN "duration" Nothing) (emitXml a)])
mkDuration :: PositiveDivisions -> Duration
mkDuration a = Duration a

-- editorial: Group
data Editorial = 
      Editorial {
          editorialFootnote :: (Maybe Footnote)
        , editorialLevel :: (Maybe GrpLevel)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Editorial where
    emitXml (Editorial a b) =
      XReps [emitXml a,emitXml b]
mkEditorial :: Editorial
mkEditorial = Editorial Nothing Nothing

-- editorial-voice: Group
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
mkEditorialVoice :: EditorialVoice
mkEditorialVoice = EditorialVoice Nothing Nothing Nothing

-- editorial-voice-direction: Group
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
mkEditorialVoiceDirection :: EditorialVoiceDirection
mkEditorialVoiceDirection = EditorialVoiceDirection Nothing Nothing Nothing

-- footnote: Group
data Footnote = 
      Footnote {
          footnoteFootnote :: FormattedText
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Footnote where
    emitXml (Footnote a) =
      XContent XEmpty
        []
        ([XElement (QN "footnote" Nothing) (emitXml a)])
mkFootnote :: FormattedText -> Footnote
mkFootnote a = Footnote a

-- full-note: Group
data GrpFullNote = 
      GrpFullNote {
          fullNoteChord :: (Maybe Empty)
        , fullNoteFullNote :: FullNote
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GrpFullNote where
    emitXml (GrpFullNote a b) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "chord" Nothing).emitXml) a]++[emitXml b])
mkGrpFullNote :: FullNote -> GrpFullNote
mkGrpFullNote b = GrpFullNote Nothing b

-- harmony-chord: Group
data HarmonyChord = 
      HarmonyChord {
          harmonyChordHarmonyChord :: ChxHarmonyChord
        , harmonyChordKind :: Kind
        , harmonyChordInversion :: (Maybe Inversion)
        , harmonyChordBass :: (Maybe Bass)
        , harmonyChordDegree :: [Degree]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml HarmonyChord where
    emitXml (HarmonyChord a b c d e) =
      XContent XEmpty
        []
        ([emitXml a]++[XElement (QN "kind" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "inversion" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "bass" Nothing).emitXml) d]++map (XElement (QN "degree" Nothing).emitXml) e)
mkHarmonyChord :: ChxHarmonyChord -> Kind -> HarmonyChord
mkHarmonyChord a b = HarmonyChord a b Nothing Nothing []

-- layout: Group
data Layout = 
      Layout {
          layoutPageLayout :: (Maybe PageLayout)
        , layoutSystemLayout :: (Maybe SystemLayout)
        , layoutStaffLayout :: [StaffLayout]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Layout where
    emitXml (Layout a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "page-layout" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "system-layout" Nothing).emitXml) b]++map (XElement (QN "staff-layout" Nothing).emitXml) c)
mkLayout :: Layout
mkLayout = Layout Nothing Nothing []

-- left-right-margins: Group
data LeftRightMargins = 
      LeftRightMargins {
          leftRightMarginsLeftMargin :: Tenths
        , leftRightMarginsRightMargin :: Tenths
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml LeftRightMargins where
    emitXml (LeftRightMargins a b) =
      XContent XEmpty
        []
        ([XElement (QN "left-margin" Nothing) (emitXml a)]++[XElement (QN "right-margin" Nothing) (emitXml b)])
mkLeftRightMargins :: Tenths -> Tenths -> LeftRightMargins
mkLeftRightMargins a b = LeftRightMargins a b

-- level: Group
data GrpLevel = 
      GrpLevel {
          levelLevel :: Level
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GrpLevel where
    emitXml (GrpLevel a) =
      XContent XEmpty
        []
        ([XElement (QN "level" Nothing) (emitXml a)])
mkGrpLevel :: Level -> GrpLevel
mkGrpLevel a = GrpLevel a

-- music-data: Group
data MusicData = 
      MusicData {
          musicDataMusicData :: [ChxMusicData]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml MusicData where
    emitXml (MusicData a) =
      XReps [emitXml a]
mkMusicData :: MusicData
mkMusicData = MusicData []

-- non-traditional-key: Group
data NonTraditionalKey = 
      NonTraditionalKey {
          nonTraditionalKeyKeyStep :: Step
        , nonTraditionalKeyKeyAlter :: Semitones
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml NonTraditionalKey where
    emitXml (NonTraditionalKey a b) =
      XContent XEmpty
        []
        ([XElement (QN "key-step" Nothing) (emitXml a)]++[XElement (QN "key-alter" Nothing) (emitXml b)])
mkNonTraditionalKey :: Step -> Semitones -> NonTraditionalKey
mkNonTraditionalKey a b = NonTraditionalKey a b

-- part-group: Group
data GrpPartGroup = 
      GrpPartGroup {
          partGroupPartGroup :: PartGroup
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml GrpPartGroup where
    emitXml (GrpPartGroup a) =
      XContent XEmpty
        []
        ([XElement (QN "part-group" Nothing) (emitXml a)])
mkGrpPartGroup :: PartGroup -> GrpPartGroup
mkGrpPartGroup a = GrpPartGroup a

-- score-header: Group
data ScoreHeader = 
      ScoreHeader {
          scoreHeaderWork :: (Maybe Work)
        , scoreHeaderMovementNumber :: (Maybe String)
        , scoreHeaderMovementTitle :: (Maybe String)
        , scoreHeaderIdentification :: (Maybe Identification)
        , scoreHeaderDefaults :: (Maybe Defaults)
        , scoreHeaderCredit :: [Credit]
        , scoreHeaderPartList :: PartList
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScoreHeader where
    emitXml (ScoreHeader a b c d e f g) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "work" Nothing).emitXml) a]++[maybe XEmpty (XElement (QN "movement-number" Nothing).emitXml) b]++[maybe XEmpty (XElement (QN "movement-title" Nothing).emitXml) c]++[maybe XEmpty (XElement (QN "identification" Nothing).emitXml) d]++[maybe XEmpty (XElement (QN "defaults" Nothing).emitXml) e]++map (XElement (QN "credit" Nothing).emitXml) f++[XElement (QN "part-list" Nothing) (emitXml g)])
mkScoreHeader :: PartList -> ScoreHeader
mkScoreHeader g = ScoreHeader Nothing Nothing Nothing Nothing Nothing [] g

-- score-part: Group
data ScorePart = 
      ScorePart {
          scorePartScorePart :: CmpScorePart
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml ScorePart where
    emitXml (ScorePart a) =
      XContent XEmpty
        []
        ([XElement (QN "score-part" Nothing) (emitXml a)])
mkScorePart :: CmpScorePart -> ScorePart
mkScorePart a = ScorePart a

-- slash: Group
data Slash = 
      Slash {
          slashSlashType :: NoteTypeValue
        , slashSlashDot :: [Empty]
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Slash where
    emitXml (Slash a b) =
      XContent XEmpty
        []
        ([XElement (QN "slash-type" Nothing) (emitXml a)]++map (XElement (QN "slash-dot" Nothing).emitXml) b)
mkSlash :: NoteTypeValue -> Slash
mkSlash a = Slash a []

-- staff: Group
data Staff = 
      Staff {
          staffStaff :: PositiveInteger
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Staff where
    emitXml (Staff a) =
      XContent XEmpty
        []
        ([XElement (QN "staff" Nothing) (emitXml a)])
mkStaff :: PositiveInteger -> Staff
mkStaff a = Staff a

-- traditional-key: Group
data TraditionalKey = 
      TraditionalKey {
          traditionalKeyCancel :: (Maybe Cancel)
        , traditionalKeyFifths :: Fifths
        , traditionalKeyMode :: (Maybe Mode)
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml TraditionalKey where
    emitXml (TraditionalKey a b c) =
      XContent XEmpty
        []
        ([maybe XEmpty (XElement (QN "cancel" Nothing).emitXml) a]++[XElement (QN "fifths" Nothing) (emitXml b)]++[maybe XEmpty (XElement (QN "mode" Nothing).emitXml) c])
mkTraditionalKey :: Fifths -> TraditionalKey
mkTraditionalKey b = TraditionalKey Nothing b Nothing

-- tuning: Group
data Tuning = 
      Tuning {
          tuningTuningStep :: Step
        , tuningTuningAlter :: (Maybe Semitones)
        , tuningTuningOctave :: Octave
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Tuning where
    emitXml (Tuning a b c) =
      XContent XEmpty
        []
        ([XElement (QN "tuning-step" Nothing) (emitXml a)]++[maybe XEmpty (XElement (QN "tuning-alter" Nothing).emitXml) b]++[XElement (QN "tuning-octave" Nothing) (emitXml c)])
mkTuning :: Step -> Octave -> Tuning
mkTuning a c = Tuning a Nothing c

-- voice: Group
data Voice = 
      Voice {
          voiceVoice :: String
       }
    deriving (Eq,Typeable,Generic,Show)
instance EmitXml Voice where
    emitXml (Voice a) =
      XContent XEmpty
        []
        ([XElement (QN "voice" Nothing) (emitXml a)])
mkVoice :: String -> Voice
mkVoice a = Voice a
