{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | Emit an xml-like AST with renderers to String and 'Element'.
-- Intended for use with generated code.
module Fadno.Xml.EmitXml
    (
     -- * Rendering functions
     renderString
    ,renderElement
     -- * API types
    ,EmitXml (..)
    ,XmlRep (..)
    ,QN (..)
    ) where

import Data.Decimal
import Text.XML.Light
import Data.Maybe

-- | QName type.
data QN = QN { qLocal :: String, qPrefix :: Maybe String }
instance Show QN where
    show (QN l Nothing) = l
    show (QN l (Just p)) = p ++ ':':l



-- | XML AST.
data XmlRep where
    XEmpty :: XmlRep
    XLit :: String -> XmlRep
    XShow :: Show a => a -> XmlRep
    XElement :: QN -> XmlRep -> XmlRep
    XAttr :: QN -> XmlRep -> XmlRep
    XText :: XmlRep -> XmlRep
    XContent :: { xtext :: XmlRep,
                  xattrs :: [XmlRep],
                  xels :: [XmlRep] } -> XmlRep
    XReps :: [XmlRep] -> XmlRep

deriving instance Show XmlRep

-- | Emit AST.
class EmitXml a where
    emitXml :: a -> XmlRep


instance EmitXml a => EmitXml (Maybe a) where
    emitXml Nothing = XEmpty
    emitXml (Just a) = emitXml a

instance {-# OVERLAPPING #-} EmitXml String where
    emitXml = XLit

instance {-# OVERLAPPABLE #-} EmitXml a => EmitXml [a] where
    emitXml = XReps . map emitXml

instance EmitXml Int where emitXml = XShow
instance EmitXml Decimal where emitXml = XShow
instance EmitXml Float where emitXml = XShow
instance EmitXml Double where emitXml = XShow
instance EmitXml Bool where emitXml = XShow


-- | render AST to String.
renderString :: XmlRep -> String
renderString (XElement name rep) =
    "<" ++ show name ++ attrs rep ++ ">" ++ text rep ++ els rep ++ "</" ++ show name ++ ">"
    where
      attrs (XContent _ as _) = concatMap attr as
      attrs (XReps rs) = concatMap attrs rs
      attrs _ = ""
      attr (XAttr n r) = " " ++ show n ++ "=\"" ++ renderString r ++ "\""
      attr XEmpty = ""
      attr r = error $ "renderString.attr: invalid production: " ++ show r
      text (XContent t _ _) = renderString t
      text (XReps rs) = concatMap text rs
      text XEmpty = ""
      text r = renderString r
      els (XContent _ _ es) = concatMap renderString es
      els (XReps rs) = concatMap els rs
      els XEmpty = ""
      els _ = ""
renderString (XReps rs) = concatMap renderString rs
renderString XEmpty = ""
renderString (XLit s) = s
renderString (XShow a) = show a
renderString (XContent XEmpty [] els) = concatMap renderString els
renderString r = error $ "renderString: invalid production: " ++ show r

-- | render AST to Element.
renderElement :: XmlRep -> Element
renderElement (XElement en rep) =
    Element (qn en)  (attrs rep) (text rep ++ els rep) Nothing
    where
      qn (QN l p) = QName l Nothing p
      attrs (XContent _ as _) = concatMap attr as
      attrs (XReps rs) = concatMap attrs rs
      attrs _ = []
      attr (XAttr n r) = [Attr (qn n) (fromMaybe "" (str r))]
      attr XEmpty = []
      attr r = error $ "renderElement.attr: invalid production: " ++ show r
      textmay = maybe [] (\s -> [Text (CData CDataRaw s Nothing)]) . str
      text (XContent t _ _) = textmay t
      text (XReps r) = concatMap text r
      text XEmpty = []
      text r = textmay r
      els (XContent _ _ es) = concatMap els es
      els e@(XElement {}) = [Elem $ renderElement e]
      els (XReps rs) = concatMap els rs
      els XEmpty = []
      els _ = []
      str (XReps rs) = case catMaybes (map str rs) of
                         [] -> Nothing
                         ss -> Just $ concat ss
      str XEmpty = Nothing
      str (XLit s) = Just s
      str (XShow a) = Just (show a)
      str (XContent XEmpty [] es) = str (XReps es)
      str r = error $ "renderElement.str: invalid production: " ++ show r
renderElement r = error $ "renderElement: invalid production: " ++ show r
