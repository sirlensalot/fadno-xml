{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Parse an XSD into types with the ability to resolve references.
-- | All XSD type fields named with underscores and have lenses.
module Fadno.Xml.ParseXsd
    (
     -- * Parsers and utilities
     Schema (..),simpleTypes,complexTypes,groups,attributeGroups,elements,attributes
    ,QN (..),qLocal,qPrefix
    ,parseFile, loadXsdSchema, schemaParser, namespaceSchema
    ,qnParser, attrParser, parsec, qn, anySimpleTypeName
     -- * Type References
    ,Ref (..),unresolved,resolved,refvalue
    ,Resolvable (..)
    ,refResolve
    -- * Productions
    ,SimpleType(..),simpleTypeName,simpleTypeRestriction,simpleTypeUnion
    ,Bound(..)
    ,SimpleRestriction(..),simpleRestrictBase,simpleRestrictEnums,simpleRestrictMin,simpleRestrictMax,simpleRestrictPattern
    ,Union(..),unionMemberTypes,unionSimpleTypes
    ,Attribute(..),attrName,attrType,attrUse,attrDefault,attrRef,attrSimpleType
    ,Use(..)
    ,AttributeGroup(..),attrGroupName,attrGroupAttributes,attrGroupRef
    ,Attributes(..),attrsAttributes,attrsAttributeGroups
    ,Occurs(..),occursMin,occursMax
    ,Element(..),elementName,elementType,elementOccurs,elementSimple,elementComplex,elementRef
    ,ComplexType(..),complexTypeName,complexSimpleContent,complexComplexContent,complexCompositor,complexAttributes
    ,SimpleContent(..),simpleContentBase,simpleContentAttributes
    ,ComplexContent(..),complexContentBase,complexContentAttributes,complexContentCompositor
    ,Compositor(..),compGroup,compChoice,compSequence
    ,Group(..),groupName,groupOccurs,groupChoice,groupSequence,groupRef
    ,Particle(..),partElement,partGroup,partChoice,partSequence
    ,Choice(..),choiceOccurs,choiceParticles
    ,Sequence(..),sequenceOccurs,sequenceParticles

    ) where

import Control.Monad.State.Strict hiding (sequence)
import Control.Monad.Except hiding (sequence)
import Data.Either
import Control.Applicative
import Prelude hiding (sequence)
import Fadno.Xml.XParser
import Control.Lens hiding (Choice,element,elements)
import Data.Data.Lens
import Data.Data
import qualified Text.Parsec as P
import Data.Monoid
import Control.Exception
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe


-- | Model an outward XSD reference.
data Ref a =
    -- | Just type name.
    Unresolved { _unresolved :: !QN } |
    -- | Type name and resolved value.
    Resolved { _resolved :: !QN,
               _refvalue ::  !a } |
    -- | Reserved for built-in types (string, etc)
    Final
    deriving (Data,Typeable,Eq)

instance Show (Ref a) where
    show (Unresolved a) = "Unresolved " ++ show a
    show (Resolved n _) = "Resolved " ++ show n -- avoid circular stuff
    show Final = "Final"

-- | QName type.
data QN = QN { _qLocal :: String, _qPrefix :: Maybe String }
        deriving (Data,Typeable,Eq,Ord)
instance Show QN
    where show (QN l Nothing) = l
          show (QN l (Just p)) = p ++ ':':l

-- | XSD simpleType production.
data SimpleType =
    SimpleTypeRestrict {
      _simpleTypeName :: !(Maybe QN),
      _simpleTypeRestriction :: !SimpleRestriction } |
    SimpleTypeUnion {
      _simpleTypeName :: !(Maybe QN),
      _simpleTypeUnion :: !Union }
    deriving (Data,Typeable,Eq,Show)

-- | Model min/max restrictions.
data Bound a = Inclusive a | Exclusive a deriving (Data,Typeable,Eq,Show,Functor,Ord)

-- | simple type restriction production.
data SimpleRestriction =
    SimpleRestriction {
      _simpleRestrictBase :: !(Ref SimpleType)
    , _simpleRestrictEnums :: ![String]
    , _simpleRestrictMin :: !(Maybe (Bound String))
    , _simpleRestrictMax :: !(Maybe (Bound String))
    , _simpleRestrictPattern :: !(Maybe String) }
    deriving (Data,Typeable,Eq,Show)


-- | Simple type union production.
data Union =
    Union {
      _unionMemberTypes :: ![Ref SimpleType]
    , _unionSimpleTypes :: ![SimpleType] }
    deriving (Data,Typeable,Eq,Show)

-- | XSD attribute production.
data Attribute =
    AttributeType {
      _attrName :: !QN,
      _attrType :: !(Ref SimpleType),
      _attrUse :: !Use,
      _attrDefault :: !(Maybe String) } |
    AttributeRef {
      _attrRef :: !(Ref Attribute),
      _attrUse :: !Use,
      _attrDefault :: !(Maybe String) } |
    AttributeSimpleType {
      _attrName :: !QN,
      _attrSimpleType :: SimpleType
    }
    deriving (Data,Typeable,Eq,Show)

-- | XSD "use" values.
data Use = Required | Optional | Prohibited deriving (Data,Typeable,Eq,Show)

-- | XSD attribute-group production.
data AttributeGroup =
    AttributeGroup {
      _attrGroupName :: !QN
    , _attrGroupAttributes :: !Attributes } |
    AttributeGroupRef {
      _attrGroupRef :: !(Ref AttributeGroup)
    }
    deriving (Data,Typeable,Eq,Show)


-- | Convenience grouping of attributes and attribute groups, which
-- | are always showing up together in xsd.
data Attributes =
    Attributes {
      _attrsAttributes :: ![Attribute],
      _attrsAttributeGroups :: ![AttributeGroup]
    } deriving (Data,Typeable,Eq,Show)

-- | "occurs-min" and "occurs-max"
data Occurs =
    Occurs {
      _occursMin :: !(Maybe String)
    , _occursMax :: !(Maybe String)
} deriving (Data,Typeable,Eq,Show)

-- | XSD element production.
data Element =
    ElementType {
      _elementName :: !QN
    , _elementType :: !(Ref (Either ComplexType SimpleType))
    , _elementOccurs :: !Occurs } |
    ElementSimple {
      _elementName :: !QN
    , _elementSimple :: !SimpleType
    , _elementOccurs :: !Occurs } |
    ElementComplex {
      _elementName :: !QN
    , _elementComplex :: !ComplexType
    , _elementOccurs :: !Occurs } |
    ElementRef {
      _elementRef :: !(Ref Element)
    , _elementOccurs :: !Occurs }
    deriving (Data,Typeable,Eq,Show)


-- | XSD complexType production.
data ComplexType =
    ComplexTypeSimple {
      _complexTypeName :: !(Maybe QN)
    , _complexSimpleContent :: !SimpleContent } |
    ComplexTypeComplex {
      _complexTypeName :: !(Maybe QN)
    , _complexComplexContent :: !ComplexContent } |
    ComplexTypeCompositor {
      _complexTypeName :: !(Maybe QN)
    , _complexCompositor :: !(Maybe Compositor)
    , _complexAttributes :: !Attributes }
    deriving (Data,Typeable,Eq,Show)

-- | simpleContent under a complex type.
data SimpleContent =
    SimpleContentExtension {
      _simpleContentBase :: !(Ref SimpleType)
    , _simpleContentAttributes :: !Attributes
    }
   deriving (Data,Typeable,Eq,Show)


-- | complexContent under a complex type.
-- | TODO: restrictions
data ComplexContent =
    ComplexContentExtension {
      _complexContentBase :: !(Ref ComplexType)
    , _complexContentAttributes :: !Attributes
    , _complexContentCompositor :: Maybe Compositor
    } deriving (Data,Typeable,Eq,Show)

-- | Compositors.
data Compositor =
    CompositorGroup { _compGroup :: !Group } |
    CompositorChoice { _compChoice :: !Choice } |
    CompositorSequence { _compSequence :: !Sequence }
    deriving (Data,Typeable,Eq,Show)

-- | XSD "group" production.
data Group =
    GroupChoice {
      _groupName :: !(Maybe QN),
      _groupOccurs :: !Occurs,
      _groupChoice :: !Choice } |
    GroupSequence {
      _groupName :: !(Maybe QN),
      _groupOccurs :: !Occurs,
      _groupSequence :: !Sequence } |
    GroupRef {
      _groupRef :: !(Ref Group),
      _groupOccurs :: !Occurs
    } deriving (Data,Typeable,Eq,Show)

-- | Particles.
data Particle =
    PartElement { _partElement :: !Element } |
    PartGroup { _partGroup :: !Group } |
    PartChoice { _partChoice :: !Choice } |
    PartSequence { _partSequence :: !Sequence }
    deriving (Data,Typeable,Eq,Show)


-- | XSD choice
data Choice =
    Choice {
      _choiceOccurs :: !Occurs
    , _choiceParticles :: ![Particle] }
    deriving (Data,Typeable,Eq,Show)

-- | XSD sequence.
data Sequence =
    Sequence {
      _sequenceOccurs :: !Occurs
    , _sequenceParticles :: ![Particle] }
    deriving (Data,Typeable,Eq,Show)

-- | Schema type, mapping top-level productions to qnames.
data Schema =
    Schema {
      _simpleTypes :: !(Map QN SimpleType)
    , _complexTypes :: !(Map QN ComplexType)
    , _groups :: !(Map QN Group)
    , _attributeGroups :: !(Map QN AttributeGroup)
    , _elements :: !(Map QN Element)
    , _attributes :: !(Map QN Attribute)
    } deriving (Data,Typeable,Eq)
instance Show Schema where
    show (Schema sts cts gs ags es as) =
        "Schema { simpleTypes = " ++ show (length sts) ++
                     ", complexTypes = " ++ show (length cts) ++
                     ", groups = " ++ show (length gs) ++
                     ", attributeGroups = " ++ show (length ags) ++
                     ", elements = " ++ show (length es) ++
                     ", attributes = " ++ show (length as) ++
                     "}"

instance Monoid Schema where
    mempty = Schema mempty mempty mempty mempty mempty mempty
    (Schema a b c d e f) `mappend` (Schema g h i j k l) =
        Schema (a<>g) (b<>h) (c<>i) (d<>j) (e<>k) (f<>l)




-- Wow, really wish I didn't have to manually export all of these lenses.
-- makeClassy has a workaround but then I get naming conflicts ......

$(makeLenses ''QN)
$(makeLenses ''Ref)
$(makeLenses ''SimpleType)
$(makeLenses ''Bound)
$(makeLenses ''SimpleRestriction)
$(makeLenses ''Union)
$(makeLenses ''Attribute)
$(makeLenses ''Use)
$(makeLenses ''AttributeGroup)
$(makeLenses ''Attributes)
$(makeLenses ''Occurs)
$(makeLenses ''Element)
$(makeLenses ''ComplexType)
$(makeLenses ''SimpleContent)
$(makeLenses ''ComplexContent)
$(makeLenses ''Compositor)
$(makeLenses ''Group)
$(makeLenses ''Particle)
$(makeLenses ''Choice)
$(makeLenses ''Sequence)
$(makeLenses ''Schema)

--
-- Resolvable
--

-- | Resolvable indicates a type has a 'Ref' member that it can
-- | resolve from a top-level 'Schema' production.
class (Typeable a) => Resolvable a where
    resolve :: Schema -> a -> a

instance Resolvable AttributeGroup where
    resolve sch = over attrGroupRef (resolve sch)

instance Resolvable (Ref AttributeGroup) where
    resolve = refResolve "AttributeGroup" attributeGroups


instance Resolvable Group where
    resolve sch = over groupRef (resolve sch)

instance Resolvable (Ref Group) where
    resolve = refResolve "Group" groups

instance Resolvable ComplexContent where
    resolve sch = over complexContentBase (resolve sch)

instance Resolvable (Ref ComplexType) where
    resolve = refResolve "ComplexType" complexTypes

instance Resolvable SimpleContent where
    resolve sch = over simpleContentBase (resolve sch)

instance Resolvable (Ref SimpleType) where
    resolve = refResolve "SimpleType" simpleTypes

instance Resolvable Element where
    resolve sch = over elementRef (resolve sch) . over elementType (resolve sch)


instance Resolvable (Ref (Either ComplexType SimpleType)) where
    resolve sch (Unresolved f) = Resolved f $ either error id
                                 ((Left <$> searchRefTarget "Either-ComplexType" complexTypes f sch)
                                  <|>
                                  (Right <$> searchRefTarget "Either-SimpleType" simpleTypes f sch))
    resolve _ r = r


instance Resolvable (Ref Element) where
    resolve = refResolve "Element" elements

instance Resolvable ComplexType where resolve _ = id
instance Resolvable SimpleType where resolve _ = id



instance Resolvable SimpleRestriction where
    resolve sch = over simpleRestrictBase (resolve sch)

instance Resolvable Union where
    resolve sch = over (unionMemberTypes.traverse) (resolve sch)


instance Resolvable Attribute where
    resolve sch = over attrRef (resolve sch) .
                  over attrType (resolve sch)

instance Resolvable (Ref Attribute) where
    resolve = refResolve "Attribute" attributes

-- | Resolve a 'Ref' against a 'Schema'.
refResolve
  :: Resolvable r =>
     String
     -> Getting (Map QN r) Schema (Map QN r)
     -> Schema
     -> Ref r
     -> Ref r
refResolve n l sch (Unresolved f) = Resolved f $ either error id $ searchRefTarget n l f sch
refResolve _ _ _ r = r

-- | Search top-level 'QN's for a 'Ref's target.
-- | Once found, target refs are also resolved -- not sure if necessary/practical.
searchRefTarget
  :: Resolvable b =>
     String
     -> Getting (Map QN b) Schema (Map QN b)
     -> QN
     -> Schema
     -> Either String b
searchRefTarget n targetLens v x = found . M.lookup v $ view targetLens x
    where found (Just a) = Right (resolve x a)
          found Nothing = Left $ n ++ ": ref search failed for " ++ show v



--
-- PARSING
--

-- | Consume a range attribute.
ranged :: XParser m => String -> (String -> Bound String) -> m (Bound String)
ranged e ctor = ctor <$> findChild (xsName e) (attr (name "value"))

-- | Consume a minInclusive restriction.
minRestrict :: XParser m => m (Bound String)
minRestrict = ranged "minInclusive" Inclusive <|> ranged "minExclusive" Exclusive

-- | Consume a maxInclusive restriction.
maxRestrict :: XParser m => m (Bound String)
maxRestrict = ranged "maxInclusive" Inclusive <|> ranged "maxExclusive" Exclusive

-- | Consume a pattern restriction.
pattern :: XParser m => m String
pattern = findChild (xsName "pattern") (attr (name "value"))

-- | Parse enum restrictions.
enums :: XParser m => m [String]
enums = findChildren (xsName "enumeration") (attr (name "value"))

-- | Parse a QName.
qn :: String -> QN
qn = parsec qnParser

-- | Match a simpleType restriction.
simpleRestrict :: XParser m => m SimpleRestriction
simpleRestrict =
    findChild (xsName "restriction") $
    SimpleRestriction <$> (Unresolved . qn <$> attr (name "base"))
                          <*> enums <*> optional minRestrict <*>
                              optional maxRestrict <*> optional pattern

-- | Match a simpleType union.
union :: XParser m => m Union
union = findChild (xsName "union") $ do
          let wsDelimited = P.many1 (attrParser >>= \r -> P.spaces >> return r)
          mts <- map (Unresolved . qn) . parsec wsDelimited <$> attr (name "memberTypes")
          uts <- findChildren (xsName "simpleType") simpleType
          return $ Union mts uts

-- | Run parsec.
parsec :: (P.Stream s Identity t) => P.Parsec s () a -> s -> a
parsec p s = either (error.show) id $ P.parse p "ParseXsd" s

-- | Attribute text parser, without whitespace.
-- | [a-zA-Z_:][-a-zA-Z0-9_:.],
-- | from http://razzed.com/2009/01/30/valid-characters-in-attribute-names-in-htmlxml/
attrParser :: P.Parsec String m String
attrParser = (:) <$> h <*> r
    where h = P.letter <|> P.oneOf "_:"
          r = many $ P.alphaNum <|> P.oneOf "-_:."

-- | QName parser.
qnParser :: P.Parsec String m QN
qnParser = P.try ((\p _ l -> QN l (Just p)) <$> many (P.letter <|> P.oneOf "_") <*>
           P.char ':' <*> many (P.alphaNum <|> P.oneOf "-_.")) <|>
           (`QN` Nothing) <$> many (P.alphaNum <|> P.oneOf "-_.")


-- | Match a simpleType.
simpleType :: XParser m => m SimpleType
simpleType = do
  atEl (xsName "simpleType")
  n <- fmap qn <$> optional (attr (name "name"))
  SimpleTypeRestrict n <$> simpleRestrict
    <|> SimpleTypeUnion n <$> union

-- | Match an attribute.
attribute :: XParser m => m Attribute
attribute = do
  atEl (xsName "attribute")
  d <- optional (attr (name "default"))
  u <- optional (attr (name "use"))
  u' <- case u of
          Nothing -> return Optional
          Just v | v == "required" -> return Required
                 | v == "optional" -> return Optional
                 | v == "prohibited" -> return Prohibited
                 | otherwise -> throwError $ "Invalid use value: " ++ show v
  let aNorm = do
              n <- qn <$> attr (name "name")
              t <- qn <$> attr (name "type")
              return $ AttributeType n (Unresolved t) u' d
      aRef = do
              r <- qn <$> attr (name "ref")
              return $ AttributeRef (Unresolved r) u' d
      aSimp = do
              n <- qn <$> attr (name "name")
              t <- oneChild simpleType
              return $ AttributeSimpleType n t
  aNorm <|> aRef <|> aSimp

-- | Match an attributeGroup.
attributeGroup :: XParser m => m AttributeGroup
attributeGroup = do
  atEl (xsName "attributeGroup")
  -- debugStack >> error "attributeGroup"
  AttributeGroup . qn <$> attr (name "name") <*> attrs <|>
    (AttributeGroupRef . Unresolved . qn) <$> attr (name "ref")

-- | Match attributes and attributeGroups (which often come together).
attrs :: XParser m => m Attributes
attrs = Attributes <$>
               findChildren (xsName "attribute") attribute <*>
               findChildren (xsName "attributeGroup") attributeGroup



-- | Match a complex type.
complexType :: XParser m => m ComplexType
complexType = do
  atEl (xsName "complexType")
  n <- fmap qn <$> optional (attr (name "name"))
  ComplexTypeSimple n <$> simpleContent
    <|> ComplexTypeComplex n <$> complexContent
    <|> ComplexTypeCompositor n <$> optional (oneChild compositor) <*> attrs

-- | Match simple content.
simpleContent :: XParser m => m SimpleContent
simpleContent = findChild (xsName "simpleContent")
                (findChild (xsName "extension")
                 (SimpleContentExtension <$> (Unresolved . qn <$> attr (name "base")) <*> attrs))

-- | Match complex content.
complexContent :: XParser m => m ComplexContent
complexContent = findChild (xsName "complexContent")
                 (findChild (xsName "extension")
                  (ComplexContentExtension <$> (Unresolved . qn <$> attr (name "base")) <*>
                                           attrs <*> optional (oneChild compositor)))

-- | Consume a compositor production.
compositor :: XParser m => m Compositor
compositor = CompositorGroup <$> group <|>
             CompositorSequence <$> sequence <|>
             CompositorChoice <$> choice

-- | Match group.
group :: XParser m => m Group
group = do
  atEl (xsName "group")
  GroupRef <$> (Unresolved . qn <$> attr (name "ref")) <*> occurs
    <|> GroupChoice <$> (fmap qn <$> optional (attr (name "name"))) <*> occurs <*> oneChild choice
    <|> GroupSequence <$> (fmap qn <$> optional (attr (name "name"))) <*> occurs <*> oneChild sequence

-- | Parse occurs-* attributes.
occurs :: XParser m => m Occurs
occurs = Occurs <$> optional (attr (name "minOccurs")) <*> optional (attr (name "maxOccurs"))

-- | Match sequence.
sequence :: XParser m => m Sequence
sequence = atEl (xsName "sequence") >> Sequence <$> occurs <*> particles

-- | Match choice.
choice :: XParser m => m Choice
choice = atEl (xsName "choice") >> Choice <$> occurs <*> particles

-- | Consume a particle production.
particles :: XParser m => m [Particle]
particles = allChildren (PartGroup <$> group <|>
                         PartSequence <$> sequence <|>
                         PartChoice <$> choice <|>
                         PartElement <$> element)

-- | Match element.
element :: XParser m => m Element
element = do
  atEl (xsName "element")
  let el = ElementType . qn <$> attr (name "name") <*> (Unresolved . qn <$> attr (name "type")) <*> occurs
      elSim = ElementSimple . qn <$> attr (name "name") <*> oneChild simpleType <*> occurs
      elCom = ElementComplex . qn <$> attr (name "name") <*> oneChild complexType <*> occurs
      elRef = ElementRef <$> (Unresolved . qn <$> attr (name "ref")) <*> occurs
  el <|> elRef <|> elSim <|> elCom


-- | Main parser.
schemaParser :: XParser m => m Schema
schemaParser = Schema <$>
         (mapifyJust simpleTypeName <$> anyChildren simpleType) <*>
         (mapifyJust complexTypeName <$> anyChildren complexType) <*>
         (mapifyJust groupName <$> anyChildren group) <*>
         (mapify attrGroupName <$> anyChildren attributeGroup) <*>
         (mapify elementName <$> anyChildren element) <*>
         (mapify attrName <$> anyChildren attribute)


mapify :: Show a => Getting (Leftmost QN) a QN -> [a] -> Map QN a
mapify l = M.fromList . map (\a -> (justName a $ firstOf l a,a))
    where justName a = fromMaybe (error $ "mapify: name field not present: " ++ show a)

mapifyJust :: Show a => Getting (Leftmost (Maybe QN)) a (Maybe QN) -> [a] -> Map QN a
mapifyJust l = M.fromList . map (\a -> (justName a $ firstOf l a, a))
    where justName a = fromMaybe (error $ "mapifyJust: name required at top level: " ++ show a) .
                       fromMaybe (error $ "mapify: name field not present: " ++ show a)


-- | Adjust top-level names to have supplied prefix.
namespaceSchema :: String -> Schema -> Schema
namespaceSchema ns =
    let pfx (k,v) = (setPfx k, over template justNoPfx v)
        justNoPfx q@(QN _ (Just _)) = q
        justNoPfx q = setPfx q
        setPfx = set qPrefix (Just ns)
        remap :: Data a => M.Map QN a -> M.Map QN a
        remap = M.fromList . over traverse pfx . M.toList
    in
    over simpleTypes remap .
    over complexTypes remap .
    over groups remap .
    over attributeGroups remap .
    over elements remap .
    over attributes remap

-- | XML Schema "anySimpleType" (ie, built-ins like string, double etc).
anySimpleTypeName :: QN
anySimpleTypeName = QN "anySimpleType" (Just "xs")

-- | Load XSD itself as a 'Schema'.
loadXsdSchema :: FilePath -> IO Schema
loadXsdSchema f = do
  ts <- _simpleTypes . namespaceSchema "xs" <$> parseFile f
  let anySimpleType = SimpleTypeRestrict (Just anySimpleTypeName) (SimpleRestriction Final [] Nothing Nothing Nothing)
  let s = set simpleTypes (M.insert anySimpleTypeName anySimpleType ts) mempty
  return s -- $ resolveRefs s


-- | Parse an XSD file.
parseFile :: FilePath -> IO Schema
parseFile f = readXml f >>= parseX schemaParser >>= either (throwIO . userError) return
