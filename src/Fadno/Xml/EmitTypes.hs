{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Consume a 'Schema' and emit types intended for code generation.
module Fadno.Xml.EmitTypes
    (
     -- * XSD Schema to types
     emitSchema
     -- * Emit various productions
    ,emitElement,emitSimpleType,emitComplexType,emitCompositor
    ,emitGroup,emitChoice,emitSequence,emitParticle,emitAttrFields
     -- * Emit monad
    ,Emit,Env(..),schema,EmitState(..),types,stack,runEmit,die
    -- * Types
    ,Name(..),Namespace(..),Cardinality(..)
    ,Field(..),fieldName,fieldType,fieldCardinality,fieldXmlEmit,fieldIdx,FieldEmit(..)
    ,Ctor(..),ctorName,ctorFields
    ,Type(..),typeName,typeType,typeDerives,typeImpls,typeEmit,typeCtors,typeEnumValues,coreType
    ,DerivesFamily(..),DataTypeEmit(..),CoreType(..),Impl(..)
    ) where

import Fadno.Xml.ParseXsd
import Data.Monoid
import Control.Lens hiding (Choice,element,elements,anon)

import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Maybe
import Data.Data
import Control.Exception hiding (handle)
import qualified Data.Map.Strict as M


-- | Field cardinality
data Cardinality =
    One |
    ZeroOrOne |
    Many
    deriving (Eq,Show,Data,Typeable,Ord,Enum,Bounded)

-- | Namespaces for various productions.
data Namespace =
    NSElement |
    NSSimple |
    NSUnion |
    NSComplex |
    NSBuiltIn |
    NSChoice |
    NSSequence |
    NSGroup
    deriving (Eq,Show,Data,Typeable,Ord,Enum,Bounded)

-- | Type name.
data Name = Name {
      nNamespace :: Namespace
    , nName :: QN
    , nIdx :: Int }
            deriving (Eq,Show,Data,Typeable,Ord)

-- | Field xml emit cue.
data FieldEmit =
    FieldAttribute |
    FieldElement |
    FieldText |
    FieldOther
    deriving (Eq,Show,Data,Typeable,Ord)

-- | Type field.
data Field = Field {
      _fieldName :: QN
    , _fieldType :: Type
    , _fieldCardinality :: Cardinality
    , _fieldXmlEmit :: FieldEmit
    , _fieldIdx :: Int
    } deriving (Eq,Data,Typeable,Ord)

instance Show Field where
    show (Field n t c x i) =
        "Field {_fieldName = " ++ show n ++
                    ",_fieldType = {_typeName = " ++ show (_typeName t) ++
                    "},_fieldCardinality = " ++ show c ++
                    ",_fieldXmlEmit = " ++ show x ++
                    ",_fieldIdx = " ++ show i ++
                    "}"

-- | Datatype constructor.
data Ctor = Ctor {
      _ctorName :: String
    , _ctorFields :: [Field]
    } deriving (Eq,Show,Data,Typeable,Ord)

-- | Derive type "families".
data DerivesFamily =
    NewTypeString |
    NewTypeIntegral |
    NewTypeNum |
    DataEnum |
    OtherDerives
    deriving (Eq,Show,Data,Typeable,Ord,Enum,Bounded)

-- | "Impl"s of classes, but also grab-bag of type deets.
data Impl =
    -- | Specifies min and max values
    Bounds (Maybe (Bound String), Maybe (Bound String)) |
    -- | Specifies string pattern
    Pattern String |
    -- | Implement 'Show' for newtype
    NewTypeShow |
    -- | Top-level element, so emit element name as well as contents.
    -- Elements normally inherit the name from the referencing production.
    TopLevel
            deriving (Eq,Show,Data,Typeable,Ord)

-- | newtype base types. Name after CT should be Haskell base type.
data CoreType =
    CTString |
    CTDecimal |
    CTFloat |
    CTDouble |
    CTInt |
    CTBool
    deriving (Eq,Show,Data,Typeable,Ord,Enum,Bounded)

-- | Data type xml emit cue.
data DataTypeEmit =
    DataTypeComplex |
    DataTypeSimple |
    DataTypeCompositor
    deriving (Eq,Show,Data,Typeable,Ord)

-- | Emitted type.
data Type =
    NewType {
      _typeName :: Name
    , _typeType :: Type
    , _typeDerives :: DerivesFamily
    , _typeImpls :: [Impl]
    , _typeDoc :: Maybe Documentation
    } |
    DataType {
      _typeName :: Name
    , _typeCtors :: [Ctor]
    , _typeDerives :: DerivesFamily
    , _typeImpls :: [Impl]
    , _typeEmit :: DataTypeEmit
    , _typeDoc :: Maybe Documentation
    } |
    EnumType {
      _typeName :: Name
    , _typeEnumValues :: [String]
    , _typeDerives :: DerivesFamily
    , _typeImpls :: [Impl]
    , _typeDoc :: Maybe Documentation
    } |
    BuiltIn {
      _typeName :: Name
    , _coreType :: CoreType
    , _typeDerives :: DerivesFamily
    , _typeImpls :: [Impl]
    } deriving (Eq,Show,Data,Typeable,Ord)

$(makeLenses ''Type)
$(makeLenses ''Ctor)
$(makeLenses ''Field)


-- | Reader environment.
data Env = Env {
      _schema :: Schema
}
$(makeLenses ''Env)

-- | State data.
data EmitState = EmitState { _types :: M.Map Name Type, _stack :: [Name] }
$(makeLenses ''EmitState)
instance Monoid EmitState where
    mempty = EmitState mempty mempty
    (EmitState a b) `mappend` (EmitState c d) = EmitState (a<>c) (b<>d)

-- | Emit monad.
type Emit a = ReaderT Env (StateT EmitState IO) a

-- | Run emit monad.
runEmit :: Env -> EmitState -> Emit a -> IO (a, EmitState)
runEmit env st act = runStateT (runReaderT act env) st

-- | Emit schema types. Starts with element productions
-- and emits all dependent types.
emitSchema :: Schema -> Emit ()
emitSchema s = do
  els <- M.keys <$> mapM emitElement (_elements s)
  types %= M.mapWithKey
            (\k v -> if nName k `elem` els
                     then over typeImpls (TopLevel:) v
                     else v)

-- | String builtin.
builtInString :: Type
builtInString = mkBuiltIn "string" CTString NewTypeString

-- | All built-in types.
builtIns :: M.Map QN Type
builtIns = foldr (\b -> M.insert (nName (_typeName b)) b) mempty
           [builtInInteger,builtInDecimal,builtInDouble,
            builtInFloat,builtInBoolean,builtInString]
    where
      builtInInteger = mkBuiltIn "integer" CTInt NewTypeIntegral
      builtInDecimal = mkBuiltIn "decimal" CTDecimal NewTypeNum
      builtInFloat = mkBuiltIn "float" CTFloat NewTypeNum
      builtInDouble = mkBuiltIn "double" CTDouble NewTypeNum
      builtInBoolean = mkBuiltIn "boolean" CTBool OtherDerives

-- | Smart ctor for built-in types.
mkBuiltIn :: String -> CoreType -> DerivesFamily -> Type
mkBuiltIn n ct df = BuiltIn (Name NSBuiltIn (QN n (Just "xs")) 0) ct df [NewTypeShow]


-- | Emit type for element content; element name production
-- captured in containing field.
emitElement :: Element -> Emit Type
emitElement (ElementType _ t _ doc) = do
  rt <- resolvedRef t
  case rt of
    Left ct -> emitComplexType Nothing ct
    Right st -> emitSimpleType st
emitElement (ElementComplex n c _ doc) = emitComplexType (Just n) c
emitElement (ElementRef {}) = die "ElementRef unsupported"
emitElement (ElementSimple {}) = die "ElementSimple unsupported"

-- | Error out in IO.
die :: MonadIO m => String -> m a
die = liftIO . throwIO . userError


-- | Resolve a ref and obtain the resolved value.
resolvedRef :: (Resolvable (Ref a)) => Ref a -> Emit a
resolvedRef r = do
  s <- view schema
  case firstOf refvalue (resolve s r) of
    Just a -> return a
    Nothing -> die $ "resolvedRef: resolve failed on " ++ show r


-- | Handle simpleType production.
emitSimpleType :: SimpleType -> Emit Type
emitSimpleType t =
    case view simpleTypeName t of
      Nothing -> use stack >>= \s -> die $ "emitSimpleType: anon type: " ++ show t ++
                 ", stack: " ++ show s
      Just stn -> do
          bt <- tryBuiltIn t
          maybe (checkDefinedType NSSimple stn $ doSimpleType t) return bt

doSimpleType :: SimpleType -> Name -> Emit Type
doSimpleType (SimpleTypeRestrict _ (SimpleRestriction base enumz mins maxs patt) doc) n = do
  bt <- resolvedRef base
  if not (null enumz)
  then emitEnum bt n enumz doc
  else do
    btt <- emitSimpleType bt
    return $ NewType n btt (_typeDerives btt)
           ([Bounds (mins,maxs) | isJust mins || isJust maxs] ++
            maybe [] (return.Pattern) patt ++
            _typeImpls btt) doc
doSimpleType (SimpleTypeUnion _ (Union refs sts) doc) n = do
  rts <- mapM resolvedRef refs
  rtts <- mapM emitSimpleType rts
  let doAnon t = checkUniqueType NSUnion Nothing (doSimpleType t)
  atns <- mapM doAnon sts
  let doCtor i t =
          Ctor (_qLocal $ nName (_typeName t))
                   [Field (QN (show i) Nothing) t One FieldAttribute 0]
  return $ fixFields $
         DataType n (zipWith doCtor [(1 :: Int) ..] $ rtts ++ atns)
                  OtherDerives [] DataTypeSimple doc

-- | Check if type is built-in.
tryBuiltIn :: SimpleType -> Emit (Maybe Type)
tryBuiltIn t =
    case view simpleTypeName t of
      Nothing -> return Nothing
      Just tn ->
          case M.lookup tn builtIns of
            Just bt -> return (Just bt)
            Nothing ->
                case firstOf (simpleTypeRestriction.simpleRestrictBase.unresolved) t of
                  Nothing -> return Nothing
                  Just bt | bt == anySimpleTypeName -> return (Just builtInString)
                          | otherwise -> return Nothing




emitEnum :: SimpleType -> Name -> [String] -> Maybe Documentation -> Emit Type
emitEnum _base n vals doc = return $ EnumType n vals DataEnum [] doc


-- | Complex type. 'anon' arg indicates element-defined complex type, therefore unique;
-- otherwise defined type.
emitComplexType :: Maybe QN -> ComplexType -> Emit Type
emitComplexType anon@(Just _) t =
    checkUniqueType NSComplex anon $ doComplexType t
emitComplexType Nothing t = do
  n <- maybe (die $ "emitComplexType: no complex name: " ++ show t) return $
       view complexTypeName t
  checkDefinedType NSComplex n $ doComplexType t


doComplexType :: ComplexType -> Name -> Emit Type
doComplexType (ComplexTypeSimple _ (SimpleContentExtension scb atts) doc) mn = do
  rt <- resolvedRef scb >>= emitSimpleType
  ats <- emitAttrFields atts
  return $ fixFields $ DataType mn
             [Ctor "" (Field (getRefType scb) rt One FieldText 0:ats)]
             OtherDerives [] DataTypeComplex doc
doComplexType (ComplexTypeCompositor _ comp atts doc) mn = do
  ats <- emitAttrFields atts
  c <- maybe (return []) (emitCompositor [nName mn]) comp
  return $ fixFields $
         DataType mn [Ctor "" (ats ++ c)] OtherDerives [] DataTypeComplex doc
doComplexType (ComplexTypeComplex _ (ComplexContentExtension ccb atts comp) doc) mn = do
  ct <- resolvedRef ccb >>= emitComplexType (Just $ nName mn)
  ats <- emitAttrFields atts
  c <- maybe (return []) (emitCompositor [nName mn]) comp
  -- TODO using FieldOther for base content, should be ok?
  return $ fixFields $ DataType mn
             [Ctor "" (Field (getRefType ccb) ct One FieldOther 0:(ats ++ c))]
             OtherDerives [] DataTypeComplex doc

-- | Obtain string name of 'Ref'.
getRefType :: Ref t -> QN
getRefType (Unresolved n) = n
getRefType (Resolved n _) = n
getRefType Final = error "Attempt to resolve ref on final"

-- | Emit compositor field.
emitCompositor :: [QN] -> Compositor -> Emit [Field]
emitCompositor ns (CompositorGroup g) = emitGroup ns g
emitCompositor ns (CompositorChoice c) = emitChoice ns c Nothing
emitCompositor ns (CompositorSequence s) = emitSequence ns Nothing s Nothing

-- | Build up name stack, used in compositor/particle field emittance.
appendNames :: [QN] -> Maybe QN -> [QN]
appendNames ss = maybe ss (:ss)

-- | Emit group. Referenced groups make types, others pass through.
emitGroup :: [QN] -> Group -> Emit [Field]
emitGroup ns (GroupRef r o) = do
    g <- resolvedRef r
    t <- checkDefinedType NSGroup (getRefType r) $ \tn ->
         do
           fs <- emitGroup ns g
           return $ DataType tn [Ctor "" fs]
                  OtherDerives [] DataTypeComplex Nothing
    return $ forOccurs o $ Field (getRefType r) t One FieldOther 0
emitGroup ns (GroupChoice n o c doc) =
    concatMap (forOccurs o) <$> emitChoice (appendNames ns n) c doc
emitGroup ns (GroupSequence n o s doc) =
    concatMap (forOccurs o) <$> emitSequence (appendNames ns n) (Just o) s doc

-- | Choice production.
emitChoice :: [QN] -> Choice -> Maybe Documentation -> Emit [Field]
emitChoice ns (Choice o ps) doc = do
  fss <- mapM (emitParticle ns) ps
  t <- checkUniqueType NSChoice (Just $ head ns) $ \mn ->
       do
         let cctor fs = Ctor (_qLocal (chooseName fs)) fs
             chooseName [f] = _fieldName f
             chooseName fs = _fieldName (head fs) -- ++ show (length fs)
         return $ fixFields $
                DataType mn (map cctor fss) OtherDerives [] DataTypeCompositor doc
  return $ forOccurs o (Field (head ns) t One FieldOther 0)

-- | Guarantee unique constructor fields for a type.
fixFields :: Type -> Type
fixFields = over typeCtors (\cs -> evalState (mapM fixC cs) mempty)
    where fixC :: Ctor -> State (M.Map QN Int) Ctor
          fixC c@(Ctor _ fs) = do
            fs' <- mapM fixF fs
            return $ set ctorFields fs' c
          fixF :: Field -> State (M.Map QN Int) Field
          fixF f@(Field fn _ _ _ _) = do
            seen <- M.lookup fn <$> get
            modify (M.insertWith (+) fn 1)
            return $ maybe f (\i -> set fieldIdx i f) seen


-- | Sequence production.
emitSequence :: [QN] -> Maybe Occurs -> Sequence -> Maybe Documentation -> Emit [Field]
emitSequence ns parentO (Sequence o ps) doc = do
  fs <- concat <$> mapM (emitParticle ns) ps
  -- don't emit new type for 'One' cardinality
  case (occursToCardinality o,fmap occursToCardinality parentO) of
    (One,Nothing) -> return fs
    (One,Just One) -> return fs
    _ -> do
      t <- checkUniqueType NSSequence (Just $ head ns) $ \mn ->
           return $ fixFields $
                  DataType mn [Ctor "" fs] OtherDerives [] DataTypeCompositor doc
      return $ forOccurs o (Field (head ns) t One FieldOther 0)

-- | Particle field production.
-- Element fields emit containing element.
emitParticle :: [QN] -> Particle -> Emit [Field]
emitParticle _ (PartElement e) = do
  et <- emitElement e
  fn <- maybe (die "emitParticle: emitted element must have name") return $
        firstOf elementName e
  let o = fromMaybe (Occurs Nothing Nothing) $ firstOf elementOccurs e
  return $ forOccurs o $ Field fn et One FieldElement 0
emitParticle ns (PartGroup g) = emitGroup ns g
emitParticle ns (PartChoice c) = emitChoice ns c Nothing
emitParticle ns (PartSequence s) = emitSequence ns Nothing s Nothing


-- | Modify a field cardinality per occurs.
-- TODO: musicxml uses max occurs only for >1 or unbounded, but could be 1 ...
-- TODO use occursToCardinality
forOccurs :: Occurs -> Field -> [Field]
forOccurs (Occurs Nothing Nothing) f = [f]
forOccurs (Occurs (Just "0") Nothing) f = [set fieldCardinality ZeroOrOne f]
forOccurs (Occurs _ _) f = [set fieldCardinality Many f]

-- | Imprecise mapping of occurs-* to cardinality.
occursToCardinality :: Occurs -> Cardinality
occursToCardinality (Occurs Nothing Nothing) = One
occursToCardinality (Occurs Nothing (Just "1")) = One
occursToCardinality (Occurs (Just "0") Nothing) = ZeroOrOne
occursToCardinality (Occurs (Just "1") Nothing) = One
occursToCardinality (Occurs (Just "1") (Just "1")) = One
occursToCardinality (Occurs _ _) = Many


-- | Handle attribute and attribute group productions as fields.
emitAttrFields :: Attributes -> Emit [Field]
emitAttrFields  = doAttrs
    where doAttrs (Attributes as ags) =
              (++) <$> (catMaybes <$> mapM resolveAttr as) <*>
              (concat <$> mapM resolveAttrGroup ags)
          forUse Prohibited _ = Nothing
          forUse Optional a = Just $ set fieldCardinality ZeroOrOne a
          forUse Required a = Just $ set fieldCardinality One a
          resolveAttr (AttributeRef r u _) = do
              a <- resolvedRef r
              maybe Nothing (forUse u) <$> resolveAttr a
          resolveAttr (AttributeSimpleType n t) = do
              a <- checkDefinedType NSSimple n (doSimpleType t) -- TODO why not just doSimpleType, or emit??
              return $ Just $ Field n a One FieldAttribute 0
          resolveAttr (AttributeType n r u _) =
              resolvedRef r >>= emitSimpleType >>= \t ->
                  return $ forUse u $ Field n t One FieldAttribute 0
          resolveAttrGroup (AttributeGroup _ as doc) = doAttrs as
          resolveAttrGroup (AttributeGroupRef r) = resolvedRef r >>= resolveAttrGroup

-- | register a unique type, where namespace collisions will
-- | increment 'nIdx' on the namespace.
checkUniqueType :: Namespace -> Maybe QN -> (Name -> Emit Type) -> Emit Type
checkUniqueType ns mtn act = do
  n <- case mtn of
          Just t -> return t
          Nothing -> fmap (take 1) (use stack) >>= \h ->
                     case h of [Name _ a _] -> return a
                               _ -> die $ "checkType: empty stack on anon type: " ++ show ns
  let find i = do
        let cand = Name ns n i
        mt <- M.lookup cand <$> use types
        maybe (return cand) (\_ -> find (succ i)) mt
  mn <- find 0
  buildType mn act


-- | register or lookup a pre-defined type; the 'Namespace' argument
-- | should uniquely idenfify.
checkDefinedType :: Namespace -> QN -> (Name -> Emit Type) -> Emit Type
checkDefinedType ns tn act = do
  let mn = Name ns tn 0
  ts <- M.lookup mn <$> use types
  case ts of
    Just t -> return t
    Nothing -> buildType mn act

-- | Update state for new type.
buildType :: Name -> (Name -> Emit Type) -> Emit Type
buildType mn act = do
  stack %= (mn:)
  nt <- act mn
  stack %= tail
  types %= M.insert mn nt
  return nt
