{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Backtracking combinators for consuming XML productions (elements, attributes).
module Fadno.Xml.XParser
    (
     -- * XParser monad
     XParser
    ,parseX
    -- * Stack manipulation
    ,peek,push,pop,checkStack
    -- * Element operations
    ,atEl,findChild,findChildren,anyChildren,oneChild,allChildren,manyOrdered
    -- * Attribute/Text operations
    ,attr,textContent
    -- * QNames
    ,name,xsName
    -- * Utility
    ,readXml
    -- * nextgen
     ,XParse(..),runXParse,xfail,require,xattr,xtext,xchild,xread
    ) where

import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as C

import Control.Exception
import Control.Monad.State.Strict hiding (sequence)
import Control.Monad.Except hiding (sequence)
import Data.Either
import Control.Applicative
import Prelude hiding (sequence)
import Control.Lens
import Text.Read (readMaybe)

-- Element lenses
lAttrs :: Lens' X.Element [X.Attr]
lAttrs f s = fmap (\a -> s { X.elAttribs = a }) (f $ X.elAttribs s)
lContent :: Lens' X.Element [X.Content]
lContent f s = fmap (\a -> s { X.elContent = a }) (f $ X.elContent s)
_Elem :: Prism' X.Content X.Element
_Elem = prism X.Elem $ \c -> case c of X.Elem e -> Right e; _ -> Left c

-- | Stack entry tracking identified elements.

-- | XParser constraint kind. Stack state + alternative + errors.
type XParser m = (Alternative m, MonadState [X.Element] m, MonadError String m)

-- | run XParser on an element.
parseX :: (Monad m) => StateT [X.Element] (ExceptT String m) b -> X.Element -> m (Either String b)
parseX sel e = runExceptT (evalStateT sel [e])

-- | Stack peek.
peek :: XParser m => m X.Element
peek = head <$> checkStack

-- | Verify populated stack.
checkStack :: XParser m => m [X.Element]
checkStack = get >>= \s ->
             if null s then throwError "Invalid stack" else return s

-- | Stack push.
push :: XParser m => X.Element -> m ()
push e = modify (e:)

-- | Stack pop.
pop :: XParser m => m ()
pop = checkStack >>= \(_:rest) -> put rest

-- | Expect/consume a particular attribute.
attr :: XParser m => X.QName -> m String
attr n = do
  as <- view lAttrs <$> peek
  let (as',found) = foldl test ([],Nothing) as
      test (rs,f@(Just _)) a = (a:rs,f)
      test (rs,_) a | X.attrKey a == n = (rs,Just (X.attrVal a))
                    | otherwise = (a:rs,Nothing)
  case found of
    Nothing -> throwError $ "Attribute not found: " ++ show n
    Just t -> do
               _head.lAttrs .= as'
               return t

-- | Get text content, returning empty string if none, per 'strContent'.
textContent :: XParser m => m String
textContent = X.strContent <$> peek

-- | Verify and "consume" current element.
atEl :: XParser m => X.QName -> m ()
atEl n = do
  e <- X.elName <$> peek
  when (n /= e) $ throwError ("Wrong element name: " ++ show e)

-- | Find child element and act on it.
findChild :: XParser m => X.QName -> m a -> m a
findChild n act = do
  c <- onChildren ((==n) . X.elName) True True act
  case c of
    [] -> throwError $ "No such child " ++ show n
    [e] -> return e
    _ -> throwError $ "findChild: multiple results: " ++ show n

-- | Expect to find one child only, and run action on it.
oneChild :: XParser m => m a -> m a
oneChild act = do
  cs <- onChildren (const True) True True act
  case cs of
    [c] -> return c
    _ -> throwError $ "oneChild: found " ++ show (length cs)

-- | Find zero or many children and act on them.
findChildren :: XParser m => X.QName -> m a -> m [a]
findChildren n = onChildren ((==n) . X.elName) False False

-- | Act on all children.
allChildren :: XParser m => m a -> m [a]
allChildren = onChildren (const True) False False

-- | Act on, consume children.
-- Accepts filter, optional flag, "just1" flag.
onChildren :: XParser m => (X.Element -> Bool) -> Bool -> Bool -> m a -> m [a]
onChildren filt opt just1 act = do
  h <- peek
  let exec rs c@(X.Elem e)
          | filt e =
              if just1 && not (null (view _2 rs))
              then return $ over _1 (c:) rs
              else do
                push e
                r <- catchError (Right <$> act) (return . Left)
                pop
                case r of
                  Left err | opt -> return $ over _1 (c:) rs
                           | otherwise -> return $ over _3 (err:) rs
                  Right v -> return $ over _2 (v:) rs
          | otherwise = return $ over _1 (c:) rs
      exec rs c = return $ over _1 (c:) rs
  (cs',rs,fs) <- foldM exec ([],[],[]) (view lContent h)
  unless (null fs) $ throwError $ "Failure: " ++ show fs
  _head.lContent .= reverse cs'
  return (reverse rs)

-- | Flailing attempt to restore "order" by faking a single-child element one at a time.
manyOrdered :: XParser m => m a -> m [a]
manyOrdered act = do
  cs <- view lContent <$> peek
  let fake c = X.Element (name "fake") [] [c] Nothing
      exec rs c = do
                push (fake c)
                r <- catchError (Right <$> act) (return . Left)
                pop
                case r of
                  Left _ -> return (over _2 (c:) rs)
                  Right a -> return (over _1 (a:) rs)
  (as,cs') <- foldM exec ([],[]) cs
  _head.lContent .= reverse cs'
  return $ reverse as


-- | Run optional action on all children.
anyChildren :: XParser m => m a -> m [a]
anyChildren = onChildren (const True) True False

-- | Special support for XSD QNames.
xsName :: String -> X.QName
xsName n = X.QName n (Just "http://www.w3.org/2001/XMLSchema") (Just "xs")

-- | Local-only QName.
name :: String -> X.QName
name n = X.QName n Nothing Nothing

-- | Convenience to read in top element from file.
readXml :: FilePath -> IO X.Element
readXml f = maybe (throwIO $ userError "parse failed") return =<< X.parseXMLDoc <$> readFile f


-- REDO. Need explicit semantics/zipper.

type XErrors = [([C.Tag],String)]

newtype XParse a = XParse { unXParse :: StateT C.Cursor (Except XErrors) a }
    deriving (Functor,Applicative,Monad,MonadState C.Cursor,MonadError XErrors,Alternative)

runXParse :: X.Element -> XParse a -> Either XErrors a
runXParse e act = runExcept (evalStateT (unXParse act) (C.fromElement e))

xfail :: String -> XParse a
xfail msg = do
  ts <- map (view _2) . C.parents <$> get
  throwError [(ts,msg)]

require :: String -> Maybe a -> XParse a
require msg = maybe (xfail $ "Required: " ++ msg) return

lcurrent :: Lens' C.Cursor X.Content
lcurrent f s = fmap (\a -> s { C.current = a}) (f (C.current s))

xattr :: X.QName -> XParse String
xattr n = xel >>= require ("attribute " ++ show n) . X.findAttr n

xel :: XParse X.Element
xel = firstOf _Elem <$> use lcurrent >>= require "element"

xtext :: XParse String
xtext = X.strContent <$> xel

xchild :: X.QName -> XParse a -> XParse a
xchild n act = do
  fc <- C.firstChild <$> get >>= require "at least one child"
  let firstEl :: C.Cursor -> XParse C.Cursor
      firstEl c = case firstOf (lcurrent._Elem) c of
                    Just e -> do
                      when (X.elName e /= n) (xfail $ "Element not found: " ++ show n)
                      return c
                    Nothing -> do
                      c' <- C.right c & require "at least one element child"
                      firstEl c'
  e <- firstEl fc
  put e
  r <- catchError (Right <$> act) (return . Left)
  case r of
    Right a -> do
           p <- C.removeGoUp <$> get >>= require "parent"
           put p
           return a
    Left err -> do
           p <- C.parent <$> get >>= require "parent"
           put p
           throwError err

xread :: Read a => String -> String -> XParse a
xread msg s = require (msg ++ ": " ++ s) $ readMaybe s

--Tests

data TestAttributes =
      TestAttributes {
          attributesEditorial :: TestEditorial
        , attributesDivisions :: (Maybe Integer) -- ^ /divisions/ child element
        , attributesTime :: [TestTime] -- ^ /time/ child element
        , attributesTranspose :: (Maybe String) -- ^ /transpose/ child element
       }
    deriving (Eq,Show)
parseAttributes :: XParse TestAttributes
parseAttributes =
      TestAttributes
        <$> parseEditorial
        <*> optional (xchild (name "divisions") (xtext >>= xread "Integer"))
        <*> many (xchild (name "time") parseTime)
        <*> optional (xchild (name "transpose") (xtext >>= return))

data TestEditorial =
      TestEditorial {
          editorialFootnote :: (Maybe String)
        , editorialLevel :: (Maybe String)
       }
    deriving (Eq,Show)
parseEditorial :: XParse TestEditorial
parseEditorial =
      TestEditorial
        <$> optional (xattr (name "footnote"))
        <*> optional (xattr (name "level"))

data TestTime =
      TestTime {
          timeNumber :: (Maybe Integer) -- ^ /number/ attribute
        , timeTime :: TestChxTime
       }
    deriving (Eq,Show)
parseTime :: XParse TestTime
parseTime =
      TestTime
        <$> optional (xattr (name "number") >>= xread "Integer")
        <*> parseChxTime

-- | @time@ /(choice)/
data TestChxTime =
      TestTimeTime {
          chxtimeTime :: [TestSeqTime]
       }
    | TestTimeSenzaMisura {
          timeSenzaMisura :: TestEmpty -- ^ /senza-misura/ child element
       }
    deriving (Eq,Show)
parseChxTime :: XParse TestChxTime
parseChxTime =
      TestTimeTime
        <$> many (parseSeqTime)
      <|> TestTimeSenzaMisura
        <$> xchild (name "senza-misura") parseEmpty


-- | @time@ /(sequence)/
data TestSeqTime =
      TestSeqTime {
          timeBeats :: String -- ^ /beats/ child element
        , timeBeatType :: String -- ^ /beat-type/ child element
       }
    deriving (Eq,Show)
parseSeqTime :: XParse TestSeqTime
parseSeqTime =
      TestSeqTime
        <$> xchild (name "beats") (xtext >>= return)
        <*> xchild (name "beat-type") (xtext >>= return)

data TestEmpty =
      TestEmpty
    deriving (Eq,Show)
parseEmpty :: XParse TestEmpty
parseEmpty =
      return TestEmpty
