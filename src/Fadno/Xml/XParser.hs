{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Backtracking combinators for consuming XML productions (elements, attributes).
module Fadno.Xml.XParser
    (
     -- * XParser monad
     XParser
    ,parseX
    -- * Stack manipulation
    ,peek,push,pop,checkStack
    -- * Element operations
    ,atEl,findChild,findChildren,anyChildren,oneChild,allChildren,dropTo
    -- * Attribute/Text operations
    ,attr,textContent
    -- * QNames
    ,name,xsName
    -- * Utility
    ,readXml
    ) where

import qualified Text.XML.Light as X

import Control.Exception
import Control.Monad.State.Strict hiding (sequence)
import Control.Monad.Except hiding (sequence)
import Data.Either
import Control.Applicative
import Prelude hiding (sequence)
import Data.Maybe
import qualified Data.Set as S

-- | Stack entry tracking identified elements.
data Entry = Entry { _el :: X.Element,
                     _consumed :: S.Set X.QName }
entry :: X.Element -> Entry
entry e = Entry e S.empty

-- | XParser constraint kind. Stack state + alternative + errors.
type XParser m = (Alternative m, MonadState [Entry] m, MonadError String m)

-- | run XParser on an element.
parseX :: (Monad m) => StateT [Entry] (ExceptT String m) b -> X.Element -> m (Either String b)
parseX sel e = runExceptT (evalStateT sel [entry e])

-- | Stack peek.
peek :: XParser m => m X.Element
peek = _el <$> peek'

-- | Stack peek.
peek' :: XParser m => m Entry
peek' = head <$> checkStack

-- | Verify populated stack.
checkStack :: XParser m => m [Entry]
checkStack = get >>= \s ->
             if null s then throwError "Invalid stack" else return s

-- | Stack push.
push :: XParser m => X.Element -> m ()
push e = modify (entry e:)

-- | Stack pop.
pop :: XParser m => m ()
pop = checkStack >>= \(_:rest) -> put rest

-- | Expect a particular attribute.
attr :: XParser m => X.QName -> m String
attr n = do
  v <- X.findAttr n <$> peek
  maybe (throwError $ "Attribute not found: " ++ show n) return v

-- | Get text content, returning empty string if none, per 'strContent'.
textContent :: XParser m => m String
textContent = X.strContent <$> peek

-- | Verify and "consume" current element.
atEl :: XParser m => X.QName -> m ()
atEl n = do
  e <- X.elName <$> peek
  if n /= e
  then throwError ("Wrong element name: " ++ show e)
  else modify (consumeChild e)
       where consumeChild child (h:Entry p c:r) = h:Entry p (S.insert child c):r
             consumeChild _ s = s

-- | Find child element and act on it.
findChild :: XParser m => X.QName -> m a -> m a
findChild n act = do
  c <- X.findChild n <$> peek
  case c of
    Nothing -> throwError $ "No such child " ++ show n
    Just e -> dropTo act e

-- | Expect to find one child only, and run action on it.
oneChild :: XParser m => m a -> m a
oneChild act = do
  cs <- anyChildren act
  case cs of
    [c] -> return c
    _ -> throwError $ "oneChild: found " ++ show (length cs)

-- | "Drop into" element: push onto stack, act on it, pop off.
dropTo :: XParser m => m a -> X.Element -> m a
dropTo act e = push e >> act >>= \r -> pop >> return r

-- | Find zero or many children and act on them.
findChildren :: XParser m => X.QName -> m a -> m [a]
findChildren n act = X.findChildren n <$> peek >>= mapM (dropTo act)

-- | Act on all "unconsumed" children.
allChildren :: XParser m => m a -> m [a]
allChildren act = do
  (Entry h seen) <- peek'
  let unseen = filter (\e -> not (S.member (X.elName e) seen)) $ X.elChildren h
  mapM (dropTo act) unseen

-- | Run optional action on all children.
anyChildren :: XParser m => m a -> m [a]
anyChildren act = catMaybes <$> allChildren (optional act)

-- | Special support for XSD QNames.
xsName :: String -> X.QName
xsName n = X.QName n (Just "http://www.w3.org/2001/XMLSchema") (Just "xs")

-- | Local-only QName.
name :: String -> X.QName
name n = X.QName n Nothing Nothing

-- | Convenience to read in top element from file.
readXml :: FilePath -> IO X.Element
readXml f = maybe (throwIO $ userError "parse failed") return =<< X.parseXMLDoc <$> readFile f
