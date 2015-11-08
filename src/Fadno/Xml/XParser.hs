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
    -- * Attribute operations
    ,attr
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

-- | Expect a particular attribute.
attr :: XParser m => X.QName -> m String
attr n = do
  v <- X.findAttr n <$> peek
  maybe (throwError $ "Attribute not found: " ++ show n) return v


-- | Verify current element.
atEl :: XParser m => X.QName -> m ()
atEl n = do
  e <- X.elName <$> peek
  when (n /= e) $ throwError ("Wrong element name: " ++ show e)

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

-- | Act on all children.
allChildren :: XParser m => m a -> m [a]
allChildren act = X.elChildren <$> peek >>= mapM (dropTo act)

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
