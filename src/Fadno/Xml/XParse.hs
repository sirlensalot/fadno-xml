{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Combinators over XML.
module Fadno.Xml.XParse

    (
     XParse(..),runXParse,xfail,require,xattr,xtext,xchild,xread,xel
    -- * QNames
    , name,xsName
    -- * Utility
    , readXml

    ) where

import qualified Text.XML.Light as X
import qualified Text.XML.Light.Cursor as C

import Control.Exception
import Control.Monad
import Control.Monad.State.Strict hiding (sequence)
import Control.Monad.Except hiding (sequence)
import Data.Either
import Control.Applicative
import Prelude hiding (sequence)
import Control.Lens
import Text.Read (readMaybe)


-- | Monoid errors with path.
type XErrors = [([C.Tag],String)]

-- | Parsing monad.
newtype XParse a = XParse { unXParse :: StateT C.Cursor (Except XErrors) a }
    deriving (Functor,Applicative,Monad,MonadState C.Cursor,MonadError XErrors,Alternative)


-- | Run monad.
runXParse :: X.Element -> XParse a -> Either XErrors a
runXParse e act = runExcept (evalStateT (unXParse act) (C.fromElement e))

-- LENSES

lcurrent :: Lens' C.Cursor X.Content
lcurrent f s = fmap (\a -> s { C.current = a}) (f (C.current s))

_Elem :: Prism' X.Content X.Element
_Elem = prism X.Elem $ \c -> case c of X.Elem e -> Right e; _ -> Left c

-- | Parse failure.
xfail :: String -> XParse a
xfail msg = do
  ts <- map (view _2) . C.parents <$> get
  throwError [(ts,msg)]

-- | Require 'Just' a thing.
require :: String -> Maybe a -> XParse a
require msg = maybe (xfail $ "Required: " ++ msg) return

-- | Operate on attribute value/
xattr :: X.QName -> XParse String
xattr n = xel >>= require ("attribute " ++ show n) . X.findAttr n

-- | Operate on an element.
xel :: XParse X.Element
xel = firstOf _Elem <$> use lcurrent >>= require "element"

-- | Operate on text.
xtext :: XParse String
xtext = X.strContent <$> xel

-- | Consume a child element.
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

-- | Parse with `read`.
xread :: Read a => String -> String -> XParse a
xread msg s = require (msg ++ ": " ++ s) $ readMaybe s




-- | XSD name.
xsName :: String -> X.QName
xsName n = X.QName n (Just "http://www.w3.org/2001/XMLSchema") (Just "xs")

-- | Local name.
name :: String -> X.QName
name n = X.QName n Nothing Nothing

-- | Convenience to read in top element from file.
readXml :: FilePath -> IO X.Element
readXml f = maybe (throwIO $ userError "parse failed") return =<< X.parseXMLDoc <$> readFile f
