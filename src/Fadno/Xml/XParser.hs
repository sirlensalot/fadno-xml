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
    ,atEl,findChild,findChildren,anyChildren,oneChild,allChildren,manyOrdered
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
import Control.Lens

-- Element lenses
lAttrs :: Lens' X.Element [X.Attr]
lAttrs f s = fmap (\a -> s { X.elAttribs = a }) (f $ X.elAttribs s)
lContent :: Lens' X.Element [X.Content]
lContent f s = fmap (\a -> s { X.elContent = a }) (f $ X.elContent s)

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
