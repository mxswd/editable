{-# LANGUAGE DefaultSignatures, FlexibleContexts, ScopedTypeVariables, TypeOperators, DeriveGeneric, FlexibleInstances, ScopedTypeVariables, OverloadedStrings, UndecidableInstances, OverlappingInstances #-}
module Data.Editable (editor, Editable, Parseable(..)) where

import GHC.Generics
import Control.Applicative
import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Concurrent
import Text.Read
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Proxy
import Data.IORef

edit :: Parseable a => Maybe String -> Maybe String -> Maybe String -> a -> IO a
edit datatype fieldName pError initialV = do
  -- To stop VTY from catching GHCI's first enter keypress
  threadDelay 1

  isBottom <- newIORef False

  e <- editWidget
  setEditText e (T.pack (shower initialV))
  setEditCursorPosition (0, length (shower initialV)) e

  fg <- newFocusGroup
  _ <- addToFocusGroup fg e

  be <- bordered =<< boxFixed 40 1 e

  c <- centered =<< ((plainText     $"Data type:   " <> maybe "unknown" T.pack datatype)
                     <--> plainText ("Constructor: " <> maybe "unknown" T.pack fieldName)
                     <--> plainText ("Field type:  " <> (T.pack (typeName initialV)))
                     <--> plainText (maybe "" (T.pack . (++) "Parse error: ") pError)
                     <--> (return be)
                     <--> plainText "Push ESC to use ⊥."
                     >>= withBoxSpacing 1 )

  coll <- newCollection
  _ <- addToCollection coll c fg


  fg `onKeyPressed` \_ k _ ->
    case k of
      KEsc -> shutdownUi >> writeIORef isBottom True >> return True
      KEnter -> shutdownUi >> return True
      _ -> return False

  runUi coll defaultContext

  isb <- readIORef isBottom
  if isb then return undefined
    else do
      res <- T.unpack `fmap` getEditText e
      case reader res of
        Right x -> return x
        Left e -> do
          edit datatype fieldName (Just $ "Failed to parse: " ++ show res ++ "\n" ++ e) initialV

-- Parseable
class Parseable a where
  reader :: String -> Either String a
  shower :: a -> String
  typeName :: a -> String

instance Parseable [Char] where
  reader = Right
  shower = id
  typeName _ = "String"

instance (Show a, Read a, Typeable a) => Parseable a where
  reader = readEither
  shower = show
  typeName = show . typeRep . proxy
    where
      proxy :: a -> Proxy a
      proxy _ = Proxy

-- Editable
class Editable a where
  editor :: a -> IO a

  default editor :: (Generic a, GEditable (Rep a)) => a -> IO a
  editor = fmap to . geditor Nothing Nothing . from

class GEditable f where
  geditor :: Maybe String -> Maybe String -> f a -> IO (f a)

instance (Parseable e) => GEditable (K1 i e) where
  geditor t c = fmap K1 . (\x -> edit t c Nothing x) . unK1

instance (GEditable e, Constructor c) => GEditable (M1 C c e) where
  geditor t _ x = fmap M1 . geditor t (Just $ conName x) $ unM1 x

instance (GEditable e, Datatype c) => GEditable (M1 D c e) where
  geditor _ c x = fmap M1 . geditor (Just $ datatypeName x) c $ unM1 x

instance (GEditable e, Selector c) => GEditable (M1 S c e) where
  geditor t c = fmap M1 . geditor t c . unM1

instance (GEditable b, GEditable c) => GEditable (b :*: c) where
  geditor t d (b :*: c) = do
    l <- geditor t d b
    r <- geditor t d c
    return (l :*: r)

instance (GEditable b, GEditable c) => GEditable (b :+: c) where
  geditor t c (L1 l) = fmap L1 $ geditor t c l
  geditor t c (R1 r) = fmap R1 $ geditor t c r

instance GEditable U1 where
  geditor _ _ U1 = do
    putStrLn "Editing () yields ()"
    return U1
