{-# LANGUAGE DeriveGeneric #-}
module Demo where

import Data.Editable
import GHC.Generics

data Foo = Bar String Int | Baz Int
  deriving (Show, Generic)

instance Editable Foo

main = do
  let before = Bar "Words" 4
  print before
  after <- editor before
  print after
