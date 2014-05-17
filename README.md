editable
========

Derive editors for data types.

Just add `deriving Generic` and `instance Editable Foo` to your
data type, and you can spawn an editor for it with
`editor :: Editable a => a -> IO a`.

```hs
{-# LANGUAGE DeriveGeneric #-}
module Demo where

import Data.Editable
import GHC.Generics

data Foo = Bar String Int | Baz Int
  deriving (Show, Generic)

instance Editable Foo
```

![example](https://pbs.twimg.com/media/Bn0q3K1IYAAZ7xY.jpg)
