module Data.Foo.Baring where

import Data.Foo

-- Barを優先する実装
instance Semigroup Foo where
  _ <> Bar = Bar
  Bar <> _ = Bar
  _ <> _   = Baz

-- importしたい関数
veryVeryBenriBaring :: Foo -> Foo
veryVeryBenriBaring _ = Bar
