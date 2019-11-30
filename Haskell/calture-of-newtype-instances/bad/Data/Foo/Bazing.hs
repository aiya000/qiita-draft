module Data.Foo.Bazing where

import Data.Foo

-- Bazを優先する実装
instance Semigroup Foo where
  Baz <> _ = Baz
  _ <> Baz = Baz
  _ <> _   = Bar

-- importしたい関数
veryVeryBenriBazing :: Foo -> Foo
veryVeryBenriBazing _ = Baz
