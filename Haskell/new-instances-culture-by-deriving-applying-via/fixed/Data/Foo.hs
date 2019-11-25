-- あるライブラリfooで定義されたモジュール
module Data.Foo where

data Foo = Bar | Baz
  deriving (Show, Eq)

-- Barを優先する実装
instance Semigroup Foo where
  _ <> Bar = Bar
  Bar <> _ = Bar
  _ <> _   = Baz

-- importしたい関数
veryVeryBenri :: Foo -> Foo
veryVeryBenri _ = Bar
