-- あるライブラリfooで定義されたモジュール
module Data.Foo where

data Foo = Bar | Baz
  deriving (Show)

-- Barを優先する実装
instance Semigroup Foo where
  _ <> Bar = Bar
  Bar <> _ = Bar
  _ <> _   = Baz

-- 絶対にimportしたい！　超便利な関数
veryVeryBenri :: Foo -> Foo
veryVeryBenri _ = Bar
