{-# LANGUAGE DerivingVia #-}

module Data.Foo.Baring where

import Data.Foo

newtype Baring = Baring
  { unBaring :: Foo
  } deriving (Eq) via Foo  -- Fooのうち必要な性質を、Bazingに抜き出す

-- Barを優先する実装
instance Semigroup Baring where
  _ <> (Baring Bar) = Baring Bar
  (Baring Bar) <> _ = Baring Bar
  _ <> _            = Baring Baz

-- importしたい関数
veryVeryBenriBaring :: Foo -> Foo
veryVeryBenriBaring _ = Bar
