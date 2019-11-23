{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

-- 自分のプロジェクトの、あるモジュール
module Mine.Somewhere where

import Data.Foo

newtype Bazing = Bazing
  { unBazing :: Foo
  } deriving (Eq) via (Foo)

-- Bazを優先する実装（orphan instance）
instance Semigroup Bazing where
  (Bazing Baz) <> _ = Bazing Baz
  _ <> (Bazing Baz) = Bazing Baz
  _ <> _            = Bazing Bar
