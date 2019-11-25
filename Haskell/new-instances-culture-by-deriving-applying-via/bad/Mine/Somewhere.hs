-- 自分のプロジェクトの、あるモジュール
module Mine.Somewhere where

import Data.Foo

-- Bazを優先する実装（orphan instance）
instance Semigroup Foo where
  Baz <> _ = Baz
  _ <> Baz = Baz
  _ <> _   = Bar
