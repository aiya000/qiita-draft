-- 大事なところ
module Main where

import Data.Foo (Foo (..))
import Data.Foo.Baring (veryVeryBenriBaring)
import Data.Foo.Bazing (veryVeryBenriBazing)  -- `instance Semigroup Bazing`をimport

main :: IO ()
main = do
  print $ veryVeryBenriBaring Baz  -- 必要な処理
  print $ veryVeryBenriBazing Bar  -- 必要な処理
  print $ (Baz <> Bar) == Baz      -- Bazを期待する
