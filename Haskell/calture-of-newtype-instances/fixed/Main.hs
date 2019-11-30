-- 大事なところ
module Main where

import Data.Foo (Foo (..))
import Data.Foo.Baring (veryVeryBenriBaring)  -- Baringはhidingしておく
import Data.Foo.Bazing (veryVeryBenriBazing, Bazing(..))

main :: IO ()
main = do
  print $ veryVeryBenriBaring Baz  -- 必要な処理
  print $ veryVeryBenriBazing Bar  -- 必要な処理
  print $ (Bazing Baz <> Bazing Bar) == Bazing Baz  -- Bazを期待する
