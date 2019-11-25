-- 大事なところ
module Main where

import Data.Foo
import Mine.Somewhere

main :: IO ()
main = do
  print $ veryVeryBenri Baz    -- 必要な処理
  print $ (Baz <> Bar) == Baz  -- Bazを期待する
