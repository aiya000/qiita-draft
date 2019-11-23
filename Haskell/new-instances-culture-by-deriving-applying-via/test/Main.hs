-- 大事なところ
module Main where

import Data.Foo
import Mine.Somewhere

-- Bazを期待する
main :: IO ()
main = print $ (Baz <> Bar) == Baz
