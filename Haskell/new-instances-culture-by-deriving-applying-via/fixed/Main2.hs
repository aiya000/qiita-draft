{-# LANGUAGE ApplyingVia #-}

module Main where

import Data.Foo (Foo (..))
import Data.Foo.Baring (veryVeryBenriBaring)
import Data.Foo.Bazing (veryVeryBenriBazing, Bazing(..))

main :: IO ()
main = do
  print $ veryVeryBenriBaring Baz
  print $ veryVeryBenriBazing Bar
  print $ (<>) @(via Bazing) Baz Bar == Baz
