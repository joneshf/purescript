module TestPsci where

import Prelude ()
import Prelude.Compat

import Test.Hspec
import TestPsci.CompletionTest (completionTests)

main :: IO ()
main = hspec -- $ do
  completionTests
  -- TODO
  -- commandTests
  -- evalTests
