module TestPsci where

import Prelude ()
import Prelude.Compat

import Test.Hspec
import TestPsci.CommandTest (commandTests)
import TestPsci.CompletionTest (completionTests)

main :: IO ()
main = hspec $ do
  completionTests
  commandTests
  -- TODO
  -- evalTests
