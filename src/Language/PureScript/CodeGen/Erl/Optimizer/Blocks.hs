-- |
-- Optimizer steps for simplifying Javascript blocks
--
module Language.PureScript.CodeGen.Erl.Optimizer.Blocks
  ( collapseNestedBlocks
  ) where

import Prelude.Compat

import Language.PureScript.CodeGen.Erl.AST

-- |
-- Collapse blocks which appear nested directly below another block
--
collapseNestedBlocks :: Erl -> Erl
collapseNestedBlocks = everywhereOnErl collapse
  where
  collapse :: Erl -> Erl
  collapse (EBlock sts) =
    case concatMap go sts of
      [s] -> s
      sts' -> EBlock sts'
  collapse js = js
  go :: Erl -> [Erl]
  go (EBlock sts) = sts
  go s = [s]
