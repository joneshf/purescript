-- |
-- This module provides collapsing of simple guard expressions
--
module Language.PureScript.CodeGen.Erl.Optimizer.Guards where

import Prelude.Compat

import Language.PureScript.CodeGen.Erl.AST

inlineSimpleGuards :: Erl -> Erl
inlineSimpleGuards = everywhereOnErl convert
  where
  convert :: Erl -> Erl
  convert (EBlock es) | not (null es) =
    case last es of
      EApp (EFunFull Nothing funbinders) vars ->
        let res = convert' vars funbinders (take (length es - 1) es) [] [] in
        let (funbinders', ebs) = res in
        EBlock (ebs ++ [EApp (EFunFull Nothing funbinders') vars])
      _ -> EBlock es
  convert other = other

  convert' vars (efb@(EFunBinder binds guard, e) : es)
    (eb@(EVarBind x (EApp (EFunFull Nothing [(EFunBinder binds' Nothing, eg), defBind]) vars')) : ebs)
    acc_es acc_ebs
    | binds == binds' && vars == vars' && isGuardVar x guard && isDefaultBinder defBind
    = if guardExpr eg then
        convert' vars es ebs ((EFunBinder binds (Just (Guard eg)), e) : acc_es) acc_ebs
      else
        convert' vars es ebs (efb:acc_es) (eb:acc_ebs)
  convert' vars (efb@(EFunBinder _ guard, _) : es) (eb@(EVarBind x _) : ebs) acc_es acc_ebs
    | not (isGuardVar x guard)
    = convert' vars es (eb:ebs) (efb:acc_es) acc_ebs
  convert' _ efb es acc_es acc_ebs = (reverse acc_es ++ efb, reverse acc_ebs ++ es)

  isGuardVar x (Just (Guard (EVar x'))) | x == x' = True
  isGuardVar _ _ = False

  isDefaultBinder (EFunBinder vars Nothing, EAtomLiteral (Atom Nothing "false"))
    | not (null vars) && all (EVar "_" ==) vars = True
  isDefaultBinder _ = False

  -- | Simple expression which can appear as a guard
  guardExpr (ENumericLiteral _) = True
  guardExpr (EStringLiteral _) = True
  guardExpr (ECharLiteral _) = True
  guardExpr (EAtomLiteral _) = True
  guardExpr (EUnary _ e) = guardExpr e
  guardExpr (EBinary _ e1 e2) = guardExpr e1 && guardExpr e2
  guardExpr (EVar _) = True
  guardExpr _ = False
