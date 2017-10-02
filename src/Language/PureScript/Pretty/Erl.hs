module Language.PureScript.Pretty.Erl where

import Prelude ()
import Prelude.Compat

import Control.Arrow ((<+>))
import Control.Monad.State hiding (sequence)
import Control.PatternArrows
import qualified Control.Arrow as A

import Language.PureScript.Crash
import Language.PureScript.CodeGen.Erl.AST
import Language.PureScript.CodeGen.Erl.Common
import Language.PureScript.Pretty.Common hiding (withIndent)
import Language.PureScript.PSString

import Data.Monoid
import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

withIndent :: StateT PrinterState Maybe gen -> StateT PrinterState Maybe gen
withIndent = withIndent' 2

literals :: (Emit gen) => Pattern PrinterState Erl gen
literals = mkPattern' match
  where

  match :: (Emit gen) => Erl -> StateT PrinterState Maybe gen
  match (ENumericLiteral n) = return $ emit $ T.pack $ either show show n
  match (EStringLiteral s) = return $ emit $ prettyPrintStringErl s
  match (ECharLiteral c) = return $ emit $ "$" <> encodeChar (fromChar c)
  match (EAtomLiteral a) = return $ emit $ runAtom a
  match (ETupleLiteral es) = do
    es' <- mapM prettyPrintErl' es
    return $ emit "{ " <> intercalate (emit ", ") es' <> emit " }"

  match (EVarBind x e) = mconcat <$> sequence
    [ return $ emit $ x <> " = "
    , prettyPrintErl' e
    ]
  match (EFunctionDef x xs e) = mconcat <$> sequence
    [ return $ emit $ runAtom x <> "(" <> intercalate "," xs <> ") -> "
    , prettyPrintErl' e
    ]

  match (EVar x) = return $ emit x

  match (EMapLiteral elts) = do
    elts' <- traverse (\(x,e) -> ((emit (runAtom x) <> emit "=>") <>) <$> prettyPrintErl' e) elts
    return $ emit "#{" <> intercalate (emit ", ") elts' <> emit "}"

  match (EMapPattern elts) = do
    elts' <- traverse (\(x,e) -> ((emit (runAtom x) <> emit ":=") <>) <$> prettyPrintErl' e) elts
    return $ emit "#{" <> intercalate (emit ", ") elts' <> emit "}"

  match (EMapUpdate e elts) = do
    e' <- prettyPrintErl' e
    elts' <- traverse (\(x,ee) -> ((emit (runAtom x) <> emit "=>") <>) <$> prettyPrintErl' ee) elts
    return $ emit "(" <> e' <> emit ")" <> emit "#{" <> intercalate (emit ", ") elts' <> emit "}"

  match (EArrayLiteral es) = mconcat <$> sequence
    [ return $ emit "["
    , intercalate (emit ", ") <$> mapM prettyPrintErl' es
    , return $ emit "]"
    ]

  match (EBlock es) =
    mconcat <$> sequence
      [ return $ emit "begin\n"
      , withIndent $ prettyPrintBlockBody es
      , return $ emit "\n"
      , currentIndent
      , return $ emit "end"
      ]

  match (EComment s) =
    mconcat <$> sequence
      [ return $ emit "\n"
      , currentIndent
      , return $ emit $ "% " <> s <> "\n"
      ]

  match (EFunRef x n) = return $ emit $ "fun " <> runAtom x <> "/" <> T.pack (show n)
  match (EFun name x e) = mconcat <$> sequence
    [ return $ emit $ "fun " <> fromMaybe "" name <> "(" <> x <> ") ->\n"
    , withIndent $ matchBody e
    , return $ emit "\n"
    , currentIndent
    , return $ emit "end"
    ]
    where
      matchBody (EBlock es) = prettyPrintBlockBody es
      matchBody _ = mconcat <$> sequence
          [ currentIndent
          , prettyPrintErl' e ]
  match (EFunFull name binders) = mconcat <$> sequence
    [ return $ emit "fun\n"
    , withIndent $ prettyPrintBinders binders
    , return $ emit "\n"
    , currentIndent
    , return $ emit "end"
    ]
    where
        prettyPrintBinders :: (Emit gen) => [(EFunBinder, Erl)] -> StateT PrinterState Maybe gen
        prettyPrintBinders bs = intercalate (emit ";\n") <$> mapM prettyPrintBinder bs

        matchBody (EBlock es) = prettyPrintBlockBody es
        matchBody e = mconcat <$> sequence
            [ currentIndent
            , prettyPrintErl' e ]

        prettyPrintBinder :: (Emit gen) => (EFunBinder, Erl) -> StateT PrinterState Maybe gen
        prettyPrintBinder (EFunBinder binds ge, e') = do
          b' <- intercalate (emit ", ") <$> mapM prettyPrintErl' binds
          v <- matchBody e'
          g' <- case ge of
            Just (Guard g) -> (emit " when " <>) <$> prettyPrintErl' g
            Nothing -> return $ emit ""
          indentStr <- currentIndent
          return $ indentStr <> emit (fromMaybe "" name) <> parensPos b' <> g' <> emit " -> \n" <> v --parensPos b' <> g' <> emit " -> " <> v

  match (ECaseOf e binders) = do
    e' <- prettyPrintErl' e
    bs <- prettyPrintBinders binders
    return $ emit "case " <> e' <> emit " of\n" <> bs <> emit "\nend"
    where
    prettyPrintBinders :: (Emit gen) => [(EBinder, Erl)] -> StateT PrinterState Maybe gen
    prettyPrintBinders bs = intercalate (emit ";\n") <$> mapM prettyPrintBinder bs

    prettyPrintBinder :: (Emit gen) => (EBinder, Erl) -> StateT PrinterState Maybe gen
    prettyPrintBinder (EBinder eb, e') = do
      c <- prettyPrintErl' eb
      v <- prettyPrintErl' e'
      return $ parensPos c <> emit " -> " <> v
    prettyPrintBinder (EGuardedBinder eb (Guard eg), e') = do
      c <- prettyPrintErl' eb
      v <- prettyPrintErl' e'
      g <- prettyPrintErl' eg
      return $ parensPos c <> emit " when "  <> g <> emit " -> " <> v

  match (EAttribute name text) = case (decodeString name, decodeString text) of
    (Just name', Just text') ->
      return $ emit "-" <> emit name' <> emit "(" <> emit text' <> emit ")"
      -- TODO: Check valid atom, cconvert etc
    _ -> internalError "Did not expect non UTF8 safe attribute text"

  match _ = mzero


fromChar :: Char -> Word16
fromChar = toEnum . fromEnum

prettyPrintBlockBody :: (Emit gen) => [Erl] -> StateT PrinterState Maybe gen
prettyPrintBlockBody es = do
  es' <- mapM prettyPrintErl' es
  indentStr <- currentIndent
  let lns = intercalate (emit ",\n" <> indentStr) es'
  pure $ indentStr <> lns

-- |
-- Pretty print a PSString, using Erlang escape sequences. Intended for
-- use in compiled Erlang output.
--
prettyPrintStringErl :: PSString -> Text
prettyPrintStringErl s = "<<" <> utf8Binary s <> ">>"

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral

app :: (Emit gen) => Pattern PrinterState Erl (gen, Erl)
app = mkPattern' match
  where
  match (EApp val args) = do
    jss <- traverse prettyPrintErl' args
    return (intercalate (emit ", ") jss, val)
  match _ = mzero

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState Erl gen
binary op str = AssocL match (\v1 v2 -> v1 <> emit (" " <> str <> " ") <> v2)
  where
  match :: Pattern PrinterState Erl (Erl, Erl)
  match = mkPattern match'
    where
    match' (EBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing


unary' :: (Emit gen) => UnaryOperator -> (Erl -> Text) -> Operator PrinterState Erl gen
unary' op mkStr = Wrap match (<>)
  where
  match :: (Emit gen) => Pattern PrinterState Erl (gen, Erl)
  match = mkPattern match'
    where
    match' (EUnary op' val) | op' == op = Just (emit $ mkStr val, val)
    match' _ = Nothing

unary :: (Emit gen) => UnaryOperator -> Text -> Operator PrinterState Erl gen
unary op str = unary' op (const str)

prettyPrintErl :: [Erl] -> Text
prettyPrintErl = maybe (internalError "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements


prettyStatements :: (Emit gen) => [Erl] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  jss <- forM sts prettyPrintErl'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map ((<> emit ".") . (indentString <>)) jss

-- |
-- Generate an indented, pretty-printed string representing a Javascript expression
--
prettyPrintErl' :: (Emit gen) => Erl -> StateT PrinterState Maybe gen
prettyPrintErl' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: (Emit gen) => Pattern PrinterState Erl gen
  matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)
  operators :: (Emit gen) => OperatorTable PrinterState Erl gen
  operators =
    OperatorTable [
        [ Wrap app $ \args val -> emit "(" <> val <> emit "(" <> args <> emit ")" <> emit ")"]
      , [ unary     Not                  "not"
        , unary     BitwiseNot           "bnot"
        , unary     Positive             "+"
        , unary     Negate               "-"
        ]
      , [ binary    FDivide              "/"
        , binary    IDivide              "div"
        , binary    Multiply             "*"
        , binary    Remainder            "rem"
        , binary    BitwiseAnd           "band"
        , binary    And                  "and"
        ]
      , [ binary    Add                  "+"
        , binary    Subtract             "-"
        , binary    BitwiseOr            "bor"
        , binary    BitwiseXor           "bxor"
        , binary    ShiftLeft            "bsl"
        , binary    ShiftRight           "bsr"
        , binary    Or                   "or"
        , binary    XOr                  "xor"
        ]
      , [ binary    EqualTo              "=="
        , binary    NotEqualTo           "/="
        , binary    IdenticalTo          "=:="
        , binary    NotIdenticalTo       "=/="
        , binary    LessThan             "<"
        , binary    LessThanOrEqualTo    "=<"
        , binary    GreaterThan          ">"
        , binary    GreaterThanOrEqualTo ">="
        ]
      , [ binary    AndAlso              "andalso"
        ]
      , [ binary    OrElse               "orelse"
        ]
    ]
