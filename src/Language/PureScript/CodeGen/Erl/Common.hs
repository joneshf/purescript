-- |
-- Common code generation utility functions
--
module Language.PureScript.CodeGen.Erl.Common
( runAtom
, atomPS
, atom
, atomModuleName
, ModuleType(..)
, toAtomName
, toVarName
, identToAtomName
, identToVar
, nameIsErlReserved
, utf8Binary
, encodeChar
) where

import Prelude.Compat hiding (all)
import Data.Char
import Data.Text (Text, intercalate, uncons, cons, singleton, all, pack, singleton)
import qualified Data.Text as T
import Data.Word (Word16)
import Data.Monoid ((<>))
import Language.PureScript.Names
import Language.PureScript.PSString
import Numeric

import Language.PureScript.CodeGen.Erl.AST

runAtom :: Atom -> Text
runAtom at = case at of
  Atom (Just q) a -> atom q <> ":" <> atom a
  Atom Nothing a -> atom a
  AtomPS (Just q) a -> atom q <> ":" <> atomPS a
  AtomPS Nothing a -> atomPS a

-- Atoms do not support codepoints > 255
atomPS :: PSString -> Text
atomPS a = atom $ foldMap escapeChar (toUTF16CodeUnits a)
  where
    escapeChar :: Word16 -> Text
    escapeChar c | c > 0xFF = "@x" <> hex 4 c -- Can't use real unicode escape
    escapeChar c | c > 0x7E || c < 0x20 = "\\x" <> hex 2 c
    escapeChar c = replaceBasicEscape $ toChar c

replaceBasicEscape :: Char -> Text
replaceBasicEscape '\b' = "\\b"
replaceBasicEscape '\t' = "\\t"
replaceBasicEscape '\n' = "\\n"
replaceBasicEscape '\v' = "\\v"
replaceBasicEscape '\f' = "\\f"
replaceBasicEscape '\r' = "\\r"
replaceBasicEscape '"'  = "\\\""
replaceBasicEscape '\\' = "\\\\"
replaceBasicEscape c = singleton c

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral

hex :: (Enum a) => Int -> a -> Text
hex width c =
  let hs = showHex (fromEnum c) "" in
  pack (replicate (width - length hs) '0' <> hs)

utf8Binary :: PSString -> Text
utf8Binary str = "\"" <> T.concat (convertChar <$> decodeStringEither str) <> "\"/utf8"
  where
    convertChar :: Either Word16 Char -> Text
    convertChar (Left _) = "\\x{fffd}"
    convertChar (Right c) = replaceBasicEscape c

encodeChar :: Word16 -> Text
-- encodeChar c | c > 0xFF = "\\x{" <> T.pack (showHex (fromEnum c) "") <> "}"
encodeChar c | c > 0x7E || c < 0x20 =
  let hs = showHex (fromEnum c) ""
      x = T.pack (replicate (2 - length hs) '0' <> hs)
  in "\\x" <> x
encodeChar c = replaceBasicEscape $ toChar c

-- Atoms:
-- Must consist entirely of valid Latin-1 characters
-- Unquoted: must start with lower case char and be alpha-numeric, @ or _
-- Quoted: enclosed in single quotes, single quotes must be escaped \'
atom :: Text -> Text
atom s
  | isValidAtom s = s
  | otherwise = "'" <> T.concatMap replaceChar s <> "'"
  where
  replaceChar '\'' = "\\'"
  replaceChar c | not (isLatin1 c) = "@x" <> hex 4 c
  replaceChar c = singleton c

  isValidAtom a = case uncons a of
    Nothing -> False
    Just (fc, _) -> isLower fc && all atomChar a && not (nameIsErlReserved a)

  atomChar '_' = True
  atomChar '@' = True
  atomChar c = isDigit c || (isLatin1 c && isAlpha c)

data ModuleType = ForeignModule | PureScriptModule

atomModuleName :: ModuleName -> ModuleType -> Text
atomModuleName (ModuleName pns) moduleType = intercalate "_" ((toAtomName . runProperName) `map` pns) <>
  case moduleType of
    ForeignModule -> "@foreign"
    PureScriptModule -> "@ps"

toAtomName :: Text -> Text
toAtomName text = case uncons text of
  Just (h, t) -> cons (toLower h) t
  Nothing -> text

toVarName :: Text -> Text
toVarName v = case uncons v of
  Just (h, t) ->
    replaceFirst h <> T.concatMap replaceChar t
  Nothing -> v
  where
    replaceChar '.' = "@_"
    replaceChar '$' = "@dollar"
    replaceChar '\'' = "@prime"
    replaceChar c | not (isLatin1 c) = "@x" <> hex 4 c
    replaceChar x = singleton x

    replaceFirst x
      | isAlpha x && isLatin1 x = singleton (toUpper x)
      | x == '_' = "_"
      | otherwise = "V@1" <> replaceChar x

identToAtomName :: Ident -> Text
identToAtomName = toAtomName . runIdent

identToVar :: Ident -> Text
identToVar = toVarName . runIdent


-- |
-- Checks whether an identifier name is reserved in Erlang.
--
nameIsErlReserved :: Text -> Bool
nameIsErlReserved name =
  name `elem` erlAnyReserved

erlAnyReserved :: [Text]
erlAnyReserved = [
  "after",
  "and",
  "andalso",
  "band",
  "begin",
  "bnot",
  "bor",
  "bsl",
  "bsr",
  "bxor",
  "case",
  "catch",
  "cond",
  "div",
  "end",
  "fun",
  "if",
  "let",
  "not",
  "of",
  "or",
  "orelse",
  "receive",
  "rem",
  "try",
  "when",
  "xor"
  ]
