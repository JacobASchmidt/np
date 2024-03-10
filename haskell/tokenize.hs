{-# LANGUAGE OverloadedStrings #-}

module NasperTokenizer where

import Data.Char (isDigit, isAlpha, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

data Token
  = Integer String
  | Float String
  | Identifier String
  | Plus | Minus | Times | Divide | Mod
  | Pipe | LParen | RParen
  | LBrace | RBrace | LBracket | RBracket
  | Comma | Dot | Ellipsis | Epsilon
  | Indent | UnIndent | Equals | Arrow | Assign
  | SingleUnderscore | Is | Not | In
  | Update | For | To | And | Or
  | Question | MaybeToken | Struct | Union | Type
  | Interface | Set | Module | Def | Const
  | If | Match | Case | Pound | Return
  deriving (Show, Eq)

tokenize :: Text -> Int -> [Token]
tokenize "" n = []
tokenize input tabs
  | T.isPrefixOf "+" input = Plus : tokenize (T.tail input) tabs
  | T.isPrefixOf "-" input = Minus : tokenize (T.tail input) tabs
  | T.isPrefixOf "*" input = Times : tokenize (T.tail input) tabs
  | T.isPrefixOf "/" input = Divide : tokenize (T.tail input) tabs
  | T.isPrefixOf "%" input = Mod : tokenize (T.tail input) tabs
  | T.isPrefixOf "|" input = Pipe : tokenize (T.tail input) tabs
  | T.isPrefixOf "(" input = LParen : tokenize (T.tail input) tabs
  | T.isPrefixOf ")" input = RParen : tokenize (T.tail input) tabs
  | T.isPrefixOf "{" input = LBrace : tokenize (T.tail input) tabs
  | T.isPrefixOf "}" input = RBrace : tokenize (T.tail input) tabs
  | T.isPrefixOf "[" input = LBracket : tokenize (T.tail input) tabs
  | T.isPrefixOf "]" input = RBracket : tokenize (T.tail input) tabs
  | T.isPrefixOf "," input = Comma : tokenize (T.tail input) tabs
  | T.isPrefixOf "..." input = Ellipsis : tokenize (T.drop 3 input) tabs
  | T.isPrefixOf "." input = Dot : tokenize (T.tail input) tabs
  | T.isPrefixOf "--" input = tokenize (T.dropWhile (/= '\n') input) tabs
  | T.isPrefixOf "==" input = Equals : tokenize (T.drop 2 input) tabs
  | T.isPrefixOf "=>" input = Arrow : tokenize (T.drop 2 input) tabs
  | T.isPrefixOf "=" input = Assign : tokenize (T.tail input) tabs
  | isDigit (T.head input) =
      let (num, rest) = T.span isDigit input
      in if T.isPrefixOf "." rest
         then let (frac, rest') = T.span isDigit (T.tail rest)
              in Float (T.unpack num <> "." <> T.unpack frac) : tokenize rest' tabs
         else Integer (T.unpack num) : tokenize rest tabs
  | isAlpha (T.head input) || T.head input == '_' =
      let (ident, rest) = T.span (\c -> isAlpha c || isDigit c || c == '_') input
      in case ident of
          "is" -> Is : tokenize rest tabs
          "not" -> Not : tokenize rest tabs
          "in" -> In : tokenize rest tabs
          "update" -> Update : tokenize rest tabs
          "for" -> For : tokenize rest tabs
          "to" -> To : tokenize rest tabs
          "and" -> And : tokenize rest tabs
          "or" -> Or : tokenize rest tabs
          "struct" -> Struct : tokenize rest tabs
          "union" -> Union : tokenize rest tabs
          "type" -> Type : tokenize rest tabs
          "interface" -> Interface : tokenize rest tabs
          "set" -> Set : tokenize rest tabs
          "module" -> Module : tokenize rest tabs
          "def" -> Def : tokenize rest tabs
          "const" -> Const : tokenize rest tabs
          "if" -> If : tokenize rest tabs
          "match" -> Match : tokenize rest tabs
          "case" -> Case : tokenize rest tabs
          "maybe" -> MaybeToken : tokenize rest tabs
          "return" -> Return : tokenize rest tabs
          _ -> Identifier (T.unpack ident) : tokenize rest tabs
  | isSpace (T.head input) =
      let spaces = T.takeWhile isSpace input
          numSpaces = T.length spaces
          numTabs = numSpaces `div` 4
          rest = T.dropWhile isSpace input
      in if numTabs > tabs
          then replicate (numTabs - tabs) Indent ++ tokenize rest numTabs
          else if numTabs < tabs
              then replicate (tabs - numTabs) UnIndent ++ tokenize rest numTabs
              else tokenize rest tabs
  | otherwise = tokenize (T.tail input) tabs -- Skip unknown character

-- Example usage
main :: IO ()
main = print $ tokenize "\
\if x == 4 \
\   return y \
\else\
\    print(1,2, 3)\
\    return 5\
\" 0
