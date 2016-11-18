module NezReader where

import Control.Applicative ((<*))
import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

{-Nez Type definition-}
data NezAST = Source [NezAST]
            | Grammar [NezAST]
            | Production [NezAST]
            | Choic [NezAST]
            | Sequence [NezAST]
            | And NezAST
            | Not NezAST
            | Rep NezAST
            | RepOne NezAST
            | Option NezAST
            | Foldtree [NezAST]
            | Tree [NezAST]
            | LinkTree [NezAST]
            | Link [NezAST]
            | Import String
            | Name String
            | Tagging String

{-parser definition-}
languageDefinition = javaStyle{ reservedOpNames = ["<", ">", "=", "/", "$", "-", "&", "!", "{$"]
                              , reservedNames = ["public", "inline", "import", "from", "grammar", "example", "template", "format", "true", "false", "define"]
                            }

tokenParser = makeTokenParser languageDefinition

nezParens = parens tokenParser
nezBraces = braces tokenParser
nezBrackets = brackets tokenParser
nezSquare = squares tokenParser
nezSemiColon = semi tokenParser
nezComma = comma tokenParser
nezDot = dot tokenParser
nezSemiSep1 = semiSep1 tokenParser
nezCommaSep1 = commaSep1 tokenParser
nezIdentifier = identifier tokenParser
nezReservedOp = reservedOp tokenParser
nezReserved = reserved tokenParser
nezWhiteSpace = whiteSpace tokenParser
nezHexdigit = hexDigit tokenParser

nezName :: Parser NezAST
nezName = do
  idn <- nezIdentifier
  return $ Name idn

start :: Parser [NezAST]
start = endBy1 (expression <|> source) eof

source :: Parser NezAST
source = stmt >>= (\sub -> return (Source [sub]))

stmt :: Parser NezAST -- removed examplestmt
stmt = try grammar <|> try importStmt <|> production

grammar :: Parser NezAST
grammar = do
  nezReserved "grammar"
  name <- nezName
  sour <- nezBraces source
  return $ Grammar [name,sour]

importStmt :: Parser NezAST
importStmt = do
  nezReserved "import"
  name <- nezIdentifier
  return $ Import name

production :: Parser NezAST
production = do
  optional (nezReserved "public")
  name <- nezName
  nezReservedOp "="
  expr <- expression
  return $ Production [name, expr]

expression  = buildExpressionParser table term
  where
    table = [[Postfix (nezReservedOp "*" >> (\a -> return Rep a))]
           , [Postfix (nezReservedOp "+" >> (\a -> return RepOne a))]
           , [Postfix (nezReservedOp "?" >> (\a -> return Choic a))]
           , [Prefix (nezReservedOp "&" >> (\a -> return And a))]
           , [Prefix (nezReservedOp "!" >> (\a -> return Not a))]
           , [Infix  (nezWhiteSpace >> (\a b -> return Sequence [a, b])) AssocLeft]
           , [Infix  (nezReservedOp "/" >> (\a b -> return Choic [a, b])) AssocLeft]
           ]
    term = try (nezDot >> return Name ".")
        <|> try (nezParens expression)
        <|> try (nezSquare many1 anyChar >>= (\a -> return Name a) )
        <|> try (string "\\0x" >> nezHexdigit >>= (\a -> return Name a))
        <|> try (string "\\U+" >> nezHexdigit >>= (\a -> return Name a))
        <|> try constructor
        <|> try constructorL
        <|> try link
        <|> try tagging
        <|> try replace
        <|> nezName

constructor :: Parser NezAST
constructor = do
  expr <- nezBraces expression
  return $ Tree expr

constructorL :: Parser NezAST
constructorL = do
  nezReservedOp "{$"
  name <- nezName
  expr <- nezBraces expression
  return $ Foldtree [name, expr]

link :: Parser NezAST
link = do
  nezReservedOp "$"
  label <- nezName
  linkInnar <- (nezParens expression)
  return $ Link [label, linkInnar]

linkTree :: Parser NezAST
linkTree = do
  nezReservedOp "$"
  label <- nezName
  linkInnar <- (nezBraces expression)
  return $ LinkTree [label, linkInnar]

tagging :: Parser NezAST
tagging = do
  nezReservedOp "#"
  name <- nezIdentifier
  return $ Tagging name

replace :: Parser NezAST
replace = between (symbol "'") (symbol "'") (anyChar) >>=　(\a -> return Name [a])
