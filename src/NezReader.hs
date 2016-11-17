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
            | Import String
            | Name String

--data NezAST' = NezAST {tag:: String, subNode:: NezAST}

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

nezName :: Parser NezAST
nezName = do
  idn <- nezIdentifier
  return $ Name idn

start :: Parser [NezAST]
start = endBy1 (expression <|> source) eof

source :: Parser NezAST
source = stmt >>= (\sub -> return (Source [sub]))

stmt :: Parser NezAST -- removed examplestmt
stmt = (try grammar <|> try importStmt <|> production)

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

expression = nezName -- stub
{-expressionparsr
expression  = buildExpressionParser table term
  where
    table = [[Suffix (nezReservedOp "*" >> return . typingFunc2List)]
           , [Suffix (nezReservedOp "+" >> return . typingFunc2ZeroMore)]
           , [Suffix (nezReservedOp "?" >> return . typingFunc2Optional)]
           , [Prefix (nezReservedOp "&" >> return)]
           , [Prefix (nezReservedOp "!" >> return)]
           , [Infix  (nezWhiteSpace >> return . typingFunc2Tuple) AssocLeft]
           , [Infix  (nezReservedOp "/" >> return . typingFunc2Union) AssocLeft]
           ]
    term = try (nezDot >> Ttoken [] ".")
        <|> try (nezParens expression)
        <|> try (nezSquare many1 anyChar >>= Tname )
        <|> try (string "\\0x" >> foldr (:) [] [nezHexdigit, nezHexdigit, nezHexdigit, nezHexdigit] >>= Ttoken [])
        <|> try (string "\\U+" >> foldr (:) [] [nezHexdigit, nezHexdigit] >>= Ttoken [])
        <|> try constructor
        <|> try constructorL
        <|> try tagging
        <|> (nezIdentifier >>= Tname )-}
