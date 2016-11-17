module NezType where

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

{-Nez Type definition-}
type NEZType = (String, Ty)

data Ty = Ttoken String String -- (tagName, expression)
        | Tlist String Ty
        | Ttuple String [Ty]
        | Ttree String [(String,Ty)] -- (tagName, [(label, Ty)])
        | Toption Ty
        | Tunion [Ty]
        | Trecursive Ty
        | Tname String
        deriving (Eq, Show)

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

stmt :: Parser NEZType
stmt =  try exampleStatement
    <|> try importStatement
    <|> try formatStatement
    <|> try templateStatement
    <|> production
    <?> "statement"

importStatement :: Parser NEZType
importStatement = do
  nezReserved "import"
  name <- nezCommaSep1 nezIdentifier
  nezReserved "from"
  nezIdentifier
  return ("import", Tname (concat name))--fixme or remove me

production :: Parser NEZType
production = do
  many (nezReserved "public" <|> nezReserved "inline")
  name <- nezIdentifier
  nezReservedOp "="
  nezExpression <- expression
  return ('T':name, nezExpression)

expression :: Parser Ty -- sub tree operator $ is removed from table
expression = buildExpressionParser table term <?> "expression"
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
        <|> (nezIdentifier >>= Tname )

constructor :: Parser Ty
constructor =



typingFunc2List :: Ty -> Ty
typingFunc2List s = case s of
  (Ttoken tag peg) -> Tlist tag s
  (Tlist tag ty) -> s
  (Ttuple tag tys) -> Tlist tag s
  (Ttree tag sub) -> Tlist tag s
  (Toption ty) -> Tlist [] s
  (Tunion tys) -> Tlist [] s
  (Trecursive ty) -> Tlist [] s
  (Tname name) -> Tlist [] s

typingFunc2ZeroMore :: Ty  -> Ty
typingFunc2ZeroMore s = case s of
  (Ttoken tag peg) -> Ttuple tag [s, Tlist tag s]
  (Tlist tag ty) -> s
  (Ttuple tag tys) -> Ttuple tag [s, Tlist tag s]
  (Ttree tag sub) -> Ttuple tag [s. Tlist tag s]
  (Toption ty) -> Tlist [] ty
  (Tunion tys) -> Ttuple [] [s, Tlist [] s]
  (Trecursive ty) -> Ttuple [] [s, Tlist [] s]
  (Tname name) -> Ttuple [] [s, Tlist [] s]

typingFunc2Optional :: Ty -> Ty
typingFunc2Optional s = case s of
  Toption ty -> s
  _ -> Toption s

typingFunc2Tuple :: Ty -> Ty -> Ty
typingFunc2Tuple (Ttuple tag1 tys1) t2 = Ttuple tag1 (tys1 ++ [t2])
typingFunc2Tuple t1 (Ttuple tag2 tys2) = Ttuple tag2 (t1 :: tys2)
typingFunc2Tuple t1 t2 = Ttuple [] [t1,t2]

typingFunc2Union :: Ty -> Ty -> Ty
typingFunc2Union (Tunion tys) t2 = Tunion tys ++ [t2]
typingFunc2Union t1 (Tunion tys) = Tunion t1 :: tys
typingFunc2Union t1 t2 = Tunion t1 t2

typingFunc4Constructor :: Ty -> Ty
typingFunc4Constructor (Tunion tys) = 
