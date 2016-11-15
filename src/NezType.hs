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

production :: Parser NezType
production = do
  many (nezReserved "public" <|> nezReserved "inline")
  name <- nezIdentifier
  nezReservedOp "="
  nezExpression <- expression
  return ('T':name, nezExpression)

expression :: Parser Ty
expression = buildExpressionParser table term <?> "expression"
  where
    table = [[Suffix (nezReservedOp "*" >> return . typingFunc2List)]
           , [Suffix (nezReservedOp "+" >> return . typingFunc2ZeroMore)]
           , [Suffix (nezReservedOp "?" >> return . typingFunc2Optional)]
           , [Prefix (nezReservedOp "&" >> return)]
           , [Prefix (nezReservedOp "!" >> return)]
           , [Prefix (nezReservedOp "$" >> return)] -- fixme
           ]

typingFunc2List :: Ty -> Ty
typingFunc2List (Ttoken tag peg) = Tlist tag (Ttoken tag peg)
typingFunc2List (Tlist tag ty) = Tlist tag ty
typingFunc2List (Ttuple tag tys) = Tlist tag (Ttuple tag tys)
typingFunc2List (Ttree tag sub) = Tlist tag (Ttree tag sub)
typingFunc2List (Toption ty) = Tlist [] (Toption ty)
typingFunc2List (Tunion tys) = Tlist [] (Tunion tys)
typingFunc2List (Trecursive ty) = Tlist [] (Trecursive ty)
typingFunc2List (Tname name) = Tlist [] (Tname name)

typingFunc2ZeroMore :: Ty  -> Ty
typingFunc2ZeroMore (Ttoken tag peg) = Ttuple tag [Ttoken tag peg, Tlist tag (Ttoken tag peg)]
typingFunc2ZeroMore (Tlist tag ty) = Tlist tag ty
typingFunc2ZeroMore (Ttuple tag tys) = Ttuple tag [Ttuple tag tys, Tlist tag (Ttuple tag tys)]
typingFunc2ZeroMore (Ttree tag sub) = Ttuple tag [Ttree tag sub, Tlist tag (Ttree tag sub)]
typingFunc2ZeroMore (Toption ty) = Tlist [] ty
typingFunc2ZeroMore (Tunion tys) = Ttuple [] [Tunion tys, Tlist [] (Tunion tys)]
typingFunc2ZeroMore (Trecursive ty) = Ttuple [] [Trecursive ty, Tlist [] (Trecursive ty)]
typingFunc2ZeroMore (Tname name) = Ttuple [] [Tname name, Tlist [] (Tname name)]

typingFunc2Optional :: Ty -> Ty
typingFunc2Optional (Toption ty) = Toption ty
typingFunc2Optional ty = Toption ty



{-old expression
nezExpression :: Parser Ty
nezExpression =  do
  choiceElements <- sepBy1 nezSequence (nezReservedOp "/")
  return $ Tunion choiceElements

nezSequence :: Parser Ty
nezSequence = do
  seqElements <- sepBy1 innar nezWhiteSpace

  return expression
-}
