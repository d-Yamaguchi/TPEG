module NezReader where


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P


---Math-----------
lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { reservedOpNames = ["*", "/", "+", "-"] })

natural     = P.natural lexer
parens      = P.parens lexer
reservedOp  = P.reservedOp lexer

expr :: Parser Integer
expr = buildExpressionParser table term <?> "expression"
    where
      table = [[unary "-" negate],
               [binop "*" (*) AssocLeft, binop "/" div AssocLeft],
               [binop "+" (+) AssocLeft, binop "-" (-) AssocLeft]]
      binop s op assoc = Infix (do{ reservedOp s; return op } <?> "operator") assoc
      unary s op = Prefix (do{ reservedOp s; return op })

term :: Parser Integer
term =
    do {
      parens expr;
    } <|> do {
      n <- natural;
      return n
    } <?>
      "term"

stmt :: Parser Integer
stmt = do
    e <- expr
    eof
    return e

---csvFile----------------------

csvFile = endBy line eol

line = sepBy cells (char ',')

cells = many (noneOf ",\n")

eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

---JSON--------------------------
