module NezType where

import Control.Arrow
import Text.ParserCombinators.Parsec

type Tag = String

data Token = Epsilon
           | TString String
           | Any
           deriving (Eq,Show)

type LabeledTree = (String, Value)

getLabel :: LabeledTree -> String
getLabel = fst

data Value = VToken Tag Token -- VToken {ty :: Tag, value :: Token}
           | VTuple Tag [Value] -- VTuple {ty :: Tag, value :: [Value]}
           | VChoice Tag [Value] -- VChoice {ty :: Tag, value :: [Value]}
           | VOption Tag Value -- VOption {ty :: Tag, value :: Value}
           | VList Tag Value -- VList {ty :: Tag, value :: Value}
           | VTree Tag [LabeledTree] -- | VTree {ty :: Tag, value :: [LabeledTree]}
           deriving (Eq,Show)

type TypedSubNode = (String, Ty)

data Ty = TToken Tag Token
        | TTuple Tag [Ty]
        | TChoice Tag [Ty]
        | TOption Tag Ty
        | TList Tag Ty
        | TTree Tag [TypedSubNode]
        deriving (Eq,Show)

typingFunc :: Value -> Ty
typingFunc (VToken tag token) = TToken tag token
typingFunc (VTuple tag vs) = TTuple tag (fmap typingFunc vs)
typingFunc (VChoice tag vs) = TChoice tag (fmap typingFunc vs)
typingFunc (VOption tag v) = TOption tag (typingFunc v)
typingFunc (VList tag v) = TList tag (typingFunc v)
typingFunc (VTree tag lts) = TTree tag (fmap (second typingFunc) lts)

-- Parser ----------------------------
nezFile = endBy line eol

line =  try (productionRule)
    <|> try comment
    <?> "Lines"

comment = (string "/*" >> manyTill anyChar ((try (string "*/") >> return Nothing <|> eof) >> spaces >> return Nothing

productionRule = do
  ruleID <- many (noneOf " ,=\n")
  spaces
  char '='
  spaces
  expr <- nezExpression
  return Just $ (ruleID, expr)

nezExpression =  try (tagedTree)
             <|> try (tagedTuple)
             <|> try (tagedChoice)
             <?> "nez expression"

tagedTree =  try (leftFolding)
         <|> try (normalTree)
         <?> "taged Tree"

normalTree = do
  char '{'
  manyTill anyChar (char '$')
  labelid <- manyTill anyChar (char '(')
  char '#'
  tag <- manyTill anyChar (space <|> char '}')
  return $ VTree tag [(labelid, )]

eol =   string "\n\n"
    <?> "end of line"
