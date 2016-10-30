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
--nezFile = endBy line eol

--line = sepBy (productionRule <|> comment) (char '\n')

--productionRule = do
--  ruleID <- many (noneOf " ,=\n")
--  spaces
--  expr <- nezExpression
--  return Just $ (ruleID, nezExpression)

--comment = (string "/*" >> manyTill anyChar ((try (string "*/") >> return Nothing <|> eof) >> spaces >> return Nothing

--nezExpression = do
--  return expression

--eol = char '\n'
