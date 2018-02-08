module TypedNez (

)where

import Control.Monad.State

data Exp = Empty
         | Term Char  
         | Seq {e1 :: Exp, e2 :: Exp}
         | Alt {e1 :: Exp, e2 :: Exp}
         | Not {e :: Exp}
         | SetOfLeafConstr
         | SetOfNodeConstr
         | None
         | Plus [Xi] ----SetOfNodeConstructor
         | Sub  [Xi]
         | Mul  [Xi]
         | To_Int Exp ----SetOfLeafConstr
         | To_String Exp 
         deriving (Eq, Ord, Show)

data Xi = Link {label :: String, child :: Exp}
        | Abs Exp
        deriving (Eq, Ord, Show)


----Embeded DSL------
(/>) :: Exp -> Exp -> Exp
e1 /> e2 = Alt e1 e2

ε :: Exp
ε = Empty

(~>) :: Exp -> Exp -> Exp
e1 ~> e2 = Seq e1 e2
-----------------------

--GrammarDefinition-----
math :: Exp
math = (Plus [Link{label = "right", child = num}, (Abs (Term '+')), Link {label = "left", child = num}])
     /> (Sub [Link{label = "right", child = num}, (Abs (Term '-')), Link {label = "left", child = num}])
     /> (Mul [Link{label = "right", child = num}, (Abs (Term '*')), Link {label = "left", child = num}])

num :: Exp
num = To_Int $ Term '0' /> Term '1'
-------------------------

type ErrorCode = String

data AST = VPlus (String -> AST)
         | VSub (String -> AST)
         | VMul (String -> AST)
         | VInt Int
         | InterMediate String
         | VNone
         | Fail ErrorCode


marge :: AST -> AST -> Maybe AST
marge (InterMediate s1) (InterMediate s2) = Just . InterMediate $ s1 ++ s2
marge _ _ = Nothing

type Parser = State String AST

parserGenerator :: Exp -> Parser
parserGenerator Empty = return $ InterMediate []
parserGenerator (Term c) = get >>= \s -> if head s == c then put (tail s) >> return (InterMediate [head s]) else return $ Fail $ "Unmatched " ++ (head s) ++ "with expected charactor" ++ [c]
parserGenerator (Seq a b) = get >>= \s -> let (v1,s1) = runState (parserGenerator a) s in
    case v1 of
        Fail err -> return $ Fail err 
        _ -> let (v2, s2) = runState (parserGenerator b) s1 in
            case v2 of
                Fail err' -> return $ Fail err'
                _ -> put s2 >> case marge v1 v2 of
                                Just x -> return x
                                Nothing -> return $ Fail $ "\n(Error Message) Marge Error in Seqence. The string " ++ s2 ++ "is not readed"

