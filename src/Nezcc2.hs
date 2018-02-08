module Nezcc2 (
    result
)
where

data AST =  VInt Int
         | InterMediate String
         | VNone
         deriving (Show)


ex :: ParserContext
ex = Succ {inputs = "aaa", pos = 0, tree = VNone}

result = char1 'a' ex

data ParserContext = Fail
                   | Succ {inputs :: String, pos :: Int, tree :: AST}
                  deriving (Show)

neof :: ParserContext -> ParserContext
neof Fail = Fail
neof px    = if (pos px) < ((length . inputs) px) then px else Fail
--neof(px) =
--    px.pos < px.length

mnext1 :: ParserContext -> ParserContext
mnext1 Fail = Fail
mnext1 px = px {pos = (pos px) + 1}
--mnext1(px) =
--    px.pos = px.pos+1
--    true

char1 :: Char -> ParserContext -> ParserContext
char1 _ Fail = Fail
char1 ch px = if ((inputs px) !! (pos px)) == ch then mnext1 px else Fail
--char1(px, ch) =
--    px.inputs[px.pos] == ch && mnext1(px)

mback1 :: ParserContext -> Int -> ParserContext
mback1 Fail _ = Fail
mback1 px pos = px {pos = pos}
--mback1(px, pos) =
--    px.pos = pos
--    true

mback3 :: ParserContext -> Int -> AST -> ParserContext
mback3 Fail _ _ = Fail
mback3 px pos tree = px {pos = pos, tree = tree}
--mback3(px, pos, tree) =
--    px.pos = pos
--    px.tree = tree
--    true

choice3 :: ParserContext -> (ParserContext -> ParserContext) -> (ParserContext -> ParserContext) -> ParserContext
choice3 Fail _ _ = Fail
choice3 px e1 e2 = let cpos = pos px in
                   let ctree = tree px in
                   case e1 px of
                    Fail -> e2 $ mback3 px cpos ctree
                    x -> x
