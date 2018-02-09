module Nezcc2 (
    runTestParsers
)
where


-- comment
-- (ParserContext -> ParserContext)の関数の部分適用&合成がやりづらいので関数の返り値は(ParserContext -> ParserContext)に統一して欲しい．


----for Debuging----
data ParseResult = Result {initalParserContext :: ParserContext, result :: ParserContext} deriving (Show)

ex0 :: ParserContext
ex0 = Succ {inputs = "aaa", pos = 0, tree = Empty}


runTestParsers = [ Result {initalParserContext = ex0, result = char1 'a' ex0}
                 , Result {initalParserContext = ex0, result = (char1 'a' . (char1 'a')) ex0}
                 ]
--------------------

data AST =  Empty
         | Label {tag :: String, child :: AST, prev :: AST}
         | Node {tag :: String, input :: String, spos :: Int, epos :: Int, astTree :: AST}
         deriving (Show)

data ParserContext = Fail
                   | Succ {inputs :: String, pos :: Int, tree :: AST}
                  deriving (Show)

neof :: ParserContext -> ParserContext
neof Fail = Fail
neof px    = if pos px < (length . inputs) px then px else Fail
--neof(px) =
--    px.pos < px.length

mnext1 :: ParserContext -> ParserContext
mnext1 Fail = Fail
mnext1 px = px {pos = pos px + 1}
--mnext1(px) =
--    px.pos = px.pos+1
--    true

char1 :: Char -> ParserContext -> ParserContext
char1 _ Fail = Fail
char1 ch px = if (inputs px !! pos px) == ch then mnext1 px else Fail
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

not3 :: ParserContext -> (ParserContext -> ParserContext) -> ParserContext
not3 Fail _ = Fail
not3 px e = let cpos = pos px in
            let ctree = tree px in
            case e px of
                Fail -> mback3 px cpos ctree
                x -> Fail
--not3(px, e) =
--pos = px.pos
--tree = px.tree
-- !e(px) && mback3(px,pos,tree)


----Tree construction---------

mtree :: ParserContext -> String -> Int -> Int -> ParserContext
mtree Fail _ _ _ = Fail
mtree px tag spos epos = px {tree = Node tag (inputs px) spos epos (tree px)}
-- mtree(px, tag, spos, epos) =
--  px.tree = Tree(tag, px.inputs, spos, epos, px.tree)
--  true

mlink :: ParserContext -> String -> AST -> AST -> ParserContext
mlink Fail _ _ _ = Fail
mlink px label child prev = px {tree = Label label child prev}
-- mlink(px, label, child, prev) =
--  px.tree = Link(tag, child, prev)
--  true

newtree :: ParserContext -> Int -> (ParserContext -> ParserContext) -> String -> Int -> ParserContext
newtree Fail _ _ _ _ = Fail
newtree px l e tag r = let cpos = pos px in
                       let cpx = px {tree = Empty} in
                       case e cpx of
                        Fail -> Fail
                        npx -> mtree npx tag (cpos+l) (pos npx + r)
-- newtree(px, l, e, tag, r) =
--  pos = px.pos
--  px.tree = EmptyTree
--  e(px) && mtree(px, tag, pos+l, px.pos+r)

linktree :: ParserContext -> String -> (ParserContext -> ParserContext) -> ParserContext
linktree Fail _ _ = Fail
linktree px label e = let prev = tree px in
                      case e px of
                        Fail -> Fail
                        npx -> mlink npx label (tree npx) prev
-- linktree(px, label, e) =
-- prev = px.tree
-- e(px) && mlink(px, label, px.tree, prev)

foldtree :: ParserContext -> String -> Int -> (ParserContext -> ParserContext) -> String -> Int -> ParserContext
foldtree Fail _ _ _ _ _ = Fail
foldtree px label l e tag r = let cpos = pos px in
    case mlink px label (tree px) Empty of
        Fail -> Fail
        npx -> case e npx of
            Fail -> Fail
            nnpx -> mtree nnpx tag (cpos + l) (pos nnpx + r)
-- foldtree(px, label, l, e, tag, r) =
--  pos = px.pos
--  mlink(px, label, px.tree, EmptyTree) && e(px) && mtree(px, tag, pos + l , px.pos + r)



-- /* Memo */----------------
-- consume(px, memo) =
--  px.pos = memo.pos
--  px.tree = memo.tree
--  memo.result

{-



store(px, memo, key, pos, res) =
 memo.key = key
 memo.pos = pos
 memo.tree = tree
 memo.res = res
 res

getkey(pos, mp) =
 pos * memosize + mp

getmemo(px, ukey) =
 px.memos[ukey % memolength]

memo(px, mp, e) =
 pos = px.pos
 ukey = getkey(pos,mp)
 memo = getmemo(px, ukey)
 if(memo.key == key) consume(px,memo) else store(px, memo, key, pos, e(px))
Tree = TreeEmpty | TreeLink | TreeNode
TreeLink  tag child prev
TreeNode tag inputs pos epos child
chibi Tree {}
TreeEmpty = {}
TreeLink = {tag, child, prev}-}
