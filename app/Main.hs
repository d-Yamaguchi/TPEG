module Main where

import NezType
import Text.ParserCombinators.Parsec

main :: IO ()
main = test1 >> test2 >> test3 >> test4

test1 :: IO()
test1 = print $ typingFunc (VToken "someTag" (TString "hoge"))

test2 :: IO()
test2 = print $ typingFunc (VTuple "someTag" [VToken "hoge" Any, VToken "fuga" Epsilon])

test3 :: IO()
test3 = print $ typingFunc (VOption "someTag" (VToken "hoge" Any))

test4 :: IO()
test4 = print $ typingFunc (VTree "someTag" [("label1", VToken "hoge" Any), ("label2", VTuple "fuga" [VToken "hoge" Any])])
