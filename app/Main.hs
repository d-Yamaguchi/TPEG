module Main where

import NezReader
import Control.Monad
import Text.Parsec

main :: IO ()
main = do
  l <- getContents
  runParseTest l

runParseTest :: String -> IO()
runParseTest xs  = case (parse start "" xs) of
    Left err -> print err
    Right str -> print "success"
