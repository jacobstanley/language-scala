module Main where

import qualified Data.ByteString as B
import           System.Environment (getArgs)

import           Language.Scala.Positions
import           Language.Scala.Scanner
import           Language.Scala.Tokens
import           Language.Scala.Utilities

------------------------------------------------------------------------

main :: IO ()
main = do
    files <- getArgs
    mapM_ runScanner files

runScanner :: FilePath -> IO ()
runScanner path = do
    bs <- B.readFile path
    let toks = scanTokens (bs :@ (startPositionInFile path))
    print (toList toks)
  where
    toList :: Tokens -> [Token]
    toList (x ::> xs)    = value x : toList xs
    toList (_ ::! xs)    = toList xs
    toList (EndTokens _) = []
