module Main where

import           GitLogParser
import qualified Text.Parsec  as P

main :: IO ()
main = do putStrLn "Enter log file location: "
          filename <- getLine
          logs <- readFile filename
          case P.parse fileParser "" logs of
            Left l -> print l
            Right r -> print $ show (length r) ++ " commit logs found."
