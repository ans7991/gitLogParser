{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module GitLogParser where

import qualified Text.Parsec as P

type Name = String
type Mail = String

data Author = Author Name Mail deriving Show

data Log = Log {
                commit  :: String,
                merge   :: String,
                author  :: Author,
                date    :: String,
                message :: String
} deriving Show

fileParser :: P.Parsec String () [Log]
fileParser = P.manyTill logParser P.eof

logParser :: P.Parsec String () Log
logParser = Log <$> commitParser
                <*> (P.try mergeParser P.<|> P.string "")
                <*> authorParser
                <*> dateParser
                <*> messageParser

commitParser :: P.Parsec String () String
commitParser = P.string "commit" >> P.spaces >> P.manyTill P.anyChar P.newline

mergeParser :: P.Parsec String () String
mergeParser = P.string "Merge:" >> P.spaces >> P.manyTill P.anyChar P.newline

authorParser :: P.Parsec String () Author
authorParser = do fname <- P.string "Author:" >> P.spaces >> P.manyTill P.anyChar P.space
                  lname <- P.manyTill P.anyChar (P.char '<')
                  mail <- P.manyTill P.anyChar (P.char '>')
                  P.newline
                  return $ Author (concat [fname, " ", lname]) mail

dateParser :: P.Parsec String () String
dateParser = P.string "Date:" >> P.spaces >> P.manyTill P.anyChar P.newline

messageParser :: P.Parsec String () String
messageParser = P.newline >> P.manyTill P.anyChar (P.try (do P.string "\n\n"
                                                             P.notFollowedBy P.tab))
