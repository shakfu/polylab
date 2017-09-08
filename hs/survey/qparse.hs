module Main where

import Text.ParserCombinators.Parsec

simple :: Parser Char
simple  = letter


word    :: Parser String
word    = many1 (letter <?> "") <?> "word"
            
separator   :: Parser ()
separator   = skipMany1 (space <|> char ',' <?> "")

sentence    :: Parser [String]
sentence    = do{ words <- sepBy1 word separator
                ; oneOf ".?!" <?> "end of sentence"
                ; return words
                }

run :: Show a => Parser a -> String -> IO ()
run p input
    = case (parse p "" input) of
        Left err -> do 
            putStr "parse error "
            print err
        Right x -> print x
        
