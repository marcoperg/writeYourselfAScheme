module Main (main) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space



-- bind op (>>) Monad a >> Monad b == Monad( a . b)
readExpr :: String -> String
readExpr input = case (parse (spaces >> symbol) "lisp" input) of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"


main :: IO()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr