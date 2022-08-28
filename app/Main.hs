module Main (main) where

import Parser (parseExpr)
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- bind op (>>) ;; a >> b = a >>= (\_ -> b) where
    -- (>>=) :: m a -> (a -> m b) -> m b
readExpr :: String -> String
readExpr input = case (parse parseExpr "lisp" input) of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO()
main = do
    (expr:_) <- getArgs
    putStrLn $ readExpr expr