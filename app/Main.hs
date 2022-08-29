module Main (main) where

import Prelude
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Except

import Parser
import ErrorChecking
import Evaluator

-- bind op (>>) ;; a >> b = a >>= (\_ -> b) where
    -- (>>=) :: m a -> (a -> m b) -> m b
readExpr :: String -> ThrowsError LispVal
readExpr input = case (parse parseExpr "lisp" input) of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled