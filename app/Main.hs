module Main (main) where

import Prelude
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad.Except
import System.IO
import Data.IORef

import Parser
import ErrorChecking
import Evaluator
import IOEnv

-- bind op (>>) ;; a >> b = a >>= (\_ -> b) where
    -- (>>=) :: m a -> (a -> m b) -> m b
readExpr :: String -> ThrowsError LispVal
readExpr input = case (parse parseExpr "lisp" input) of
    Left err -> throwError $ Parser err
    Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ args !! 0
        _ -> putStrLn "Program takes only 0 or 1 argument"