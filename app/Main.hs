module Main (main) where

import Prelude
import Parser
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.List

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Integer number) = show number
showVal (Float number) = show number
showVal (Complex real img) = (show real) ++ "+" ++ (show img) ++ "i"
showVal (Rational num den) = (show num) ++ "/" ++ (show den)
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Float _) = val
eval val@(Rational _ _) = val
eval val@(Complex _ _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber)]

isNumber :: [LispVal] -> LispVal
isNumber ([Integer _]) = Bool True
isNumber _ = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol ([Atom _]) = Bool True
isSymbol _ = Bool False

isString :: [LispVal] -> LispVal
isString ([String _]) = Bool True
isString _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Integer $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Integer n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                            if null parsed
                                then 0
                                else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- bind op (>>) ;; a >> b = a >>= (\_ -> b) where
    -- (>>=) :: m a -> (a -> m b) -> m b
readExpr :: String -> LispVal
readExpr input = case (parse parseExpr "lisp" input) of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO()
main = getArgs >>= print . eval . readExpr . head