module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readOct, readHex, readInt, readFloat)
import Data.Char (digitToInt)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | Complex Float Float
             | Rational Integer Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char

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

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

getEscaped :: Char -> Parser Char
getEscaped c = case c of
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    '\\' -> return '\\'
    _ -> return c

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many ((char '\\' >> oneOf "\"\\nrt" >>= getEscaped) <|> noneOf "\"")
    _ <- char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first:rest
    return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom atom

{-
parseDecimalNumber :: Parser LispVal
parseDecimalNumber = liftM (Number . read) $ many1 digit
parseDecimalNumber = many1 digit >>= (\a -> return $ (Number . read) a)
parseDecimalNumber = do
    a <- many1 digit
    let n = read a :: Integer
    return $ Number n
-}

parseInteger :: Parser LispVal
parseInteger = try parseOctNumber 
                    <|> try parseHexNumber
                    <|> try parseBinNumber
                    <|> try parseDecNumber
                    where parseOctNumber = do
                                _ <- string "#o"
                                n <- many1 (oneOf "012345678")
                                return $ Integer $ fst $ head (readOct n)
                          parseHexNumber = do
                                _ <- string "#x"
                                n <- many1 (digit <|> oneOf "abcdefABCDEF")
                                return $ Integer $ fst $ head (readHex n)
                          parseDecNumber = do
                                _ <- optional (string "#d")
                                n <- many1 digit
                                return $ Integer $ read n
                          parseBinNumber = do
                                _ <- string "#b"
                                n <- many (oneOf "01")
                                let bn = fst $ head $ (readInt 2 (`elem` "01") digitToInt) n
                                return $ Integer $ bn

parseComplex :: Parser LispVal
parseComplex = do
    real <- (try parseFloat <|> parseInteger)
    _ <- optional (char ' ') >> char '+' >> optional (char ' ')
    img <- (try parseFloat <|> parseInteger)
    _ <- char 'i'
    return $ Complex (convert real) (convert img)
        where convert (Float x) = x :: Float
              convert (Integer x) = fromInteger x :: Float

parseRational :: Parser LispVal
parseRational = do
    num <- many1 digit
    _ <- optional (char ' ') >> char '/' >> optional (char ' ')
    den <- many1 digit
    return $ Rational (read num) (read den)

parseNumber :: Parser LispVal
parseNumber = try parseComplex
                <|> try parseRational
                <|> try parseInteger

parseChar :: Parser LispVal
parseChar = string "#\\" >> ((string "space" >> (return $ Character ' '))
                            <|> (string "newline" >> (return $ Character '\n'))
                            <|> (anyChar >>= (\c -> return $ Character c)))

parseFloat :: Parser LispVal
parseFloat = do
    whole <- many1 digit
    _ <- char '.'
    dec <- many1 digit
    return $ Float $ fst . head $ readFloat (whole ++ "." ++ dec)


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseChar
            <|> try parseString
            <|> try parseNumber
            <|> try parseFloat
            <|> parseAtom
            <|> parseQuoted
            <|> (do char '('
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x)