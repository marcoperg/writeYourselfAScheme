module ErrorChecking where

import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)

import Parser

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values" ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "parse error at " ++ show parseErr

instance Show LispError where show = showError


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val