module AuxLexer where

import DataModule
import Data.Char (isDigit, isSpace, digitToInt, isAlpha, isAlphaNum)



lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer (':':'=' : restStr) = AssignmentTok : lexer restStr
lexer (';' : restStr) = SemiColonTok : lexer restStr
lexer ('=' : restStr) = error ("------------- Try ':=' instead of '=' -------------")
lexer str@(chr : restStr)
    | isSpace chr = lexer restStr
lexer str@(chr : _)
    | isDigit chr
    = IntTok (fromIntegral (stringToInt digitStr)) : lexer restStr
    where
        (digitStr, restStr) = break (not . isDigit) str
        stringToInt :: String -> Int
        stringToInt=foldl (\acc chr->10*acc+digitToInt chr) 0
lexer str@(chr : restStr)
    | isAlpha chr || chr == '_' =
        let (varName, restStr') = span (\c -> isAlphaNum c || c == '_') str
        in
            case varName of
                "if" -> IfTok : lexer restStr'
                "then" -> ThenTok : lexer restStr'
                "else" -> ElseTok : lexer restStr'
                "while" -> WhileTok : lexer restStr'
                "do" -> DoTok : lexer restStr'
                "True" -> TrueTok : lexer restStr'
                "False" -> FalseTok : lexer restStr'
                _ -> VarTok varName : lexer restStr'
    where
        isAlphaNumOrUnderscore c = isAlphaNum c || c == '_'
lexer (_ : restString) = error ("unexpected character: ")
