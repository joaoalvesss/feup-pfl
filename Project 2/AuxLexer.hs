-- module AuxLexer where

import Data.Char (isDigit, isSpace, digitToInt, isAlpha, isAlphaNum)
import DataModule
import Stack


lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('=':'=' : restStr) = EqualityTok : lexer restStr
lexer (':':'=' : restStr) = AssignmentTok : lexer restStr
lexer (';' : restStr) = SemiColonTok : lexer restStr
lexer ('=' : restStr) = CompareTok : lexer restStr
lexer ('<':'=' : restStr) = InequalityTok : lexer restStr
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
                "and" -> AndTok : lexer restStr'
                "not" -> NotTok : lexer restStr'
                _ -> VarTok varName : lexer restStr'
    where
        isAlphaNumOrUnderscore c = isAlphaNum c || c == '_'
lexer (_ : restString) = error ("unexpected character: ")



parent :: String -> Bool
parent str = parentAux str empty

parentAux :: String -> Stack Char -> Bool
parentAux [] stk = isEmpty stk
parentAux (x:xs) stk
    | x == '(' = parentAux xs (push '(' stk)
    | x == ')' = not (isEmpty stk) && top stk == '(' && parentAux xs (pop stk)
    | x == '[' = parentAux xs (push '[' stk)
    | x == ']' = not (isEmpty stk) && top stk == '[' && parentAux xs (pop stk) 
    | otherwise = parentAux xs stk 



insertEndWhileToken :: [Token] -> Stack Token -> [Token]
insertEndWhileToken [] _ = []
insertEndWhileToken (token : restTokens) stack
    | token == OpenTok = token : insertEndWhileToken restTokens (push token stack)
    | token == CloseTok =
        if isEmpty (pop stack)
            then token: EndWhileTok : restTokens
            else token : insertEndWhileToken restTokens (pop stack)
    | otherwise = token : insertEndWhileToken restTokens stack


test:: String -> [Token]
test input = insertEndWhileToken (lexer input) empty


{-

insertEndWhileToken :: [Token] -> Stack Token -> ([Token], [Token])
insertEndWhileToken [] _ = ([], [])
insertEndWhileToken (token : restTokens) stack
    | token == OpenTok = 
        let (processed, remaining) = insertEndWhileToken restTokens (push token stack)
        in (token : processed, remaining)
    | token == CloseTok =
        if isEmpty stack
            then ([token, EndWhileTok], restTokens)
        else let (processed, remaining) = insertEndWhileToken restTokens (pop stack)
            in (token : processed, remaining)
    | otherwise = 
        let (processed, remaining) = insertEndWhileToken restTokens stack
        in (token : processed, remaining)

test :: String -> ([Token], [Token])
test input = insertEndWhileToken (lexer input) empty

-}

