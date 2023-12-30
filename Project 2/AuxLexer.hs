module AuxLexer where

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

insertEndWhileToken :: [Token] -> Stack Token -> [Token]
insertEndWhileToken [] _ = []
insertEndWhileToken (token : restTokens) stack
    | token == OpenTok = 
        token : insertEndWhileToken restTokens (push token stack)
    | token == SemiColonTok =    
        if isEmpty stack
            then token : EndWhileTok : restTokens
            else token : insertEndWhileToken restTokens stack
    | token == CloseTok =    
        token : insertEndWhileToken restTokens (pop stack)
    | otherwise = token : insertEndWhileToken restTokens stack

insertThenToken :: [Token] -> Stack Token -> [Token]
insertThenToken [] _ = []   
insertThenToken (token : restTokens) stack
    | token == OpenTok = 
        token : insertThenToken restTokens (push token stack)
    | token == CloseTok =    
        token : insertThenToken restTokens (pop stack) 
    | token == SemiColonTok =    
        if isEmpty stack
            then EndThenTok : restTokens
            else token : insertThenToken restTokens stack 
    | otherwise = token : insertThenToken restTokens stack        
            
insertElseToken :: [Token] -> Stack Token -> [Token]
insertElseToken [] _ = []   
insertElseToken (token : restTokens) stack
    | token == OpenTok = 
        token : insertElseToken restTokens (push token stack)
    | token == CloseTok =    
        token : insertElseToken restTokens (pop stack) 
    | token == SemiColonTok =    
        if isEmpty stack
            then EndElseTok : restTokens
            else token : insertElseToken restTokens stack 
    | otherwise = token : insertElseToken restTokens stack    

processTokens :: [Token] -> [Token]
processTokens (token : restOfTokens) 
    | token == DoTok =
        token : processTokens (insertEndWhileToken restOfTokens empty)
    | token == ThenTok =
        token : processTokens (insertThenToken restOfTokens empty)
    | token == ElseTok =
        token : processTokens (insertElseToken restOfTokens empty)
    | otherwise =
        token : processTokens restOfTokens
processTokens [] = []   

test :: String -> [Token]
test input = processTokens (lexer input)

