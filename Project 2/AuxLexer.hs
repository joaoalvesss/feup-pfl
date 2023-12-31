module AuxLexer where

import Data.Char (isDigit, isSpace, digitToInt, isAlpha, isAlphaNum)
import DataModule
import Stack

-- | 'lexer' function tokenizes a given string into a list of tokens.
-- It recognizes operators, parentheses, assignment symbols, and keywords.
-- Throws 'Run-time error' for invalid input.
lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
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
lexer (_ : restString) = error "Run-time error"

-- | 'insertEndWhileToken' function inserts EndWhileTok tokens in a given list of tokens.
-- It marks the end of while-loops based on DoTok, OpenTok, CloseTok, and SemiColonTok.
insertEndWhileToken :: [Token] -> Stack Token -> [Token]
insertEndWhileToken [] _ = []
insertEndWhileToken (DoTok : OpenTok : restOfTokens) stack = insertEndWhileToken restOfTokens (push OpenTok stack)
insertEndWhileToken (DoTok : restOfTokens) stack = insertEndWhileToken restOfTokens stack
insertEndWhileToken (SemiColonTok : CloseTok : SemiColonTok : restOfTokens) _ = SemiColonTok : EndWhileTok : restOfTokens
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

-- | 'insertThenToken' function inserts EndThenTok tokens in a given list of tokens.
-- It marks the end of 'then' blocks based on ThenTok, OpenTok, CloseTok, SemiColonTok, and ElseTok.
insertThenToken :: [Token] -> Stack Token -> [Token]
insertThenToken (ThenTok : OpenTok : restOfTokens) stack = ThenTok : insertThenToken restOfTokens (push OpenTok stack)  
insertThenToken (ThenTok : restOfTokens) stack = ThenTok : insertThenToken restOfTokens stack 
insertThenToken (SemiColonTok : CloseTok : ElseTok : restOfTokens) _ = SemiColonTok : EndThenTok : ElseTok : restOfTokens
insertThenToken (SemiColonTok : ElseTok : restOfTokens) _ = SemiColonTok : EndThenTok : ElseTok : restOfTokens 
insertThenToken (CloseTok : ElseTok : restOfTokens) _ = CloseTok : EndThenTok : ElseTok : restOfTokens 
insertThenToken [] _ = []   
insertThenToken (token : restTokens) stack
    | token == OpenTok = 
        token : insertThenToken restTokens (push token stack)
    | token == CloseTok =    
        token : insertThenToken restTokens (pop stack)  
    | otherwise = token : insertThenToken restTokens stack        

-- | 'insertElseToken' function inserts EndElseTok tokens in a given list of tokens.
-- It marks the end of 'else' blocks based on OpenTok, ElseTok, SemiColonTok, CloseTok.            
insertElseToken :: [Token] -> Stack Token -> [Token]
insertElseToken (OpenTok : ElseTok : restOfTokens) stack = ElseTok : insertElseToken restOfTokens (push OpenTok stack)
insertElseToken (ElseTok : OpenTok : restOfTokens) stack = ElseTok : insertElseToken restOfTokens (push OpenTok stack)  
insertElseToken (SemiColonTok : CloseTok : restOfTokens) stack = 
    if isEmpty stack
        then SemiColonTok : EndElseTok : restOfTokens 
        else SemiColonTok : CloseTok : (insertElseToken restOfTokens (pop stack))
insertElseToken [] _ = []   
insertElseToken (token : restTokens) stack
    | token == OpenTok = 
        token : insertElseToken restTokens (push token stack)
    | token == CloseTok =    
        token : insertElseToken restTokens (pop stack) 
    | token == SemiColonTok =    
        if isEmpty stack
            then token : EndElseTok : restTokens
            else token : insertElseToken restTokens stack 
    | otherwise = token : insertElseToken restTokens stack    

-- | 'processTokens' function processes a list of tokens, inserting corresponding block markers.
-- It handles 'if', 'then', 'else', and 'do' blocks based on the given list of tokens.
processTokens :: [Token] -> [Token]
processTokens (DoTok : OpenTok : restOfTokens) = DoTok : processTokens (insertEndWhileToken (DoTok : OpenTok : restOfTokens) empty)  
processTokens (IfTok : OpenTok : restOfTokens) = IfTok : OpenTok : processTokens restOfTokens  
processTokens (CloseTok : ThenTok : restOfTokens) = processTokens (ThenTok : restOfTokens)
processTokens (ThenTok : OpenTok : restOfTokens) = processTokens (insertThenToken (ThenTok : OpenTok :  restOfTokens) empty)
processTokens (ElseTok : OpenTok : restOfTokens) = processTokens (insertElseToken ( ElseTok : OpenTok :  restOfTokens) empty)
processTokens (token : restOfTokens) 
    | token == IfTok =
        token : OpenTok : processTokens restOfTokens 
    | token == DoTok =
        token : processTokens (insertEndWhileToken (token:restOfTokens) empty)
    | token == ThenTok =
        CloseTok : token : processTokens (insertThenToken (restOfTokens) empty)
    | token == ElseTok =
        token : processTokens (insertElseToken (restOfTokens) empty)
    | otherwise =
        token : processTokens restOfTokens
processTokens [] = []   


