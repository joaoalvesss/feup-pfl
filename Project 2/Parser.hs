module Parser where

import Data.Char (isDigit, isSpace, digitToInt)
import DataModule
import AuxLexer
import ParserAexp 
import ParserBexp

parseStm :: [Token] -> (Stm, [Token])
parseStm (VarTok var : AssignmentTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (aexp, SemiColonTok : remainingTokens) ->
            (Assign var aexp, remainingTokens)
        _ -> error "Run-time error"

parseStm (IfTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, thenRestTokens) ->
            case thenRestTokens of
                ThenTok : remainingTokens1 ->
                    case parseStmsUntilEndThen remainingTokens1 of
                        (thenStms, EndThenTok : elseRestTokens) ->
                            case elseRestTokens of
                                ElseTok : remainingTokens2 ->
                                    case parseStmsUntilEndElse remainingTokens2 of
                                        (elseStms, EndElseTok : finalTokens) ->
                                            (IfThenElse condition thenStms elseStms, finalTokens)
                                        _ -> error "Run-time error"
                                _ -> error "Run-time error"
                        _ -> error "Run-time error"
                _ -> error "Run-time error"
        _ -> error "Run-time error"
parseStm (WhileTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, DoTok : restOfTokens) ->
            case restOfTokens of
                OpenTok : _ -> parseDo condition restOfTokens
                _ -> parseDo condition restOfTokens             
        _ -> error "Run-time error"
parseStm [] = (IgnoreStm, [])
parseStm tokens = error "Run-time error" 


--------------------------------------------------------


debugParseBexpBetweenBrackets :: [Token] -> Maybe (Bexp, [Token])
debugParseBexpBetweenBrackets tokens =
    case parseBexpBetweenBrackets tokens of
        Just (bexp, remainingTokens) -> Just (bexp, remainingTokens)
        Nothing -> error "Run-time error"


parseThen :: [Token] -> Maybe (Bexp, [Stm], [Token])
parseThen (tok : restTokens) =
    case parseBexpBetweenBrackets (tok : restTokens) of
        Just (condition, SemiColonTok : remainingTokens) ->
            let (thenStms, endTokens) = parseStmsUntilEndThen remainingTokens
            in case endTokens of
                (EndThenTok : rest) -> Just (condition, thenStms, rest)
                _ -> Nothing
        _ -> Nothing


parseElse :: [Token] -> Maybe ([Stm], [Token])
parseElse (ElseTok : restTokens) =
    let (elseStms, remainingTokens) = parseStmsUntilEndElse restTokens
    in case remainingTokens of
        (EndElseTok : rest) -> Just (elseStms, rest)
        _ -> Nothing
parseElse _ = Nothing

parseStmsUntilEndThen :: [Token] -> ([Stm], [Token])
parseStmsUntilEndThen tokens =
    case parseStms (\toks -> isEndThenToken toks || isEndElseToken toks) tokens of
        (stms, remainingTokens) -> (stms, remainingTokens)

parseStmsUntilEndElse :: [Token] -> ([Stm], [Token])
parseStmsUntilEndElse tokens =
    case parseStms (\toks -> isEndElseToken toks) tokens of
        (stms, remainingTokens) -> (stms, remainingTokens)

isEndThenToken :: [Token] -> Bool
isEndThenToken (EndThenTok : _) = True
isEndThenToken _ = False

isEndElseToken :: [Token] -> Bool
isEndElseToken (EndElseTok : _) = True
isEndElseToken _ = False

parseStmsUntilEndWhile :: [Token] -> ([Stm], [Token])
parseStmsUntilEndWhile tokens =
    parseStms (\toks -> isEndDoToken toks || null toks) tokens
    
parseDo :: Bexp -> [Token] -> (Stm, [Token])
parseDo condition whileBody =
    let (stms, remainingTokens) = parseStmsUntilEndWhile whileBody
    in case remainingTokens of
        (EndWhileTok : endTokens) -> (While condition stms, endTokens)
        _ -> error "Run-time error"


isEndDoToken :: [Token] -> Bool
isEndDoToken (EndWhileTok : _) = True
isEndDoToken (CloseTok : remaining) = isSemiColonEndWhile remaining
isEndDoToken _ = False

isSemiColonEndWhile :: [Token] -> Bool
isSemiColonEndWhile (CloseTok : EndWhileTok : _) = True
isSemiColonEndWhile (EndWhileTok : _) = True
isSemiColonEndWhile _ = False


parseStms :: ([Token] -> Bool) -> [Token] -> ([Stm], [Token])
parseStms _ [] = ([], [])
parseStms stopCondition tokens
    | stopCondition tokens = ([], tokens)
    | otherwise =
        case parseStm tokens of
            (stm, remainingTokens) | isValidStm stm ->
                let (nextStms, restTokens) = parseStms stopCondition remainingTokens
                in (stm : nextStms, restTokens)
            (_, remainingTokens) ->
                error "Run-time error"


parseBexpBetweenBrackets :: [Token] -> Maybe (Bexp, [Token])
parseBexpBetweenBrackets (OpenTok : TrueTok : CloseTok : remainingTokens) =
    Just (TrueExp, remainingTokens)
parseBexpBetweenBrackets (OpenTok : FalseTok : CloseTok : remainingTokens) =
    Just (FalseExp, remainingTokens)
parseBexpBetweenBrackets (OpenTok : restTokens) =
    case parseAndBoolEq restTokens of
        Just (expr, CloseTok : remainingTokens) -> Just (expr, remainingTokens)
        _ -> Nothing
parseBexpBetweenBrackets _ = Nothing

parseAexpBetweenBrackets :: [Token] -> Maybe (Aexp, [Token])
parseAexpBetweenBrackets (OpenTok : IntTok n : CloseTok : remainingTokens) =
    Just (IntExp n, remainingTokens)
parseAexpBetweenBrackets (OpenTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (expr, CloseTok : remainingTokens) -> Just (expr, remainingTokens)
        _ -> Nothing
parseAexpBetweenBrackets _ = Nothing

isValidStm :: Stm -> Bool
isValidStm stm = True

test :: String ->  [Token]
test input =  (processTokens(lexer input))
