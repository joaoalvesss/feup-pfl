module Parser where

import Data.Char (isDigit, isSpace, digitToInt)
import DataModule
import AuxLexer
import ParserAexp 
import ParserBexp

-- | 'parseStm' function parses a list of tokens and returns a statement along with the remaining tokens.
-- It handles variable assignments, if-then-else constructs, and while loops.
-- If the parsing encounters an error, it raises a run-time error.
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


-- | 'debugParseBexpBetweenBrackets' is a debug version of 'parseBexpBetweenBrackets'.
-- It attempts to parse a boolean expression enclosed within brackets and returns the result along with the remaining tokens.
-- If parsing fails, it raises a run-time error.
debugParseBexpBetweenBrackets :: [Token] -> Maybe (Bexp, [Token])
debugParseBexpBetweenBrackets tokens =
    case parseBexpBetweenBrackets tokens of
        Just (bexp, remainingTokens) -> Just (bexp, remainingTokens)
        Nothing -> error "Run-time error"


-- | 'parseThen' attempts to parse the "then" part of an if-then-else construct.
-- It returns a tuple containing the parsed boolean expression, list of statements for the "then" part, and the remaining tokens.
-- If parsing fails, it returns 'Nothing'.
parseThen :: [Token] -> Maybe (Bexp, [Stm], [Token])
parseThen (tok : restTokens) =
    case parseBexpBetweenBrackets (tok : restTokens) of
        Just (condition, SemiColonTok : remainingTokens) ->
            let (thenStms, endTokens) = parseStmsUntilEndThen remainingTokens
            in case endTokens of
                (EndThenTok : rest) -> Just (condition, thenStms, rest)
                _ -> Nothing
        _ -> Nothing

-- | 'parseElse' attempts to parse the "else" part of an if-then-else construct.
-- It returns a tuple containing the list of statements for the "else" part and the remaining tokens.
-- If parsing fails, it returns 'Nothing'.
parseElse :: [Token] -> Maybe ([Stm], [Token])
parseElse (ElseTok : restTokens) =
    let (elseStms, remainingTokens) = parseStmsUntilEndElse restTokens
    in case remainingTokens of
        (EndElseTok : rest) -> Just (elseStms, rest)
        _ -> Nothing
parseElse _ = Nothing


-- | 'parseStmsUntilEndThen' parses a sequence of statements until it encounters an "EndThen" token.
-- It returns a tuple containing the parsed statements and the remaining tokens.
parseStmsUntilEndThen :: [Token] -> ([Stm], [Token])
parseStmsUntilEndThen tokens =
    case parseStms (\toks -> isEndThenToken toks || isEndElseToken toks) tokens of
        (stms, remainingTokens) -> (stms, remainingTokens)


-- | 'parseStmsUntilEndElse' parses a sequence of statements until it encounters an "EndElse" token.
-- It returns a tuple containing the parsed statements and the remaining tokens.
parseStmsUntilEndElse :: [Token] -> ([Stm], [Token])
parseStmsUntilEndElse tokens =
    case parseStms (\toks -> isEndElseToken toks) tokens of
        (stms, remainingTokens) -> (stms, remainingTokens)


-- | 'isEndThenToken' checks if the given list of tokens starts with an "EndThen" token.
isEndThenToken :: [Token] -> Bool
isEndThenToken (EndThenTok : _) = True
isEndThenToken _ = False


-- | 'isEndElseToken' checks if the given list of tokens starts with an "EndElse" token.
isEndElseToken :: [Token] -> Bool
isEndElseToken (EndElseTok : _) = True
isEndElseToken _ = False


-- | 'parseStmsUntilEndWhile' parses a sequence of statements until it encounters an "EndDo" or an empty token list.
-- It returns a tuple containing the parsed statements and the remaining tokens.
parseStmsUntilEndWhile :: [Token] -> ([Stm], [Token])
parseStmsUntilEndWhile tokens =
    parseStms (\toks -> isEndDoToken toks || null toks) tokens


-- | 'parseDo' is a helper function that parses the body of a 'While' loop.
-- It takes a boolean expression representing the loop condition and the tokens of the loop body.
-- It returns a tuple containing the parsed 'While' statement and the remaining tokens.
parseDo :: Bexp -> [Token] -> (Stm, [Token])
parseDo condition whileBody =
    let (stms, remainingTokens) = parseStmsUntilEndWhile whileBody
    in case remainingTokens of
        (EndWhileTok : endTokens) -> (While condition stms, endTokens)
        _ -> error "Run-time error"


-- | 'isEndDoToken' checks if the given list of tokens starts with an "EndDo" token.
isEndDoToken :: [Token] -> Bool
isEndDoToken (EndWhileTok : _) = True
isEndDoToken (CloseTok : remaining) = isSemiColonEndWhile remaining
isEndDoToken _ = False


-- | 'isSemiColonEndWhile' checks if the given list of tokens starts with a sequence of CloseToken followed by EndWhileToken.
isSemiColonEndWhile :: [Token] -> Bool
isSemiColonEndWhile (CloseTok : EndWhileTok : _) = True
isSemiColonEndWhile (EndWhileTok : _) = True
isSemiColonEndWhile _ = False


-- | 'parseStms' takes a stop condition and a list of tokens and parses a sequence of statements until the stop condition is met.
-- It returns a tuple containing the parsed statements and the remaining tokens.
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


-- | 'parseBexpBetweenBrackets' parses a boolean expression between brackets.
-- It returns a tuple containing the parsed boolean expression and the remaining tokens.
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


-- | 'parseAexpBetweenBrackets' parses an arithmetic expression between brackets.
-- It returns a tuple containing the parsed arithmetic expression and the remaining tokens.
parseAexpBetweenBrackets :: [Token] -> Maybe (Aexp, [Token])
parseAexpBetweenBrackets (OpenTok : IntTok n : CloseTok : remainingTokens) =
    Just (IntExp n, remainingTokens)
parseAexpBetweenBrackets (OpenTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (expr, CloseTok : remainingTokens) -> Just (expr, remainingTokens)
        _ -> Nothing
parseAexpBetweenBrackets _ = Nothing


-- | 'isValidStm' checks if a statement is valid.
-- In this example, it always returns 'True'.
isValidStm :: Stm -> Bool
isValidStm stm = True


-- | 'test' is a helper function that takes an input string, processes it, and returns the resulting list of tokens.
test :: String ->  [Token]
test input =  (processTokens(lexer input))
