--module Parser where

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
        _ -> error "Failed to parse assignment statement"
parseStm (IfTok : restTokens) =
    -- IGNORE
    undefined
parseStm (WhileTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, DoTok : whileBody) ->
            let (stms, remainingTokens) = parseStmsUntilEndWhile whileBody
            in case remainingTokens of
                (EndWhileTok : endTokens) -> (While condition stms, endTokens)
                _ -> error "Expected EndWhileTok after While statement"
        _ -> error "Failed to parse while statement"
parseStm [] = error "Unexpected end of input"
parseStm _ = error "Invalid input for statement" 

parseStmsUntilEndWhile :: [Token] -> ([Stm], [Token])
parseStmsUntilEndWhile tokens =
    parseStms (\toks -> isEndToken toks || null toks) tokens

isEndToken :: [Token] -> Bool
isEndToken (EndWhileTok : _) = True
isEndToken _ = False

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
                error $ "1 Unexpected tokens after parsing: " ++ show remainingTokens

parse :: String -> [Stm]
parse input =
    let (stms, remainingTokens) = parseStmsUntilEndWhile (processTokens (lexer input))
    in
        if null remainingTokens
            then stms
            else error $ "2 Unexpected tokens after parsing: " ++ show remainingTokens


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

isValidStm :: Stm -> Bool
isValidStm stm = True

test :: String ->  [Token]
test input = processTokens(lexer input)

