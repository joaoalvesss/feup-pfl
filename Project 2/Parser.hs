module Parser where

import Data.Char (isDigit)
import DataModule
import AuxLexer
import ParserAexp 
import ParserBexp

-- In Parser.hs

parseStm :: [Token] -> (Stm, [Token])
parseStm (VarTok var : AssignmentTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (aexp, SemiColonTok : remainingTokens) ->
            (Assign var aexp, remainingTokens)
        _ -> error "Failed to parse assignment statement"
parseStm (IfTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, ThenTok : thenBody) ->
            let (thenStms, elseRestTokens) = parseStmsUntilElseOrEndThen thenBody
            in case elseRestTokens of
                (ElseTok : elseBody) ->
                    let (elseStms, remainingTokens) = parseStmsUntilEndThen elseBody
                    in (IfThenElse condition thenStms elseStms, remainingTokens)
                _ -> error "Missing else branch in if statement"
        _ -> error "Failed to parse if statement"
parseStm (WhileTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, DoTok : whileBody) ->
            let (stms, remainingTokens) = parseStms untilEndWhileTok whileBody
            in (While condition stms, remainingTokens)
        _ -> error "Failed to parse while statement"
parseStm [] = error "Unexpected end of input"
parseStm _ = error "Invalid input for statement"

-- Helper functions
parseStmsUntilElseOrEndThen :: [Token] -> ([Stm], [Token])
parseStmsUntilElseOrEndThen tokens = parseStms isEndToken tokens
  where
    isEndToken (EndElseTok : _) = True
    isEndToken (EndThenTok : _) = True
    isEndToken _ = False

parseStmsUntilEndThen :: [Token] -> ([Stm], [Token])
parseStmsUntilEndThen tokens = parseStms isEndToken tokens
  where
    isEndToken (EndThenTok : _) = True
    isEndToken _ = False

parseStms :: ([Token] -> Bool) -> [Token] -> ([Stm], [Token])
parseStms _ [] = ([], [])
parseStms stopCondition tokens
    | stopCondition tokens = ([], tokens)
    | otherwise =
        case parseStm tokens of
            (stm, remainingTokens) ->
                if isValidStm stm
                    then
                        let (nextStms, restTokens) = parseStms stopCondition remainingTokens
                        in (stm : nextStms, restTokens)
                    else
                        error $ "2 Unexpected tokens after parsing: " ++ show tokens

remainingTokens :: Stm -> [Token]
remainingTokens (Assign _ _) = []
remainingTokens (While _ stms) = concatMap remainingTokens stms

untilEndWhileTok :: [Token] -> Bool
untilEndWhileTok (EndWhileTok : _) = True
untilEndWhileTok _ = False

parseBexpBetweenBrackets :: [Token] -> Maybe (Bexp, [Token])
parseBexpBetweenBrackets (OpenTok : restTokens) =
    case parseAndBoolEq restTokens of
        Just (expr, CloseTok : remainingTokens) -> Just (expr, remainingTokens)
        _ -> Nothing
parseBexpBetweenBrackets _ = Nothing

isValidStm :: Stm -> Bool
isValidStm _ = True

parse :: String -> Stm
parse input =
    let (stm, remainingTokens) = parseStm (lexer input)
    in if null remainingTokens
        then stm
        else error $ "1 Unexpected tokens after parsing: " ++ show remainingTokens


