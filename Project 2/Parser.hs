module Parser where

import Data.Char (isDigit, isSpace, digitToInt)
import DataModule
import AuxLexer
import ParserAexp 
import ParserBexp

-- In Parser.hs

parseStm :: [Token] -> Stm
parseStm (VarTok var : AssignmentTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (aexp, SemiColonTok : remainingTokens) ->
            Assign var aexp
        _ -> error "Failed to parse assignment statement"
parseStm (WhileTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, DoTok : whileBody) ->
            let stms = parseStms untilEndWhileTok whileBody
            in While condition stms
        _ -> error "Failed to parse while statement"       
parseStm _ = error "Invalid input for statement"

parseStms :: ([Token] -> Bool) -> [Token] -> [Stm]
parseStms _ [] = []
parseStms stopCondition tokens
    | stopCondition tokens = []
    | otherwise =
        case parseStm tokens of
            stm | isValidStm stm ->
                let nextStms = parseStms stopCondition (remainingTokens stm)
                in stm : nextStms
            _ ->
                error $ "Unexpected tokens after parsing: " ++ show tokens

                 


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
isValidStm stm = True

parse :: String -> [Stm]
parse input = parseStms untilEndWhileTok (lexer input)