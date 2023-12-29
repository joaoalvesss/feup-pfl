module Parser where

import Data.Char (isDigit, isSpace, digitToInt)
import DataModule
import AuxLexer
import ParserAexp 
import ParserBexp

parseStm :: [Token] -> Stm
parseStm (VarTok var : AssignmentTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (aexp, SemiColonTok : remainingTokens) ->
            Assign var aexp
        _ -> error "Failed to parse assignment statement"
parseStm (IfTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, thenBranch) ->
            case thenBranch of
                (ThenTok : ifBody) ->
                    let (elseBody, remainingTokens) = splitElseBranch ifBody
                    in IfThenElse condition (parseBlock ifBody) (parseBlock elseBody)
                (ElseTok : elseBody) ->
                    IfThenElse condition (parseBlock []) (parseBlock elseBody)
                _ -> error "Failed to parse if statement, missing 'then' keyword"
        _ -> error "Failed to parse if statement"
parseStm (WhileTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, whileBody) ->
            While condition (parseBlock whileBody)
        _ -> error "Failed to parse while statement"
parseStm (ElseTok : restTokens) =
    case parseBexpBetweenBrackets restTokens of
        Just (condition, elseBody) ->
            IfThenElse condition (parseBlock []) (parseBlock elseBody)
        _ -> error "Unexpected 'else' keyword"
parseStm _ = error "Invalid input for statement"

parseBexpBetweenBrackets :: [Token] -> Maybe (Bexp, [Token])
parseBexpBetweenBrackets (OpenTok : restTokens) =
    case parseAndBoolEq restTokens of
        Just (expr, CloseTok : restTokens1) -> Just (expr, restTokens1)
        _ -> Nothing
parseBexpBetweenBrackets _ = Nothing

splitElseBranch :: [Token] -> ([Token], [Token])
splitElseBranch tokens =
    case break (\t -> t == ElseTok) tokens of
        (ifBody, ElseTok : elseBody) -> (ifBody, elseBody)
        (ifBody, _) -> (ifBody, [])
        
parseBlock :: [Token] -> [Stm]
parseBlock [] = []
parseBlock tokens =
    case parseStm tokens of
        stm | isValidStm stm ->
            stm : parseBlock (dropWhile (== SemiColonTok) (dropWhile (/= SemiColonTok) tokens))
        _ -> error "Unexpected tokens in block"

parse :: String -> Stm
parse input =
    case parseStm (lexer input) of
        stm | isValidStm stm -> stm
        _ -> error "Unexpected tokens after parsing"

isValidStm :: Stm -> Bool
isValidStm stm = True
