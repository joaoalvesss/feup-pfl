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
parseStm _ = error "Invalid input for assignment statement"

parse :: String -> Stm
parse input =
    case parseStm (lexer input) of
        stm | isValidStm stm -> stm
        _ -> error "Unexpected tokens after parsing"

isValidStm :: Stm -> Bool
isValidStm stm = True
