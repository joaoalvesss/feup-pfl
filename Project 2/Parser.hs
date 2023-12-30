--module Parser where

import Data.Char (isDigit, isSpace, digitToInt)
import DataModule
import AuxLexer
import ParserAexp 
import ParserBexp


--parseStm cria o statement e devolve tbm os restantes tokens


parseStm :: [Token] -> (Stm, [Token])
parseStm (VarTok var : AssignmentTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (aexp, SemiColonTok : remainingTokens) ->
            (Assign var aexp, remainingTokens)
        _ -> error "Failed to parse assignment statement"
parseStm _ = error "Invalid input for assignment statement"

parseStms :: [Token] -> [Stm]
parseStms [] = []
parseStms tokens =
    case parseStm tokens of
        (stm, remainingTokens) | isValidStm stm ->
            stm : parseStms remainingTokens
        (_, remainingTokens) ->
            error $ "Unexpected tokens after parsing: " ++ show remainingTokens


parse :: String -> [Stm]
parse input = parseStms (processTokens(lexer input))


isValidStm :: Stm -> Bool
isValidStm stm = True
