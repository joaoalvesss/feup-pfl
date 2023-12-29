import Data.Char (isDigit, isSpace, digitToInt)
import DataModule
import AuxLexer
import ParserAexp 

parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm (VarTok var : AssignmentTok : restTokens) =
    case parseSumOrProdOrIntOrPar restTokens of
        Just (aexp, SemiColonTok : remainingTokens) ->
            Just (Assign var aexp, remainingTokens)
        _ -> Nothing
parseStm _ = Nothing




parse :: String -> Maybe Stm
parse input =
    case parseStm (lexer input) of
        Just (stm, []) -> Just stm
        _ -> Nothing


