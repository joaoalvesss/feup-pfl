module ParserAexp where

import DataModule
import Data.Char (isDigit, isSpace, digitToInt)


parseVar :: [Token] -> Maybe (Aexp, [Token])
parseVar (VarTok var : restTokens) = Just (VarExp var, restTokens)
parseVar _ = Nothing

parseIntOrParseVarexp :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParseVarexp (IntTok n : restTokens) =
    Just (IntExp n, restTokens)
parseIntOrParseVarexp (VarTok var : restTokens) =
    Just (VarExp var, restTokens)
parseIntOrParseVarexp (OpenTok : restTokens1) =
    case parseSumOrProdOrIntOrPar restTokens1 of
        Just (aexp, (CloseTok : restTokens2)) ->
            Just (aexp, restTokens2)
        Just _ -> Nothing
        Nothing -> Nothing
parseIntOrParseVarexp _ = Nothing

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
    case parseIntOrParseVarexp tokens of
        Just (aexp1, (TimesTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (MulExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

parseSumOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrInt tokens =
    case parseProdOrInt tokens of
        Just (aexp1, (PlusTok : restTokens1)) ->
            case parseSumOrProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (AddExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

parseSubOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSubOrProdOrInt tokens =
    case parseProdOrInt tokens of
        Just (aexp1, (MinusTok : restTokens1)) ->
            case parseSubOrProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (SubExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens =
    case parseIntOrParseVarexp tokens of
        Just (aexp1, (TimesTok : restTokens1)) ->
            case parseProdOrIntOrPar restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (MulExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

parseSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens =
    case parseSubOrProdOrInt tokens of
        Just (aexp1, (PlusTok : restTokens1)) ->
            case parseSumOrProdOrIntOrPar restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (AddExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

parseAexp :: [Token] -> Aexp
parseAexp tokens =
    case parseSumOrProdOrIntOrPar tokens of
        Just (aexp, []) -> aexp
        _ -> error "Parse Aexp error"



