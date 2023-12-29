module ParserAexp where


import DataModule
import Data.Char (isDigit, isSpace, digitToInt)



parseInt :: [Token] -> Maybe (Aexp, [Token])
parseInt (IntTok n : restTokens) = Just (IntExp n, restTokens)
parseInt tokens = Nothing

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens
    = case parseInt tokens of
        Just (aexp1, (TimesTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (MulExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result -- can be ’Nothing’ or valid


parseSumOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrInt tokens
    = case parseProdOrInt tokens of
        Just (aexp1, (PlusTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (AddExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result -- could be ’Nothing’ or valid

parseIntOrParenaexp :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenaexp (IntTok n : restTokens)
    = Just (IntExp  n, restTokens)
parseIntOrParenaexp (OpenTok : restTokens1)
    = case parseSumOrProdOrIntOrPar restTokens1 of
        Just (aexp, (CloseTok : restTokens2)) ->
            Just (aexp, restTokens2)
        Just _ -> Nothing -- no closing paren
        Nothing -> Nothing
parseIntOrParenaexp tokens = Nothing


parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens
    = case parseIntOrParenaexp tokens of
        Just (aexp1, (TimesTok : restTokens1)) ->
            case parseProdOrIntOrPar restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (MulExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

parseSumOrProdOrIntOrPar::[Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens
    = case parseProdOrIntOrPar tokens of
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
