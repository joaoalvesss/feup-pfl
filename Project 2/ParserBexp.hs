-- In ParserBexp.hs

module ParserBexp where

import DataModule
import AuxLexer
import ParserAexp

parseBexp :: [Token] -> Bexp
parseBexp tokens =
    case parseAndBoolEq tokens of
        Just (bexp, []) -> bexp
        _ -> error "Parse Bexp error"

parseAndBoolEq :: [Token] -> Maybe (Bexp, [Token])
parseAndBoolEq tokens =
    case parseBoolEqAndNeg tokens of
        Just (expr1, (AndTok : restTokens1)) ->
            case parseAndBoolEq restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (AndExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

parseBoolEqAndNeg :: [Token] -> Maybe (Bexp, [Token])
parseBoolEqAndNeg tokens =
    case parseNegAndLessAndEq tokens of
        Just (expr1, (CompareTok : restTokens1)) ->
            case parseBoolEqAndNeg restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (EqBoolExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

parseNegAndLessAndEq :: [Token] -> Maybe (Bexp, [Token])
parseNegAndLessAndEq (NotTok : restTokens) =
    case parseLessOrEqOrTrueOrFalseOrParentOrArith restTokens of
        Just (expr1, restTokens1) ->
            Just (NotExp expr1, restTokens1)
        Nothing -> Nothing
parseNegAndLessAndEq restTokens = parseLessOrEqOrTrueOrFalseOrParentOrArith restTokens


parseLessOrEqOrTrueOrFalseOrParentOrArith :: [Token] -> Maybe (Bexp, [Token])
parseLessOrEqOrTrueOrFalseOrParentOrArith (OpenTok : restTokens) =
    case parseAndBoolEq restTokens of
        Just (expr, (CloseTok : restTokens1)) -> Just (expr, restTokens1)
        Just _ -> Nothing
        Nothing -> Nothing
parseLessOrEqOrTrueOrFalseOrParentOrArith (TrueTok : restTokens) = Just (TrueExp, restTokens)
parseLessOrEqOrTrueOrFalseOrParentOrArith (FalseTok : restTokens) = Just (FalseExp, restTokens)
parseLessOrEqOrTrueOrFalseOrParentOrArith tokens =
    case parseSumOrProdOrIntOrPar tokens of
        Just (expr1, (InequalityTok : restTokens1)) ->
            case parseSumOrProdOrIntOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (LeExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        Just (expr1, (EqualityTok : restTokens1)) ->
            case parseSumOrProdOrIntOrPar restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (EqExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> Nothing
