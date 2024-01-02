-- | Module: ParserBexp
-- Module for parsing boolean expressions using tokens from the lexer and building the corresponding 'Bexp' AST.
module ParserBexp where

import DataModule
import AuxLexer
import ParserAexp

-- | Function: parseBexp
-- Parses a list of tokens into a boolean expression ('Bexp').
parseBexp :: [Token]  -- ^ The list of tokens to be parsed.
          -> Bexp     -- ^ The resulting boolean expression.
parseBexp tokens =
    case parseAndBoolEq tokens of
        Just (bexp, []) -> bexp
        _ -> error "Run-time error"

-- | Function: parseAndBoolEq
-- Parses boolean expressions with 'And' operations and equality comparisons.
parseAndBoolEq :: [Token] -> Maybe (Bexp, [Token])
parseAndBoolEq tokens =
    case parseBoolEqAndNeg tokens of
        Just (expr1, (AndTok : restTokens1)) ->
            case parseAndBoolEq restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (AndExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- | Function: parseBoolEqAndNeg
-- Parses boolean expressions with equality comparisons and negations.
parseBoolEqAndNeg :: [Token] -> Maybe (Bexp, [Token])
parseBoolEqAndNeg tokens =
    case parseNegAndLessAndEq tokens of
        Just (expr1, (CompareTok : restTokens1)) ->
            case parseBoolEqAndNeg restTokens1 of
                Just (expr2, restTokens2) ->
                    Just (EqBoolExp expr1 expr2, restTokens2)
                Nothing -> Nothing
        result -> result

-- | Function: parseNegAndLessAndEq
-- Parses boolean expressions with negations, less than or equal comparisons, and true/false literals.
parseNegAndLessAndEq :: [Token] -> Maybe (Bexp, [Token])
parseNegAndLessAndEq (NotTok : restTokens) =
    case parseLessOrEqOrTrueOrFalseOrParentOrArith restTokens of
        Just (expr1, restTokens1) ->
            Just (NotExp expr1, restTokens1)
        Nothing -> Nothing
parseNegAndLessAndEq restTokens = parseLessOrEqOrTrueOrFalseOrParentOrArith restTokens

-- | Function: parseLessOrEqOrTrueOrFalseOrParentOrArith
-- Parses boolean expressions with less than or equal comparisons, true/false literals, parentheses, and arithmetic expressions.
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
