module ParserAexp where

import DataModule
import Data.Char (isDigit, isSpace, digitToInt)


-- | Function: parseVar
--
-- Parses a variable token into a variable expression ('VarExp').
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: A 'Maybe' pair of the parsed variable expression and the remaining tokens.
parseVar :: [Token]          -- ^ The list of tokens to be parsed.
         -> Maybe (Aexp, [Token]) -- ^ A 'Maybe' pair of the parsed variable expression and the remaining tokens.
parseVar (VarTok var : restTokens) = Just (VarExp var, restTokens)
parseVar _ = Nothing

-- | Function: parseIntOrParseVarexp
--
-- Parses an integer token or a variable token into an arithmetic expression ('Aexp').
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: A 'Maybe' pair of the parsed arithmetic expression and the remaining tokens.
parseIntOrParseVarexp :: [Token]          -- ^ The list of tokens to be parsed.
                       -> Maybe (Aexp, [Token]) -- ^ A 'Maybe' pair of the parsed arithmetic expression and the remaining tokens.
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

-- | Function: parseProdOrInt
--
-- Parses multiplication operations in the expression.
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: A 'Maybe' pair of the parsed multiplication expression and the remaining tokens.
parseProdOrInt :: [Token]          -- ^ The list of tokens to be parsed.
               -> Maybe (Aexp, [Token]) -- ^ A 'Maybe' pair of the parsed multiplication expression and the remaining tokens.
parseProdOrInt tokens =
    case parseIntOrParseVarexp tokens of
        Just (aexp1, (TimesTok : restTokens1)) ->
            case parseProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (MulExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

-- | Function: parseSumOrProdOrInt
--
-- Parses addition operations in the expression.
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: A 'Maybe' pair of the parsed addition expression and the remaining tokens.
parseSumOrProdOrInt :: [Token]          -- ^ The list of tokens to be parsed.
                    -> Maybe (Aexp, [Token]) -- ^ A 'Maybe' pair of the parsed addition expression and the remaining tokens.
parseSumOrProdOrInt tokens =
    case parseProdOrInt tokens of
        Just (aexp1, (PlusTok : restTokens1)) ->
            case parseSumOrProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (AddExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

-- | Function: parseSubOrProdOrInt
--
-- Parses subtraction operations in the expression.
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: A 'Maybe' pair of the parsed subtraction expression and the remaining tokens.
parseSubOrProdOrInt :: [Token]          -- ^ The list of tokens to be parsed.
                   -> Maybe (Aexp, [Token]) -- ^ A 'Maybe' pair of the parsed subtraction expression and the remaining tokens.
parseSubOrProdOrInt tokens =
    case parseProdOrInt tokens of
        Just (aexp1, (MinusTok : restTokens1)) ->
            case parseSubOrProdOrInt restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (SubExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

-- | Function: parseProdOrIntOrPar
--
-- Parses multiplication operations or individual integers in the expression, including parentheses.
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: A 'Maybe' pair of the parsed multiplication expression and the remaining tokens.
parseProdOrIntOrPar :: [Token]          -- ^ The list of tokens to be parsed.
                   -> Maybe (Aexp, [Token]) -- ^ A 'Maybe' pair of the parsed multiplication expression and the remaining tokens.
parseProdOrIntOrPar tokens =
    case parseIntOrParseVarexp tokens of
        Just (aexp1, (TimesTok : restTokens1)) ->
            case parseProdOrIntOrPar restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (MulExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

-- | Function: parseSumOrProdOrIntOrPar
--
-- Parses addition or multiplication operations or individual integers in the expression, including parentheses.
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: A 'Maybe' pair of the parsed addition/multiplication expression and the remaining tokens.
parseSumOrProdOrIntOrPar :: [Token]          -- ^ The list of tokens to be parsed.
                        -> Maybe (Aexp, [Token]) -- ^ A 'Maybe' pair of the parsed addition/multiplication expression and the remaining tokens.
parseSumOrProdOrIntOrPar tokens =
    case parseSubOrProdOrInt tokens of
        Just (aexp1, (PlusTok : restTokens1)) ->
            case parseSumOrProdOrIntOrPar restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (AddExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        result -> result

-- | Function: parseAexp
--
-- Parses the entire arithmetic expression from a list of tokens.
--
-- * Input: The list of tokens to be parsed.
--
-- * Output: The parsed arithmetic expression.
parseAexp :: [Token]  -- ^ The list of tokens to be parsed.
          -> Aexp     -- ^ The parsed arithmetic expression.
parseAexp tokens =
    case parseSumOrProdOrIntOrPar tokens of
        Just (aexp, []) -> aexp
        _ -> error "Run-time error"



