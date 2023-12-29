-- In ParserBexp.hs

-- module ParserBexp where

import DataModule
import AuxLexer
import ParserAexp

aux :: String -> Maybe (Bexp, [Token])
aux input = parseBexp (lexer input)

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp tokens = parseAnd tokens

parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens = 
    case parseNot tokens of
        Just (bexp1, (AndTok : restTokens1)) ->
            case parseAnd restTokens1 of
                Just (bexp2, restTokens2) ->
                    Just (AndExp bexp1 bexp2, restTokens2)
                Nothing -> Nothing
        result -> result

parseNot :: [Token] -> Maybe (Bexp, [Token])
parseNot (NotTok : restTokens) =
    case parseAtom restTokens of
        Just (bexp, restTokens1) ->
            Just (NotExp bexp, restTokens1)
        Nothing -> Nothing
parseNot tokens = parseAtom tokens

parseAtom :: [Token] -> Maybe (Bexp, [Token])
parseAtom (TrueTok : restTokens) = Just (TrueExp, restTokens)
parseAtom (FalseTok : restTokens) = Just (FalseExp, restTokens)

parseAtom (OpenTok : restTokens1) =
    case parseBexp restTokens1 of
        Just (bexp, (CloseTok : restTokens2)) ->
            Just (bexp, restTokens2)
        _ -> Nothing


