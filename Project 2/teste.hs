import Data.Char (isDigit, isSpace, digitToInt)


data Aexp =
  IntExp Integer |  -- Integer constant
  VarExp String |   -- Variable reference
  AddExp Aexp Aexp | -- Addition Expression
  MulExp Aexp Aexp |   -- Multiplication Expression
  NegateExp Aexp  -- Negate a number
  deriving Show

data Bexp =
  TrueExp |         -- True constant
  FalseExp |        -- False constant
  NotExp Bexp |     -- Logical NOT
  AndExp Bexp Bexp | -- Logical AND
  EqExp Aexp Aexp |  -- Equality comparison
  LeExp Aexp Aexp    -- Less than or equal to comparison
  deriving Show

data Stm =
  Assign String Aexp |        -- Assignment statement
  IfThenElse Bexp [Stm] [Stm] | -- Conditional statement
  While Bexp [Stm]              -- Loop statement
  deriving Show

data Token
    = PlusTok -- +
    | TimesTok -- *
    | OpenTok -- (
    | CloseTok -- )
    | IntTok Integer -- num
    | TrueTok
    | FalseTok
    | NotTok
    | AndTok
    | EqTok
    | LeTok
    | IdentTok String
    | IfTok
    | ThenTok
    | ElseTok
    | WhileTok
    | DoTok
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('=' : restStr) = EqTok : lexer restStr
lexer ('<' : restStr) = LeTok : lexer restStr
lexer ('&' : '&' : restStr) = AndTok : lexer restStr
lexer ('!' : restStr) = NotTok : lexer restStr
lexer ('T' : 'r' : 'u' : 'e' : restStr) = TrueTok : lexer restStr
lexer ('F' : 'a' : 'l' : 's' : 'e' : restStr) = FalseTok : lexer restStr
lexer str@(chr : restStr)
    | isSpace chr = lexer restStr
lexer str@(chr : _)
    | isDigit chr
    = IntTok (fromIntegral (stringToInt digitStr)) : lexer restStr
    where
        (digitStr, restStr) = break (not . isDigit) str
        -- convert a string to an integer
        stringToInt :: String -> Int
        stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0


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




parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp = parseSumOrProdOrIntOrPar

-- Parsing Boolean expressions
parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp (TrueTok : restTokens) = Just (TrueExp, restTokens)
parseBexp (FalseTok : restTokens) = Just (FalseExp, restTokens)
parseBexp (NotTok : restTokens) =
    case parseBexp restTokens of
        Just (bexp, restTokens') -> Just (NotExp bexp, restTokens')
        Nothing -> Nothing
parseBexp tokens =
    case parseAexp tokens of
        Just (aexp1, (EqTok : restTokens1)) ->
            case parseAexp restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (EqExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        Just (aexp1, (LeTok : restTokens1)) ->
            case parseAexp restTokens1 of
                Just (aexp2, restTokens2) ->
                    Just (LeExp aexp1 aexp2, restTokens2)
                Nothing -> Nothing
        _ -> Nothing

-- Parsing statements
parseStm :: [Token] -> Maybe (Stm, [Token])
parseStm (IdentTok var : restTokens) =
    case restTokens of
        (EqTok : restTokens1) ->
            case parseAexp restTokens1 of
                Just (aexp, restTokens2) ->
                    Just (Assign var aexp, restTokens2)
                Nothing -> Nothing
        _ -> Nothing
parseStm (IfTok : restTokens) =
    case parseBexp restTokens of
        Just (bexp, (ThenTok : restTokens1)) ->
            case parseStmSeq restTokens1 of
                Just (stmThen, (ElseTok : restTokens2)) ->
                    case parseStmSeq restTokens2 of
                        Just (stmElse, restTokens3) ->
                            Just (IfThenElse bexp stmThen stmElse, restTokens3)
                        Nothing -> Nothing
                Nothing -> Nothing
        _ -> Nothing
parseStm (WhileTok : restTokens) =
    case parseBexp restTokens of
        Just (bexp, (DoTok : restTokens1)) ->
            case parseStmSeq restTokens1 of
                Just (stmSeq, restTokens2) ->
                    Just (While bexp stmSeq, restTokens2)
                Nothing -> Nothing
        _ -> Nothing
parseStm _ = Nothing

parseStmSeq :: [Token] -> Maybe ([Stm], [Token])
parseStmSeq tokens =
    case parseStm tokens of
        Just (stm, restTokens) ->
            case parseStmSeq restTokens of
                Just (stmSeq, restTokens') ->
                    Just (stm : stmSeq, restTokens')
                Nothing -> Nothing
        Nothing -> Just ([], tokens)


parse :: String -> [Stm]
parse str =
    case parseStmSeq (lexer str) of
        Just (stmSeq, []) -> stmSeq
        _ -> error "Parse error"


