module DataModule where


data Aexp 
  = IntExp Integer   -- Integer constant
  | VarExp String    -- Variable reference
  | AddExp Aexp Aexp  -- Addition Aexpession
  | MulExp Aexp Aexp    -- Multiplication Aexpession
  | NegateExp Aexp  -- Negate a number
  deriving Show


data Token
    = PlusTok -- +
    | TimesTok -- *
    | OpenTok -- (
    | CloseTok -- )
    | IntTok Integer -- num
    | VarTok String -- variable name
    | AssignmentTok -- :=
    | SemiColonTok -- ;
    | IfTok       -- if
    | ThenTok      -- then
    | ElseTok     -- else
    | WhileTok     -- while
    | DoTok          -- do
    | EqualityTok   -- ==
    | NotTok        -- not
    | TrueTok   -- True
    | FalseTok  -- False
    deriving Show

data Bexp 
  = TrueExp           -- True constant
  | FalseExp          -- False constant
  | NotExp Bexp       -- Logical NOT
  | AndExp Bexp Bexp  -- Logical AND
  | EqExp Aexp Aexp   -- Equality comparison
  | LeExp Aexp Aexp   -- Less than or equal to comparison
  deriving Show

data Stm 
  = Assign String Aexp         -- Assignment statement
  | IfThenElse Bexp [Stm] [Stm]  -- Conditional statement
  | While Bexp [Stm]              -- Loop statement
  deriving Show



