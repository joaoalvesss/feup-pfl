# FEUP - PFL

## Group T10_G09

- João Brandão Alves - up202108044 -> %
- Diogo Leandro Silva - up202105327 -> %

## Part 1 - Assembler Overview

The assembler serves as a fundamental component of the low-level machine, encompassing the Code to be executed, an Evaluation Stack for assessing integer numbers and booleans, and a Storage for managing variables and their corresponding values.

### Data and Types Definition

In the initial phase, the project entails the definition of Data and Types that will be utilized throughout the entire endeavor:

```haskell
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Element = 
  Int Integer | Boolean Bool deriving Show

type Stack = [Element]
type State = [(String, Element)]
```

The predefined data Inst and type Code are provided in the Specification file, while the data StackTypes has been introduced to accommodate Inteiro Integer or Booleano Bool, thereby enabling two distinct data types within the Stack, which is a type of list of StackTypes.

The type State is essentially a tuple consisting of String and StackTypes, mirroring the structure of the Stack, thereby facilitating the association of a variable with an Integer/Boolean.

### Relevant Functions

Here are the relevant functions and their descriptions:

#### `createEmptyStack :: Stack`
```haskell
createEmptyStack :: Stack
createEmptyStack = []
```
This function is used to create an empty Stack.

#### `createEmptyState :: State`
```haskell
createEmptyState :: State
createEmptyState = []
```
It is employed to generate an empty State.

#### `stack2Str :: Stack -> String`
```haskell
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map showElement stack)
  where
    showElement (Int n) = show n
    showElement (Boolean True) = "True"
    showElement (Boolean False) = "False"
```
This function converts the Stack to a string, with the assistance of an auxiliary function `showElement`.

#### `state2Str :: State -> String`
```haskell
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showElement val | (var, val) <- sortOn fst state]
  where
    showElement (Int n) = show n
    showElement (Boolean True) = "True"
    showElement (Boolean False) = "False"
```
It converts the State to a string by sorting it based on the variable's name, with the aid of an auxiliary function `showElement`.

#### `run :: (Code, Stack, State) -> (Code, Stack, State)`
```haskell
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (instruction:rest, stack, state) = case instruction of
     -- Various cases and corresponding actions
     ...
```
Through the execution of the Code and function call matching, this function enables specific actions to be performed based on specific instructions. In the event of a failure to match any of the function calls, a Run-time error is generated to ensure the inclusion of all potential and logical function calls.








## Part 2 - Compiler Overview

In this section, we delve into the development of a translation (compiler) from a small imperative programming language, encompassing arithmetic and boolean expressions, as well as statements consisting of assignments, sequences, if-then-else statements, and while loops, into lists of instructions for the previously defined machine.

### Data and Types Definition

The project involves the definition of Data and Types that will be utilized in this part:

```haskell
data Aexp 
  = IntExp Integer   -- Integer constant
  | VarExp String    -- Variable reference
  | AddExp Aexp Aexp  -- Addition Aexpession
  | MulExp Aexp Aexp    -- Multiplication Aexpession
  | NegateExp Aexp  -- Negate a number
  deriving Show

data Bexp 
  = TrueExp           -- True constant
  | FalseExp          -- False constant
  | NotExp Bexp       -- Logical NOT
  | AndExp Bexp Bexp  -- Logical AND
  | EqExp Aexp Aexp   -- Equality comparison
  | EqBoolExp Bexp Bexp -- Bool Equality comparison
  | LeExp Aexp Aexp   -- Less than or equal to comparison
  deriving Show

data Stm 
  = Assign String Aexp         -- Assignment statement
  | IfThenElse Bexp [Stm] [Stm]  -- Conditional statement
  | While Bexp [Stm]              -- Loop statement
  deriving Show
```

The defined types include `Aexp` for arithmetic expressions (including constants), `Bexp` for boolean expressions (including constants), `Stm` for statements and structures (with support for multiple statements within if-then-else and while loops).

### Relevant Functions

The following functions play a pivotal role in the translation process:

TODO