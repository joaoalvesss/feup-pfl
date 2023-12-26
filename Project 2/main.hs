-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe)

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


type Stack = [Integer]
type State = [(String, Integer)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []

-- VER ISTO
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map showBool stack)
  where
    showBool 0 = "False"
    showBool 1 = "True"
    showBool n = show n

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showVal val | (var, val) <- sortOn fst state]
  where
    showVal 0 = "False"
    showVal 1 = "True"
    showVal n = show n

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) -- If code is empty, return the final state
run (instruction:rest, stack, state) = case instruction of
     Push n -> run (rest, n : stack, state)
     Add -> run (rest, binaryOperation (+) stack, state)
     Mult -> run (rest, binaryOperation (*) stack, state)
     Sub -> run (rest, binaryOperation (-) stack, state)
     Tru -> run (rest, 1 : stack, state)
     Fals -> run (rest, 0 : stack, state)
     Equ -> run (rest, comparisonOperation (==) stack, state)
     Le -> run (rest, comparisonOperation (<=) stack, state)
     And -> run (rest, binaryOperation (\x y -> if x /= 0 && y /= 0 then 1 else 0) stack, state)
     Neg -> run (rest, unaryOperation negate stack, state)
     Fetch var -> run (rest, stateLookup var stack state : stack, state)
     Store var -> run (rest, stackTail stack, stateUpdate var (stackHead stack) state)
     Noop -> run (rest, stack, state)
     Branch c1 c2 -> run (if stackHead stack /= 0 then c1 else c2, stackTail stack, state)
     Loop c1 c2 -> run (c1 ++ [Branch c2 c1], stack, state)


-- Helper functions
binaryOperation :: (Integer -> Integer -> Integer) -> Stack -> Stack
binaryOperation op (x:y:xs) = op x y : xs
binaryOperation _ stack = stack

unaryOperation :: (Integer -> Integer) -> Stack -> Stack
unaryOperation op (x:xs) = op x : xs
unaryOperation _ stack = stack

comparisonOperation :: (Integer -> Integer -> Bool) -> Stack -> Stack
comparisonOperation op (x:y:xs) = if op x y then 1 : xs else 0 : xs
comparisonOperation _ stack = stack

stackHead :: Stack -> Integer
stackHead (x:_) = x
stackHead _ = error "Empty stack"

stackTail :: Stack -> Stack
stackTail (_:xs) = xs
stackTail _ = error "Empty stack"

stateLookup :: String -> Stack -> State -> Integer
stateLookup var stack state = fromMaybe (error "Variable not found") (lookup var state)

stateUpdate :: String -> Integer -> State -> State
stateUpdate var val state = (var, val) : filter (\(v, _) -> v /= var) state

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
     where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

createEmptyStore = undefined -- TODO
store2Str = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, store2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")