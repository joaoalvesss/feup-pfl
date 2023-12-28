-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023

import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe)
import Debug.Trace


-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Element = 
  Int Integer | Boolean Bool deriving Show

type Stack = [Element]
type State = [(String, Element)]

createEmptyStack :: Stack
createEmptyStack = []

createEmptyState :: State
createEmptyState = []


instance Eq Element where
  (Int x) == (Int y) = x == y
  (Boolean x) == (Boolean y) = x == y
  _ == _ = False

stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map showElement stack)
  where
    showElement (Int n) = show n
    showElement (Boolean True) = "True"
    showElement (Boolean False) = "False"

state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showElement val | (var, val) <- sortOn fst state]
  where
    showElement (Int n) = show n
    showElement (Boolean True) = "True"
    showElement (Boolean False) = "False"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (instruction:rest, stack, state) = case instruction of
     Push n -> run (rest, Int n : stack, state)
     Add -> run (rest, binaryOperation (\x y -> Int (toInt x + toInt y)) stack, state)
     Mult -> run (rest, binaryOperation (\x y -> Int (toInt x * toInt y)) stack, state)
     Sub -> run (rest, binaryOperation (\x y -> Int (toInt x - toInt y)) stack, state)
     Tru -> run (rest, Boolean True : stack, state)
     Fals -> run (rest, Boolean False : stack, state)
     Equ -> run (rest, comparisonOperation (\x y -> Boolean (x == y)) stack, state)
     Le -> run (rest, comparisonOperation (\x y -> Boolean (toInt x <= toInt y)) stack, state)
     And -> run (rest, binaryOperation (\x y -> if toBool x /= 0 && toBool y /= 0 then Boolean True else Boolean False) stack, state)
     Neg -> run (rest, unaryOperation (\x -> negateElement x) stack, state)
     Fetch var -> run (rest, stateLookup var stack state : stack, state)
     Store var -> run (rest, stackTail stack, stateUpdate var (stackHead stack) state)
     Noop -> run (rest, stack, state)
     Branch c1 c2 -> run (if toInt (stackHead stack) /= 0 then c1 else c2, stackTail stack, state)
     Loop c1 c2 -> run ((c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]) ++ rest, stack, state)
     where
     toInt (Int n) = n
     toInt (Boolean True) = 1
     toInt (Boolean False) = 0
     toBool (Boolean True) = 1
     toBool (Boolean False) = 0
     toBool (Int n) = error "Run-time error"


-- Helper function to handle comparisonOperation
comparisonOperation :: (Element -> Element -> Element) -> Stack -> Stack
comparisonOperation op (x:y:xs) = op x y : xs
comparisonOperation _ stack = stack


-- Helper functions
binaryOperation :: (Element -> Element -> Element) -> Stack -> Stack
binaryOperation op (x:y:xs) = op x y : xs
binaryOperation _ stack = stack

unaryOperation :: (Element -> Element) -> Stack -> Stack
unaryOperation op (x:xs) = op x : xs
unaryOperation _ stack = stack

negateElement :: Element -> Element
negateElement (Int n) = Int (negate n)
negateElement (Boolean b) = Boolean (not b)

stackHead :: Stack -> Element
stackHead (x:_) = x
stackHead _ = error "Empty stack"

stackTail :: Stack -> Stack
stackTail (_:xs) = xs
stackTail _ = error "Empty stack"

stateLookup :: String -> Stack -> State -> Element
stateLookup var stack state =
  fromMaybe (error "Variable not found") (lookup var state)

stateUpdate :: String -> Element -> State -> State
stateUpdate var val state =
  (var, val) : filter (\(v, _) -> v /= var) state

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