-- | Module: Stack
-- Module for a simple stack implementation with basic operations and a calculator function.
module Stack (
    -- * Data Type
    Stack,
    -- * Stack Operations
    push,
    pop,
    top,
    empty,
    isEmpty,
    -- * Calculator Function
    calc
) where

-- | Data Type: Stack
-- A stack data type with elements of type 'a'.
data Stack a = Stk [a] deriving Show

-- | Function: push
-- Pushes an element onto the top of the stack.
push :: a        -- ^ The element to be pushed onto the stack.
     -> Stack a  -- ^ The original stack.
     -> Stack a  -- ^ The stack after pushing the element.
push x (Stk xs) = Stk (x:xs)

-- | Function: pop
-- Pops the top element off the stack.
pop :: Stack a    -- ^ The original stack.
    -> Stack a    -- ^ The stack after popping the top element.
pop (Stk (_:xs)) = Stk xs
pop _            = error "Run-time error"

-- | Function: top
-- Retrieves the top element of the stack.
top :: Stack a  -- ^ The stack.
    -> a        -- ^ The top element of the stack.
top (Stk (x:_)) = x
top _           = error "Run-time error"

-- | Constant: empty
-- An empty stack.
empty :: Stack a
empty = Stk []

-- | Function: isEmpty
-- Checks if the stack is empty.
isEmpty :: Stack a  -- ^ The stack.
        -> Bool     -- ^ 'True' if the stack is empty, 'False' otherwise.
isEmpty (Stk []) = True
isEmpty (Stk _)  = False

-- | Function: calc
-- Performs basic arithmetic operations (+, -, *, /) and pushes the result onto the stack.
calc :: Stack Float  -- ^ The original stack.
     -> String       -- ^ The operation to be performed or a number to be pushed.
     -> Stack Float  -- ^ The stack after performing the operation or pushing the number.
calc stk str
    | str == "+"    = push (top (pop stk) + top stk) (pop (pop stk))
    | str == "-"    = push (top (pop stk) - top stk) (pop (pop stk))
    | str == "/"    = push (top (pop stk) / top stk) (pop (pop stk))
    | str == "*"    = push (top (pop stk) * top stk) (pop (pop stk))
    | otherwise     = push (read str :: Float) stk
