module Stack (Stack, push, pop, top, empty, isEmpty, calc) where
data Stack a = Stk [a] deriving Show

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False


calc :: Stack Float -> String -> Stack Float
calc stk str 
    |str == "+" = push (top (pop stk) + top stk ) (pop (pop stk)) 
    |str == "-" = push (top (pop stk) - top stk ) (pop (pop stk))
    |str == "/" = push (top (pop stk) / top stk ) (pop (pop stk))
    |str == "*" = push (top (pop stk) * top stk ) (pop (pop stk))
    |otherwise = push (read str::Float) stk   
