-- Eytan Brodskey, David Jansen, Shanmukh Challa
-- Homework 3

module Homework3 where

--Exercise 1

type Prog = [Cmd]

data Cmd = LD Int
        | ADD
        | MULT
        | DUP
        | DEF String Prog
        | CALL String
        deriving Show


type Stack = [Int]

type D = Stack -> Stack 
--semCmd :: Cmd -> Stack -> Stack

semCmd :: Cmd -> D
semCmd (LD a)  xs         = [a] ++ xs
semCmd (ADD)   (x1:x2:xs) = [x1+x2] ++ xs
semCmd (MULT)  (x1:x2:xs) = [x1*x2] ++ xs
semCmd (DUP)   (x1:xs)    = [x1,x1] ++ xs
semCmd _       _          = []


sem :: Prog -> D
sem [] x = x 
sem (x:xs) y = sem xs (semCmd x y)

semPrint :: Prog -> Stack
semPrint p = sem p []


--Exercise 2

--Example:
--semPrint2 [LD 5, LD 6, DEF "SQR" [DUP, MULT], CALL "SQR"]
--result: [36, 5]

type Macros = [(String, Prog)]
type State = [String]

sem2 :: Prog -> E
sem2 [] (x, s) = (x, s)
sem2 (x:xs) (k,s) = sem2 xs (semCmd2 x (k,s))

type E = (Stack, Macros) -> (Stack, Macros)

semCmd2 :: Cmd -> E
semCmd2 (LD a) (xs, s)         = ([a] ++ xs, s)
semCmd2 (ADD)  ((x1:x2:xs), s) = ([x1+x2] ++ xs, s)
semCmd2 (MULT) ((x1:x2:xs), s) = ([x1*x2] ++ xs, s)
semCmd2 (DUP)  ((x1:xs), s)    = ([x1,x1] ++ xs, s)
semCmd2 (DEF str a) (xs, s)    = (xs, s++[(str, a)])
semCmd2 (CALL str) (xs, (s, p):ss)   | str == s = sem2 p (xs, (s, p):ss)

semPrint2 :: Prog -> Stack
semPrint2 p = fst (sem2 p ([], []))

