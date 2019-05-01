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

type Macros = [(String, Prog)]
type State = [String]

semCmd2 :: Cmd -> D
semCmd2 (LD a)  xs         = [a] ++ xs
semCmd2 (ADD)   (x1:x2:xs) = [x1+x2] ++ xs
semCmd2 (MULT)  (x1:x2:xs) = [x1*x2] ++ xs
semCmd2 (DUP)   (x1:xs)    = [x1,x1] ++ xs
semCmd2 _       _          = []
semCmd2 (DEF s a)          = 

