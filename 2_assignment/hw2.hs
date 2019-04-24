-- David Jansen, Eytan Brodskey, Shanmukh Challa
-- Homework 2

module Homework2 where
import ExprSyn
-- Question 1:

--a). 
data Mode = Up | Down deriving Show
data Pos = I Int | S String deriving Show
type Pars = [String]
type Vals = [Int]

data Cmd = Pen Mode
         | Moveto (Pos, Pos)
         | Def String (Pars) Cmd
         | Call String (Vals)
         | Cmd Cmd
         deriving Show

--b).
--vector = Def "vector" (x1 x2 y1 y2) [Pen Up, Moveto(x1, y1), Pen Down, Moveto(x2, y2), Pen Up]

--c).

steps :: Int -> [Cmd]
steps 0 = []
steps n = [ Pen Up, Moveto(I n, I n), Pen Down, Moveto( I (n-1), I n), Moveto( I (n-1), I (n-1)) ] ++ steps(n-1)


-- Question 3:

data Op = Add | Multiply | Negate deriving Show
data Exp = Num Int
        | Apply Op [Exp]
        deriving Show
--a)
expr = Apply Negate [Apply Multiply [(Apply Add [Num 3, Num 4]), Num 7]]

--b)
--The advantage of the top syntax is that it is easier to read and more similar to what we've seen in class.
--A disadvantage is that it is more difficult to parse, since it is left recursive.
--The advantage of the bottom syntax is that it is non left recursive, making it easier to parse.
--A disadvantage is that it is harder to read at first.

--c)
translate :: Expr -> Exp
translate (N x) = Num x
translate (Plus x y) = Apply Add [translate(x), translate(y)]
translate (Times x y) = Apply Multiply [translate(x), translate(y)]
translate (Neg x) = Apply Negate [translate(x)]