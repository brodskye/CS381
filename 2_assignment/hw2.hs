-- David Jansen, Eytan Brodsky, Shanmukh Challa
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

-- Question 2:

--a)
type Pair = (Int, Int)
data Circuit = Circ Gates Links | EpsilonCircuit deriving Show
data Gates = G Int GateFn Gates | EpsilonGates deriving Show
data GateFn = And | Or | Xor | Not deriving Show
data Links = From Pair Pair Links | EpsilonLinks deriving Show

--b)

circ = Circ (G 1 Xor (G 2 And (EpsilonGates))) (From (1,1) (2,1) (From (1,2) (2,2) (EpsilonLinks)))


--c)
prettyPrinterCircuit:: Circuit -> String
prettyPrinterCircuit (EpsilonCircuit) = ","
prettyPrinterCircuit (Circ a b) = prettyPrinterGates a ++ " " ++ prettyPrinterLinks b


prettyPrinterGates:: Gates -> String
prettyPrinterGates (EpsilonGates) = ","
prettyPrinterGates (G a b c) = (show a) ++ prettyPrinterGateFn b ++ " " ++ prettyPrinterGates c --Needs three arguments

prettyPrinterGateFn :: GateFn -> String
prettyPrinterGateFn (And) = "AND\n"
prettyPrinterGateFn (Or) = "OR\n"
prettyPrinterGateFn (Xor) = "XOR\n"
prettyPrinterGateFn (Not) = "NOT\n"

prettyPrinterLinks:: Links -> String
prettyPrinterLinks (EpsilonLinks) =","
prettyPrinterLinks (From (a,b) (c,d) e) = "From " ++ (show a) ++","++ (show b) ++" to "++(show c) ++","++ (show d)++" "++ prettyPrinterLinks e



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
