module HW4 where

-- Exercise 1
-- a
type Prog = [Cmd]
data Cmd = LD Int
		 | ADD
		 | MULT
		 | DUP
		 | INC
		 | SWAP
		 | POP Int
		 deriving Show

type Rank = Int
type CmdRank = (Int, Int)

rankC :: Cmd -> CmdRank
rankC (LD e) = (0, 1)
rankC ADD = (2, 1)
rankC MULT = (2, 1)
rankC DUP = (1, 2)
rankC INC = (1, 1)
rankC SWAP = (2, 2)
rankC (POP e) = (e, 0)

rankP :: Prog -> Maybe Rank
rankP [] = Just 0
rankP prog = rank prog 0

rank :: Prog -> Rank -> Maybe Rank
rank [] a = Just a
rank (x:xs) a   | fst (rankC x) > a = Nothing
                | otherwise = rank xs ((a - fst (rankC x)) + snd (rankC x))

-- b
type Stack = [Int]
semStatTC :: Prog -> Maybe Stack
semStatTC prog  | rankP prog == Nothing = Nothing
                | otherwise = Just (semProg prog)


semProg :: Prog -> Stack
semProg = foldl (flip sem) []

sem :: Cmd -> Stack -> Stack
sem (LD x) s = (x:s)
sem ADD (x1:x2:s) = ((x1+x2):s)
sem MULT (x1:x2:s) = ((x1*x2):s)
sem DUP (x:s) = (x:x:s)
sem INC (x:s) = ((x+1):s)
sem SWAP (x1:x2:s) = (x2:x1:s)
sem (POP a) s = (drop a) s

-- Exercise 2
data Shape = X
		| TD Shape Shape
		| LR Shape Shape
		deriving Show

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox (TD a b)
    | ax >= bx = (ax, ay + by)
    | ax < bx = (bx, ay + by)
    where (ax, ay) = bbox a
          (bx, by) = bbox b
bbox (LR a b)
    | ay >= by = (ax + bx, ay)
    | ay < by = (ax + bx, by)
    where (ax, ay) = bbox a
          (bx, by) = bbox b
bbox X = (1, 1)

rect :: Shape -> Maybe BBox
rect X = Just(1,1)
rect (TD c d) = case(cx == cx) of
		  True -> Just(cx, cy+cy)
		  False -> Nothing
		  where (cx, cy) = bbox c
		  	(dx, dy) = bbox d

-- Exercise 3

--a).
    --1). f :: [a] -> a -> [a]
    --    g :: [b] -> b' -> [b']

    --2). For the first one f returns a list if it is null because null will return true if x is an empty list. 
      --In the case of x being true it will return a list of the type y. If x is not true it will return x which is a list as well.
      
      --For the second one g returns an empty list if x ends up being true, this is because the not will negate it to be false  which will then return [] an empty lit.
      --If x ends up not being null the negation will make x end up being true so it will again return a list, this time made up of y.
    
    --3). In this case g is more general because it can work with more than a single type while f is constrained to one type.
        --Both are very general though since they can work with any type.
    
    --4). For f we must have the same type for x and for [y], so this means if we select a type for either one the other will have to follow that same type.
        --For g we can return an empty list based on x so that means x does not need to be specified or match y.

--b). 
h :: [b] -> [(a, b)] -> [b]
h b a = map snd a

--c). 
k :: (a -> b) -> ((a -> b) -> a) -> b
k a b =  a ( b ( a ) )

--d). No you can't because you do not know what the types of a and b are so they could be anything or completely different from one another.
    --Also if you attempt to define a function type for it you need to know what type b is. If we attempt to do a definition it could change what b is therefor restricting it from being any type.
