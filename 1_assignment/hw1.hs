-- David Jansen, Shanmukh Challa, Eytan Brodsky
-- CS381
-- Homework 1

module HW1types where

import Data.List (nub,sort)

type Node  = Int
type Edge  = (Node,Node)
type Graph = [Edge]
type Path  = [Node]
type Bag a = [(a,Int)]

type Number = Int
type Point = (Number,Number)
type Length = Number

data Shape = Pt Point
        | Circle Point Length
        | Rect Point Length Length
        deriving Show

type Figure = [Shape]
type BBox = (Point,Point)


norm :: Ord a => [a] -> [a]
norm = sort . nub

--1a
ins :: Eq a => a -> Bag a -> Bag a
ins a [] = [(a, 1)]
ins a ((x, n):as)
                | x == a = (x, n+1):as
                | otherwise = (x, n): ins a as
--1b
del :: Eq a => a -> Bag a -> Bag a
del a [] = []
del a ((x, n):as)
                | n == 1 = as
                | x == a = (x, n-1):as
                | otherwise = (x, n): del a as

--1c
bag :: Eq a => [a] -> Bag a
bag = foldr ins []

--1d
subbag :: Eq a => Bag a -> Bag a -> Bool
subbag [] b = True
subbag ((x, n):as) b
                | (checkList x n b) == False = False
                | otherwise = subbag as b


checkList :: Eq a => a -> Int -> Bag a -> Bool
checkList a b [] = False
checkList a b ((x, n):xs)
                | ((x == a) && (n <= b)) = True
                | otherwise = checkList a b xs



--1e
isbag :: Eq a => Bag a -> Bag a -> Bag a
isbag [] a = []
isbag ((x,n):as) ((y,m):bs)
        | elem x (map fst ((y,m):bs)) == True = (x, n+m):isbag as ((y,m):bs)
        | otherwise = isbag as ((y,m):bs)

--1f
size :: Bag a -> Int
size a = foldl (\b (c, d) -> b + d) 0 a

--Exercise 2. Graphs

--2a

nodes :: Graph -> [Node]
nodes = norm . foldl (\a (f,s) -> a ++ [f,s]) []

--2b

suc :: Node -> Graph -> [Node]
suc a [] = []
suc a ((x, n):as)
                | x == a = [] ++ n: suc a as
                | otherwise = suc a as

--2c

detach :: Node -> Graph -> Graph
detach a [] = []
detach a ((x, n):as)
                | x == a = detach a as
                | n == a = detach a as
                | otherwise = [] ++ (x, n): detach a as

-- 2d
cyc2 :: Graph -> Graph
cyc2 [(1, _)] = []
cyc2 [(a, _)] = cyc2 [(a-1, a)] ++ [(a-1 , a)]

cyc :: Int -> Graph
cyc a = cyc2 [(a,1)] ++ [(a,1)]

--3a
width :: Shape -> Length
width (Pt point) = 0
width (Circle point length) = 2*length
width (Rect point length1 _) = length1

--3b
bbox :: Shape -> BBox
bbox (Pt point) = (point, point)
bbox (Circle point length) = (((fst(point) - length), (snd (point) - length)), ((fst(point) + length), (snd (point) + length)))
bbox (Rect point length width) = (point, ((fst(point) +length), (snd(point)+width)))

--3c
minX :: Shape -> Number
minX (Pt point) = fst(point)
minX (Circle point length) = fst(point) - length
minX (Rect point length1 _) = fst(point)

--3d
move :: Shape -> Point -> Shape
move (Pt point1) point2 = (Pt (addPt point1 point2))
move (Circle point1 length) point2 = (Circle (addPt point1 point2) length)
move (Rect point1 length1 length2) point2 = (Rect (addPt point1 point2) length1 length2)

addPt :: Point -> Point -> Point
addPt a b = ((fst(a) + fst(b)), (snd(a) + snd(b)))

--3e
alignLeft :: Figure -> Figure
alignLeft a = (alignLeftRec a (minValue(makeXArray a)))

alignLeftRec :: Figure -> Number -> Figure
alignLeftRec [] a = []
alignLeftRec (x:xs) a = [] ++ (moveToX a x):alignLeftRec xs a

moveToX :: Number -> Shape -> Shape
moveToX num (Pt point) = (Pt (num, snd(point)))
moveToX num (Circle point length) = (Circle (num, snd(point)) length)
moveToX num (Rect point length1 length2) = (Rect (num, snd(point)) length1 length2)

minValue :: [Number] -> Number
minValue a = foldr1 min a

makeXArray :: Figure -> [Number]
makeXArray [] = []
makeXArray (x:xs) = [] ++ (extractX x):makeXArray xs

extractX :: Shape -> Number
extractX (Pt point) = fst(point)
extractX (Circle point _) = fst(point)
extractX (Rect point _ _) = fst(point)

--3f

distanceSquared :: Point -> Point -> Int
distanceSquared (x1, y1) (x2, y2) = x' ^ 2 + y' ^2
    where
    x' = x1 - x2
    y' = y1 - y2

inside :: Shape -> Shape -> Bool
inside (Circle point1 length1) (Circle point2 length2) = distanceSquared point1 point2 <= (length1 - length2) ^ 2
inside (Rect point1 length1 length2) (Circle point2 radius) = distanceSquared point1 point2 <= radius ^ 2 && distanceSquared point2 (fst point1 + length2, snd point1) <= radius ^ 2 && distanceSquared point2 (fst point1 + length2, snd point1 + length1) <= radius ^ 2 && distanceSquared point2 (fst point1, snd point1 + length1) <= radius ^ 2
inside s1 s2 = fst (fst (bbox(s1))) >= fst (fst (bbox(s2))) && snd (fst (bbox(s1))) <= snd (fst (bbox(s2))) && fst (snd (bbox(s1))) <= fst (snd (bbox(s2))) && snd (snd (bbox(s1))) >= snd (snd (bbox(s2)))
