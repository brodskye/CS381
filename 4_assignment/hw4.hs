module HW4 where

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
