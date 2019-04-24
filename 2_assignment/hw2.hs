import Data.String
import ExprSyn

Up = 1
Down = 0
Num = Int
Name = String

data Mode = Up | Down
data Pos = Num | Name
data Pars = Name Pars | Name
data Vals = Num Vals | Num

data Cmd = Pen Mode
    | Moveto (Pos,Pos)
    | Def Name ( Pars ) Cmd
    | Call Name ( Vals )
    | Cmd; Cmd
    deriving Show
