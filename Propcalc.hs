module PropCalc where

data Exp
    | Prop Char        
    | And Exp Exp  
    | Or Exp Exp 
    | Implies Exp Exp   
    | Not Exp
    deriving (Show, Eq)

eval :: [Exp] -> Exp -> Maybe Bool
eval given try
    |elem try given = Just True
    |elem (Not try) given = Just False
    |otherwise = Nothing

atoms :: Exp -> [Char]
atoms (Prop c) = [c]
atoms _ x y  = (atoms x) ++ (atoms y)
atoms Not Exp = atoms Exp
