module PropCalc where

import Data.List
import Control.Applicative

data Exp
    = Prop Char        
    | And Exp Exp
    | Or Exp Exp
    | Implies Exp Exp
    | Not Exp
    deriving (Show, Eq)

eval :: [Exp] -> Exp -> Maybe Bool
eval given try
    |elem try given = Just True
    |elem (Not try) given = Just False
    |otherwise = eval' given try

eval' :: [Exp] -> Exp -> Maybe Bool
eval' _ (Prop p) = Nothing
eval' given (And p q) = liftA2 (&&) (eval given p) (eval given q)
eval' given (Or p q) = liftA2 (||) (eval given p) (eval given q)
eval' given (Implies p q) = liftA2 (||) (eval given (Not p)) (eval given q)
eval' given (Not p) = fmap not (eval given p)

taut :: Exp -> Bool
taut sent = unMaybe (foldr (liftA2 (&&)) (Just True) (map (\entry -> eval entry sent) (truthTable (atoms sent))))

unMaybe :: Maybe Bool -> Bool
unMaybe (Just True) =  True
unMaybe (Just False) = False
unMaybe Nothing = False

truthTable :: [Char] -> [[Exp]]
truthTable = foldr (\x y -> (map ((Prop x):) y) ++ (map ((Not (Prop x)):) y)) [[]]

atoms' :: Exp -> [Char]
atoms' (Prop c) = [c]
atoms' (And x y)  = (atoms' x) ++ (atoms' y)
atoms' (Or x y)  = (atoms' x) ++ (atoms' y)
atoms' (Implies x y)  = (atoms' x) ++ (atoms' y)
atoms' (Not x) = atoms' x

atoms :: Exp -> [Char]
atoms = nub . atoms'
