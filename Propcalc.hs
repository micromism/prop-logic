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

evalOnTT :: [Exp] -> Exp -> Maybe Bool
evalOnTT given try
    |elem try given = Just True
    |elem (Not try) given = Just False
    |otherwise = evalOnTT' given try

evalOnTT' :: [Exp] -> Exp -> Maybe Bool
evalOnTT' _ (Prop p) = Nothing
evalOnTT' given (And p q) = liftA2 (&&) (evalOnTT given p) (evalOnTT given q)
evalOnTT' given (Or p q) = liftA2 (||) (evalOnTT given p) (evalOnTT given q)
evalOnTT' given (Implies p q) = liftA2 (||) (evalOnTT given (Not p)) (evalOnTT given q)
evalOnTT' given (Not p) = fmap not (evalOnTT given p)

taut :: Exp -> Bool
taut sent = truthy (foldr (liftA2 (&&)) (Just True) (map (\entry -> evalOnTT entry sent) (truthTable (atoms sent))))

truthy :: Maybe Bool -> Bool
truthy (Just True) =  True
truthy (Just False) = False
truthy Nothing = False

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

provableGiven :: [Exp] -> Exp -> Bool
provableGiven axioms sent = taut (Implies (conjunct axioms) sent)

conjunct :: [Exp] -> Exp
conjunct [e] = e
conjunct (e:es) = And e (conjunct es)
conjunct [] = undefined
