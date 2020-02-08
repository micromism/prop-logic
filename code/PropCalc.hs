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

evalOnTT :: [Exp] -> Exp -> Bool
evalOnTT given try
    |elem try given = True
    |elem (Not try) given = False
    |otherwise = evalOnTT' given try

evalOnTT' :: [Exp] -> Exp -> Bool
evalOnTT' _ (Prop p) = undefined
evalOnTT' given (And p q) = (&&) (evalOnTT given p) (evalOnTT given q)
evalOnTT' given (Or p q) = (||) (evalOnTT given p) (evalOnTT given q)
evalOnTT' given (Implies p q) = (||) (evalOnTT given (Not p)) (evalOnTT given q)
evalOnTT' given (Not p) = not (evalOnTT given p)

taut :: Exp -> Bool
taut sent = foldr (&&) True (map (\entry -> evalOnTT entry sent) (truthTable (atoms sent)))

taut' :: Exp -> Maybe Bool
taut' sent = combine (taut sent, taut (Not sent))
        where
        combine (True, _) = Just True
        combine (False, False) = Nothing
        combine (False, True) = Just False

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

provableGiven' :: [Exp] -> Exp -> Maybe Bool
provableGiven' [] sent = taut' sent
provableGiven' axioms sent = taut' (Implies (conjunct axioms) sent)

conjunct :: [Exp] -> Exp
conjunct [e] = e
conjunct (e:es) = And e (conjunct es)
conjunct [] = undefined
