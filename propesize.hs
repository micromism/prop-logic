import PropParse
import PropCalc
import Control.Monad
import Text.Parsec

main = do
    putStrLn "How many statements would you like to assume?"
    num <- getLine
    putStrLn ("Reading in " ++ num ++ ".")
    assumptions <- sequence (replicate (read num::Int) getLine)
    putStrLn ("What statement would you like to check?")
    toCheck <- getLine
    putStrLn (proofRes assumptions toCheck)

rExp :: String -> Exp
rExp e = case (parse pExp "" (rmSpace e)) of
           Left err -> error (show err)
           Right exp -> exp

rmSpace :: String -> String
rmSpace = filter (/=' ')

--proofRes' :: Maybe Bool -> String
--pproofRes' (Just True) = "Provable True"
--pproofRes' (Just False) = "Provable False"
--pproofRes' Nothing = "Indeterminate under given assumptions"

proofRes :: [String] -> String -> String
proofRes givenS proveS = proofRes' (provableGiven (map rExp givenS) (rExp proveS))
                    where
                        proofRes' True = "Provable true."
                        proofRes' False = "Not provable."
