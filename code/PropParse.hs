module PropParse where

import PropCalc (Exp(Prop, And, Or, Implies, Not))
import Text.Parsec
import Control.Applicative hiding ((<|>))

pExp :: Parsec String () Exp
pExp =      try (And <$> pBlock <* pAnd <*> pBlock)
        <|> try (Or <$> pBlock <* pOr <*> pBlock)
        <|> try (Implies <$> pBlock <* pImplies <*> pBlock)
        <|> try (pNot *> (Not <$> pBlock))
        <|> pBlock

pBlock :: Parsec String () Exp
pBlock = char '(' *> pExp <* char ')'
        <|> Prop <$> letter

pAnd :: Parsec String () Char
pAnd = char '&' >> char '&'

pOr :: Parsec String () Char
pOr = char '|' >> char '|'

pImplies :: Parsec String () Char
pImplies = char '=' >> char '>'

pNot :: Parsec String () Char
pNot = char '~' <|> char '-'
