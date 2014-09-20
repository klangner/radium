module Chemistry.Formula ( Formula(..)
                          , parseFormula
                          ) where

import Text.ParserCombinators.Parsec
import Chemistry.Element


data Formula = Formula [(Element, Int)] deriving (Eq, Show)

-- | Parse formula 
--
-- > parseFormula "C2H4" `shouldBe` Formula [(element 6, 2), (element 1, 4)]  

parseFormula :: String -> Formula
parseFormula xs = case parse formula "" xs of
    Left _ -> Formula []
    Right val -> Formula val


-- Parse whole formula
formula :: Parser [(Element, Int)]
formula = many elementSymbol
    
-- Parse element
-- Element is max 3 letter long
-- Starts with upper case
-- has 0, 1 or 2 lower letters
elementSymbol :: Parser (Element, Int)
elementSymbol = do
    l1 <- upper
    l2 <- many lower
    let e = elementBySymbol (l1:l2) 
    ds <- many digit
    let n = if null ds then 1 else read ds :: Int
    return (e, n)
