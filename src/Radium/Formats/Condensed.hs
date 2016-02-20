{- |
Module : Radium.Formats.FormulaParser
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

Parser for condensed formula format (http://en.wikipedia.org/wiki/Structural_formula#Condensed_formulas).
.
Formula can be entered as H2O, 2H2O, SO4+2 (Sulfate) or (CH3)2CO (Acetone)
-}

module Radium.Formats.Condensed ( Molecule(..)
                                , readCondensed
                                , writeCondensed
                                ) where

import Text.ParserCombinators.Parsec

data Molecule a = Ion (Molecule a) Int
                | Molecule [Molecule a] Int    
                | Element a Int
                deriving (Eq, Show)
                
type SymbolMolecule = Molecule String                 
              
-- Molecule is a functor              
instance Functor Molecule where  
    fmap f (Ion m n) = Ion (fmap f m) n  
    fmap f (Molecule ms n) = Molecule (map (fmap f) ms) n
    fmap f (Element s n) = Element (f s) n        

-- | Parse formula 
--
-- > parseFormula "C2H4" `shouldBe` Molecule [Element "C" 2, Element "H", 4)]  
readCondensed :: String -> SymbolMolecule
readCondensed xs = case parse ion "" xs of
    Left _ -> Molecule [] 0
    Right val -> val

-- Parse ion
-- E.g H-
ion :: Parser SymbolMolecule
ion = do 
    c <- number
    m <- formula
    n <- optionMaybe ionNumber
    return $ case n of
        Just val -> Ion (Molecule m c) val
        _ -> Molecule m c

-- Parse formula
formula :: Parser [SymbolMolecule]
formula = many (subformula <|> element)

-- Parse subformula in brackets '(' ')'.
-- E.g. (CH3)2CO
subformula :: Parser SymbolMolecule
subformula = do
    s <- between (char '(') (char ')') formula
    n <- number
    return (Molecule s n) 
    
-- Parse element
-- Element consists of name and number
element :: Parser SymbolMolecule
element = do
    s <- symbol
    n <- number
    return (Element s n) 
       
-- Parse element symbol
-- Starts with upper case
-- has 0, 1 or 2 lower letters
symbol :: Parser String
symbol = do 
    s <- upper
    ss <- many lower
    return (s:ss)
    
       
-- Parse number of elements. If number not found then return 1
number :: Parser Int
number =  do
    ds <- many digit
    return $ if null ds then 1 else read ds :: Int

-- Parse ion number. Ion number starts with '+' or '-'
ionNumber :: Parser Int
ionNumber =  do
    s <- char '-' <|> char '+'
    n <- number
    return $ if s == '+' then n else (-n)


-- | Write Molecule to string
writeCondensed :: SymbolMolecule -> String
writeCondensed (Ion x n) = writeCondensed x ++ writeIon n
writeCondensed (Molecule xs n) =  writeNumber n ++ concatMap writeCondensed2 xs
writeCondensed (Element x n) = x ++ writeNumber n

-- | Write Molecule to string
writeCondensed2 :: SymbolMolecule -> String
writeCondensed2 (Molecule xs n) =  "(" ++ concatMap writeCondensed2 xs ++ ")" ++ writeNumber n
writeCondensed2 m = writeCondensed m

-- | Write ion number in format expected by formula
writeIon :: Int -> String
writeIon 0 = ""
writeIon 1 = "+"
writeIon (-1) = "-"
writeIon n = if n > 1 then "+" ++ show n else show n

-- | Write number in format expected by formula
writeNumber :: Int -> String
writeNumber 0 = ""
writeNumber 1 = ""
writeNumber (-1) = "-"
writeNumber n = show n
