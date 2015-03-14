{- |
Module Radium.Formats.Smiles
Copyright : Copyright (C) 2014 Krzysztof Langner
License : BSD3

Maintainer : Krzysztof Langner <klangner@gmail.com>
Stability : alpha
Portability : portable

SMILES is popular format for describing the structure of chemical molecules.
http://en.wikipedia.org/wiki/Simplified_molecular-input_line-entry_system

-}

module Radium.Formats.Smiles ( readSmiles
                             , writeSmiles ) where
                             

import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import Data.Maybe


-- | This model describes molecules with valence bounds
data Smiles = Atom  String      -- Symbol
                    Int         -- Isotopes count
                    Int         -- Hydrogen count
                    Int         -- Ion number (charge)
                    Int         -- Atom class
            | Aliphatic String
            | Aromatic String
            | Unknown
            | Empty
              deriving( Eq, Show )


-- | Set of all aliphatic symbols
aliphatics :: Set.Set String
aliphatics = Set.fromList ["B", "C", "N", "O", "S", "P", "F", "Cl", "Br", "I" ]

-- | Set of all aromatic symbols
aromatics :: Set.Set Char
aromatics = Set.fromList "bcnosp"

-- | Parses textual representation
readSmiles :: String -> Smiles
readSmiles xs = case parse atom "" xs of
    Left _ -> Empty
    Right val -> val

-- Parse atom
atom :: Parser Smiles
atom = bracketAtom <|> aliphaticOrganic <|> aromaticOrganic <|> unknown

-- Parse atom
bracketAtom :: Parser Smiles
bracketAtom = do
    _ <- char '['
    i <- optionMaybe number
    s <- symbolOrUnknown
    hc <- optionMaybe hcount
    n <- optionMaybe charge
    ac <- optionMaybe atomClass
    _ <- char ']'
    return $ Atom s (fromMaybe 0 i) (fromMaybe 0 hc) (fromMaybe 0 n) (fromMaybe 0 ac)
    
-- Accept symbol or unknown '*' character    
symbolOrUnknown :: Parser String
symbolOrUnknown = symbol <|> string "*"    

-- Parse hydrogen
hcount :: Parser Int
hcount =  do
    _ <- char 'H'
    hc <- optionMaybe number
    let n = fromMaybe 1 hc
    return $ if n == 0 then 1 else n

-- Parse ion number (charge). Ion number starts with '+' or '-'
charge :: Parser Int
charge =  do
    s <- char '-' <|> char '+'
    n <- number
    let m = if n == 0 then 1 else n
    return $ if s == '-' then (-m) else m

-- Parse atom class
atomClass :: Parser Int
atomClass =  do
    _ <- char ':'
    number

-- Parse number of elements. If number not found then return 1
number :: Parser Int
number =  do
    ds <- many digit
    return $ if null ds then 0 else read ds :: Int

-- Parse aliphatic
aliphaticOrganic :: Parser Smiles
aliphaticOrganic = do
    ss <- symbol
    if Set.member ss aliphatics then return (Aliphatic ss) else fail "" 

-- Parse aromatic
aromaticOrganic :: Parser Smiles
aromaticOrganic = do
    ss <- lower
    if Set.member ss aromatics then return (Aromatic [ss]) else fail "" 

-- Parser for '*' symbol
unknown :: Parser Smiles
unknown = do
    _ <- char '*'
    return Unknown

-- Parse element symbol
-- Starts with upper case
-- has 0, 1 or 2 lower letters
symbol :: Parser String
symbol = do 
    s <- upper
    ss <- many lower
    return (s:ss)
    
    
-- | Write SMILES to string    
writeSmiles :: Smiles -> String
writeSmiles (Atom xs ic hc n ac) = "[" ++ showIsotopes ++ xs ++ showHyrdogen ++ showCharge ++ showClass ++ "]"
    where showIsotopes = if ic > 0 then show ic else ""
          showHyrdogen | hc > 1 = "H" ++ show hc
                       | hc == 1 = "H"
                       | otherwise = ""
          showCharge | n < (-1) = show n
                     | n == (-1) = "-"
                     | n == 1 = "+"
                     | n > 1 = "+" ++ show n
                     | otherwise = ""
          showClass = if ac > 0 then ":" ++ show ac else ""

writeSmiles (Aliphatic xs) = xs
writeSmiles (Aromatic xs) = xs
writeSmiles Unknown = "*"
writeSmiles Empty = ""