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

module Radium.Formats.Smiles ( Smiles(..)
                             , readSmiles
                             , writeSmiles ) where
                             

import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import Data.Maybe


-- | This model describes molecules with valence bounds
data Smiles = Atom String Int
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
    s <- symbol
    n <- optionMaybe ionNumber
    _ <- char ']'
    let m = fromMaybe 0 n
    return $ Atom s m

-- Parse ion number. Ion number starts with '+' or '-'
ionNumber :: Parser Int
ionNumber =  do
    s <- char '-' <|> char '+'
    n <- number
    let m = n+1
    return $ if s == '-' then (-m) else m

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
writeSmiles (Atom xs n) | n < (-1) = "[" ++ xs ++ show n ++ "]"
                        | n == (-1) = "[" ++ xs ++ "-]"
                        | n == 1 = "[" ++ xs ++ "+]"
                        | n > 1 = "[" ++ xs ++ "+" ++ show n ++ "]"
                        | otherwise = "[" ++ xs ++ "]"
writeSmiles (Aliphatic xs) = xs
writeSmiles (Aromatic xs) = xs
writeSmiles Unknown = "*"
writeSmiles Empty = ""