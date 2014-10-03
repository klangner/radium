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

module Radium.Formats.Smiles (SmilesModel(..), readSmiles) where

import Text.ParserCombinators.Parsec


-- | This model describes molecules with valence bounds
data SmilesModel = Atom String
                 | Aliphatic String
                 | Aromatic String
                 | Empty
                  deriving( Eq, Show )


-- | Parses textual representation
readSmiles :: String -> SmilesModel
readSmiles xs = case parse atom "" xs of
    Left _ -> Empty
    Right val -> val

-- Parse atom
atom :: Parser SmilesModel
atom = bracketAtom <|> aliphaticOrganic

-- Parse atom
bracketAtom :: Parser SmilesModel
bracketAtom = do
    s <- between (char '[') (char ']') symbol
    return $ Atom s

-- Parse aliphatic
aliphaticOrganic :: Parser SmilesModel
aliphaticOrganic = do
    _ <- char 'O'
    return $ Aliphatic "O" 


-- Parse element symbol
-- Starts with upper case
-- has 0, 1 or 2 lower letters
symbol :: Parser String
symbol = do 
    s <- upper
    ss <- many lower
    return (s:ss)
    