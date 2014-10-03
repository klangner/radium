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

module Radium.Formats.Smiles where

import Text.ParserCombinators.Parsec
import Radium.Model


-- | This formated uses String model.
type SmilesModel = ValenceModel String

-- | Parses textual representation
parseSmiles :: String -> SmilesModel
parseSmiles xs = case parse atom "" xs of
    Left _ -> Empty
    Right val -> Atom val []

-- Parse ion
atom :: Parser String
atom = between (char '[') (char ']') symbol


-- Parse element symbol
-- Starts with upper case
-- has 0, 1 or 2 lower letters
symbol :: Parser String
symbol = do 
    s <- upper
    ss <- many lower
    return (s:ss)
    