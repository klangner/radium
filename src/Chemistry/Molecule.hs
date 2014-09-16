module Chemistry.Molecule ( Molecule
                          , parseFormula
                          ) where

import Chemistry.Element

data Formula = Formula [(Element, Int)] deriving Show
data Molecule = Molecule { _moleculeElements ::[(Element, Int)]
                         , _moleculeBounds :: [(Int, Int)] 
                         } deriving Show

-- | Parse formula and generate all isomers
parseFormula :: String -> Formula
parseFormula _ = Formula []
