module Radium.Formats.SmilesSpec (spec) where

import Radium.Formats.Smiles
import Test.Hspec


spec :: Spec
spec =

  describe "Parse SMILES string" $ do 
    it "parse single atom [Au]" $
        readSmiles "[Au]" `shouldBe` Atom "Au"
 
    it "parse water H2O" $
        readSmiles "O" `shouldBe` Aliphatic "O" 
        