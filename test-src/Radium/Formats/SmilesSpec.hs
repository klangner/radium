module Radium.Formats.SmilesSpec (spec) where

import Radium.Model
import Radium.Formats.Smiles
import Test.Hspec


spec :: Spec
spec =

  describe "Parse SMILES string" $ 
    it "parse single atom" $ do
        let (Atom s _) = parseSmiles "[Au]"  
        s `shouldBe` "Au"
