module Radium.Formats.SmilesSpec (spec) where

import Radium.Formats.Smiles
import Test.Hspec


spec :: Spec
spec =

  describe "Parse SMILES string" $ do 
    it "parse single atom [Au]" $
        checkFormatters "[Au]"
 
    it "parse water H2O" $
        checkFormatters  "O"  
 
    it "parse aromatic" $
        checkFormatters  "c" 
 
    it "parse '*'" $
        checkFormatters  "*" 
        
    it "parse [H+]" $
        checkFormatters  "[H+]" 
        
    it "parse [*]" $
        checkFormatters  "[*]" 
        
    it "parse hydrogens [CH]" $
        checkFormatters  "[CH]" 
        
    it "parse hydrogens [CH4]" $
        checkFormatters  "[CH4]"
        

checkFormatters :: String -> Expectation
checkFormatters xs = writeSmiles (readSmiles xs) `shouldBe` xs