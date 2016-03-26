module Radium.Formats.SmilesSpec (spec) where

import Radium.Formats.Smiles
import Test.Hspec


spec :: Spec
spec = do

  describe "Parse Atoms" $ do
    it "parse single atom [Au]" $
        checkFormatters "[Au]"

    it "parse [*]" $
        checkFormatters  "[*]" 

    it "parse water H2O" $
        checkFormatters  "O"

    it "parse '*'" $
        checkFormatters  "*"

    it "parse isotope [13CH4]" $
        checkFormatters "[13CH4]"

    it "parse atom class [CH4:2]" $
        checkFormatters "[CH4:2]"


  describe "Parse Hydrogens" $ do
    it "parse [H+]" $
        checkFormatters  "[H+]"

    it "parse hydrogens [CH]" $
        checkFormatters  "[CH]"

    it "parse hydrogens [CH4]" $
        checkFormatters  "[CH4]"


  describe "Parse Charge" $ do
    it "parse [H+]" $
        checkFormatters  "[H+]"

    it "parse [Cl-]" $
        checkFormatters  "[Cl-]"

    it "parse [OH1-]" $
        checkFormatters  "[OH-]"

    it "parse [OH-1]" $
        checkFormatters  "[OH-]"

    it "parse [Cu+2]" $
        checkFormatters  "[Cu+2]"

    it "parse [Cu++]" $
        checkFormatters  "[Cu+2]"

    it "parse max negative charge" $
        checkFormatters  "[Cu-15]"

    it "parse max charge" $
        checkFormatters  "[Cu+15]"


  describe "Parse Isotopes" $ do
    it "parse [13CH4]" $
        checkFormatters  "[13CH4]"

    it "parse [2H+]" $
        checkFormatters  "[2H+]"

    it "parse [238U]" $
        checkFormatters  "[238U]"


  describe "Parse Organic subset" $ do
    it "parse C" $
        checkFormatters  "C"

    it "parse N" $
        checkFormatters  "N"

    it "parse Cl" $
        checkFormatters  "Cl"


  describe "Parse Bonds" $ do
    it "parse Ethane" $
        checkFormatters  "CC"

    it "parse Ethanol" $
        checkFormatters  "CCO"

    it "parse n-butylamine" $
        checkFormatters  "NCCCC"

    it "parse Ethane C=C" $
        checkFormatters  "C=C"

    it "parse hydrogen cyanide" $
        checkFormatters  "C#N"

    it "parse 2-butyne" $
        checkFormatters  "CC#CC"

    it "parse propanol" $
        checkFormatters  "CCC=O"


checkFormatters :: String -> Expectation
checkFormatters xs = fmap writeSmiles (readSmiles xs) `shouldBe` (Right xs)