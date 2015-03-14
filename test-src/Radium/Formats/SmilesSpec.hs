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

  describe "Parse isotopes" $ do
    it "parse [13CH4]" $
        checkFormatters "[13CH4]"

    it "parse [238U]" $
        checkFormatters "[238U]"


checkFormatters :: String -> Expectation
checkFormatters xs = writeSmiles (readSmiles xs) `shouldBe` xs