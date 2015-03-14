module Radium.Formats.CondensedSpec (spec) where

import Radium.Formats.Condensed
import Test.Hspec


spec :: Spec
spec = 

  describe "Formula parser" $ do 
    it "parse formula with single elements" $ do
        readCondensed "HO" `shouldBe` Molecule [Element "H" 1, Element "O" 1] 1
        checkFormatters  "HO"

    it "parse formula with multi letter element symbols" $ do
        readCondensed "HeO" `shouldBe` Molecule [Element "He" 1, Element "O" 1] 1
        checkFormatters  "HeO"

    it "parse formula with multiple elements" $ do
        readCondensed "He3O2" `shouldBe` Molecule [Element "He" 3, Element "O" 2] 1
        checkFormatters  "He3O2"

--    it "parse formula with groups" $ do
--        readCondensed "(CH3)2CO" `shouldBe` Molecule [ Molecule [Element "C" 1, Element "H" 3] 2, Element "C" 1, Element "O" 1] 1
--        checkFormatters  "(CH3)2CO"

    it "parse formula with ions +2" $ do
        readCondensed "SO4+2" `shouldBe` Ion (Molecule [Element "S" 1, Element "O" 4] 1) 2
        checkFormatters  "SO4+2"

    it "parse formula with ions -2" $ do
        readCondensed "SO4-2" `shouldBe` Ion (Molecule [Element "S" 1, Element "O" 4] 1) (-2)
        checkFormatters  "SO4-2"

    it "parse formula with ions -" $ do
        readCondensed "H-" `shouldBe` Ion (Molecule [Element "H" 1] 1) (-1)
        checkFormatters  "H-"

    it "parse formula with multiple moles" $ do
        readCondensed "2H2O" `shouldBe` Molecule [Element "H" 2, Element "O" 1] 2
        checkFormatters  "2H2O"

    
checkFormatters :: String -> Expectation
checkFormatters xs = writeCondensed (readCondensed xs) `shouldBe` xs