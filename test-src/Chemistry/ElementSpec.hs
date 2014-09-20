module Chemistry.ElementSpec (spec) where

import Chemistry.Element
import Test.Hspec


spec :: Spec
spec = do

  describe "Element by atomic number" $ 
    it "Element by 12" $ 
        atomicNumber (element 12) `shouldBe` 12

  describe "Element by name" $ 
    it "Oxygen atomic number is 8" $ 
        atomicNumber (elementBySymbol "O") `shouldBe` 8

  describe "Count shell electrons" $ 
    it "Radium has 7 shells" $ 
        electronConfig (elementBySymbol "Ra") `shouldBe` [2, 8, 18, 32, 18, 8, 2]

  describe "Count shell electrons" $ 
    it "Calcium has " $ 
        electronConfig (element 19) `shouldBe` [2, 8, 8, 1]

  describe "Count shell electrons in metal" $ 
    it "Chromium has " $ 
        electronConfig (element 24) `shouldBe` [2, 8, 13, 1]

  describe "Count valance electrons" $ 
    it "Caesium has 1 valance electron" $ 
        valanceElectrons (elementBySymbol "Cs") `shouldBe` 1

  describe "Count covalent bounds" $ 
    it "Caesium has 1 bound" $ 
        covalentBounds (elementBySymbol "Cs") `shouldBe` 1
        