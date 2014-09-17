module Chemistry.ElementSpec (spec) where

import Chemistry.Element
import Test.Hspec


spec :: Spec
spec = do

  describe "Element by atomic number" $ 
    it "Element by 12" $ do
        atomicNumber (element 12) `shouldBe` 12

  describe "Element by name" $ 
    it "Oxygen atomic number is 8" $ do
        atomicNumber (elementBySymbol "O") `shouldBe` 8

  describe "Count shell electrons" $ 
    it "Radium has 7 shells" $ do
        shellElectrons (elementBySymbol "Ra") `shouldBe` [2, 8, 18, 32, 18, 8, 2]

  describe "Count valance electrons" $ 
    it "Caesium has 1 valance electron" $ do
        valanceElectrons (elementBySymbol "Cs") `shouldBe` 1

  describe "Count covalent bounds" $ 
    it "Caesium has 1 bound" $ do
        covalentBounds (elementBySymbol "Cs") `shouldBe` 1
        