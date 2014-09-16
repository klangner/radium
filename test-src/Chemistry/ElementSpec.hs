module Chemistry.ElementSpec (spec) where

import Chemistry.Element
import Test.Hspec


spec :: Spec
spec = do

  describe "Element by atomic number" $ 
    it "Element by 12" $ do
        let n = fmap atomicNumber $ element 12
        n `shouldBe` Just 12

  describe "Element by name" $ 
    it "Oxygen atomic number is 8" $ do
        let n = fmap atomicNumber $ elementBySymbol "O"
        n `shouldBe` Just 8

  describe "Count shell electrons" $ 
    it "Radium has 7 shells" $ do
        let xs = fmap shellElectrons $ elementBySymbol "Ra"
        xs `shouldBe` Just [2, 8, 18, 32, 18, 8, 2]

  describe "Count valance electrons" $ 
    it "Caesium has 1 valance electron" $ do
        let n = fmap valanceElectrons $ elementBySymbol "Cs"
        n `shouldBe` Just 1

  describe "Count covalent bounds" $ 
    it "Caesium has 1 bound" $ do
        let n = fmap covalentBounds $ elementBySymbol "Cs"
        n `shouldBe` Just 1
        