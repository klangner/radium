module Chemistry.ElementSpec (spec) where

import Chemistry.Element
import Test.Hspec


spec :: Spec
spec = do

  describe "Element by atomic number" $ 
    it "Element by 12" $ do
        let n = fmap atomicNumber $ elementByAtomNumber 12
        n `shouldBe` Just 12

  describe "Element by name" $ 
    it "Oxygen atomic number is 8" $ do
        let n = fmap atomicNumber $ element "O"
        n `shouldBe` Just 8

  describe "Count shell electrons" $ 
    it "Radium has 7 shells" $ do
        let xs = fmap shellElectrons $ element "Ra"
        xs `shouldBe` Just [2, 8, 18, 32, 18, 8, 2]

  describe "Count valance electrons" $ 
    it "Caesium has 1 valance electron" $ do
        let n = fmap valanceElectronCount $ element "Cs"
        n `shouldBe` Just 1

  describe "Count bounds" $ 
    it "Caesium has 1 bound" $ do
        let n = fmap valanceBoundCount $ element "Cs"
        n `shouldBe` Just 1
        