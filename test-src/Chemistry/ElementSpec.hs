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
        