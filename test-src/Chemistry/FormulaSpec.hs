module Chemistry.FormulaSpec (spec) where

import Chemistry.Element
import Chemistry.Formula
import Test.Hspec


spec :: Spec
spec = do

  describe "Formula with single letter symbols" $ do
    it "HO" $ parseFormula "HO" `shouldBe` Formula [(element 1, 1), (element 8, 1)] 

  describe "Formula with double letter symbols" $ 
    it "HO" $ parseFormula "HeO" `shouldBe` Formula [(elementBySymbol "He", 1), (elementBySymbol "O", 1)] 

  describe "Formula with numbers" $ 
    it "HO" $ parseFormula "He3O2" `shouldBe` Formula [(elementBySymbol "He", 3), (elementBySymbol "O", 2)] 
    