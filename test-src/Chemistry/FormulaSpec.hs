module Chemistry.FormulaSpec (spec) where

import Chemistry.Element
import Chemistry.Formula
import Test.Hspec


spec :: Spec
spec = do

  describe "Formula with single letter symbols" $ 
    it "HO" $ parseFormula "HO" `shouldBe` FGroup [(FElement (element 1), 1), (FElement (element 8), 1)] 

  describe "Formula with double letter symbols" $ 
    it "HO" $ parseFormula "HeO" `shouldBe` FGroup [(FElement (elementBySymbol "He"), 1), (FElement (elementBySymbol "O"), 1)] 

  describe "Formula with numbers" $ 
    it "HO" $ parseFormula "He3O2" `shouldBe` FGroup [(FElement (elementBySymbol "He"), 3), (FElement (elementBySymbol "O"), 2)] 
    