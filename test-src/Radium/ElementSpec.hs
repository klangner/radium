module Radium.ElementSpec (spec) where

import           Radium.Element
import           Test.Hspec


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

  describe "Count shell electrons in different states" $ do
    it "Possible carbon electron states"
      (possibleElectronConfigs 6 `shouldBe` [[(1, 1, 2), (2, 1, 2), (2, 2, 2)], [(1, 1, 2), (2, 1, 1), (2, 2, 3)]])

    it "Possible hydrogen electron states"
      (possibleElectronConfigs 1 `shouldBe` [[(1, 1, 1)]])

    it "Possible boron electron states"
      (possibleElectronConfigs 5 `shouldBe` [[(1, 1, 2), (2, 1, 2), (2, 2, 1)], [(1, 1, 2), (2, 1, 1), (2, 2, 2)]])

    it "Possible nitrogen electron states"
      (possibleElectronConfigs 7 `shouldBe` [[(1, 1, 2), (2, 1, 2), (2, 2, 3)]])

    it "Possible oxygen electron states"
      (possibleElectronConfigs 8 `shouldBe` [[(1, 1, 2), (2, 1, 2), (2, 2, 4)]])

    it "Possible sulfur electron states"
      (possibleElectronConfigs 16 `shouldBe` [[(1, 1, 2), (2, 1, 2), (2, 2, 6), (3, 1, 2), (3, 2, 4)],
                                              [(1, 1, 2), (2, 1, 2), (2, 2, 6), (3, 1, 2), (3, 2, 3), (3, 3, 1)],
                                              [(1, 1, 2), (2, 1, 2), (2, 2, 6), (3, 1, 1), (3, 2, 3), (3, 3, 2)]])

    it "Possible phosphorus electron states"
      (possibleElectronConfigs 15 `shouldBe` [[(1, 1, 2), (2, 1, 2), (2, 2, 6), (3, 1, 2), (3, 2, 3)],
                                               [(1, 1, 2), (2, 1, 2), (2, 2, 6), (3, 1, 1), (3, 2, 3), (3, 3, 1)]])

  describe "Test atom valences" $ do
    it "Possible valences for boron"
      (possibleValences (element 5) `shouldBe` [1, 3])

    it "Possible valences for carbon"
      (possibleValences (element 6) `shouldBe` [2, 4])

    it "Possible valence for nitrogen"
      (possibleValences (element 7) `shouldBe` [3])

    it "Possible valences for fluorine"
      (possibleValences (element 9) `shouldBe` [1])
      
    it "Possible valences for neon"
      (possibleValences (element 10) `shouldBe` [0])

    it "Possible valences for phosphorus"
      (possibleValences (element 15) `shouldBe` [3, 5])

    it "Possible valences for calcium"
      (possibleValences (element 20) `shouldBe` [0, 2])

    it "Possible valences for xenon"
      (possibleValences (element 54) `shouldBe` [0, 2, 4, 6])
