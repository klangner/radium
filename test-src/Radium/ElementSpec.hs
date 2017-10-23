module Radium.ElementSpec (spec) where

import           Radium.Element
import           Test.Hspec


spec :: Spec
spec = do

  describe "Element by atomic number" $ do
    it "Element by 12"
      (atomicNumber (element 12) `shouldBe` 12)

    it "Element by 1"
      (symbol (element 1) `shouldBe` "H")

  describe "Element by name" $ do
    it "Oxygen atomic number is 8"
      (atomicNumber (elementBySymbol "O") `shouldBe` 8)

    it "Tennessine atomic weight is 294"
      (atomWeight (elementBySymbol "Ts") `shouldBe` 294)

  describe "Count shell electrons" $ do
    it "Radium has 7 shells"
      (electronConfig (elementBySymbol "Ra") `shouldBe` [2, 8, 18, 32, 18, 8, 2])

    it "Calcium has 4 shells"
      (electronConfig (elementBySymbol "Ca") `shouldBe` [2, 8, 8, 2])

    it "19th element (Potassium) has 4 shells"
      (electronConfig (element 19) `shouldBe` [2, 8, 8, 1])

    it "Chromium has 4 shells"
      (electronConfig (element 24) `shouldBe` [2, 8, 13, 1])

  describe "Cound shell electrons for ions" $ do
    it "Ca2+ has 3 shells"
      (ionElectronConfig (Ion (element 20) 2) `shouldBe` [2, 8, 8])

    it "Cl- has 3 full shells"
      (ionElectronConfig (Ion (element 17) (-1)) `shouldBe` [2, 8, 8])

    it "H+ has no shells"
      (ionElectronConfig (Ion (element 1) 1) `shouldBe` [])

    it "Ti2+ has 3 shells"
      (ionElectronConfig (Ion (element 22) 2) `shouldBe` [2, 8, 10])

  describe "Count valance electrons" $
    it "Caesium has 1 valance electron"
      (valenceElectrons (elementBySymbol "Cs") `shouldBe` 1)

  describe "Count covalent bounds" $
    it "Caesium has 1 bound"
      (covalentBounds (elementBySymbol "Cs") `shouldBe` 1)

  describe "Count shell electrons in different states" $ do
    it "Possible carbon electron states"
      (possibleElectronConfigs (element 6) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 2], [State 1 S 2, State 2 S 1, State 2 P 3]])

    it "Possible hydrogen electron states"
      (possibleElectronConfigs (element 1) `shouldBe` [[State 1 S 1]])

    it "Possible boron electron states"
      (possibleElectronConfigs (element 5) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 1], [State 1 S 2, State 2 S 1, State 2 P 2]])

    it "Possible nitrogen electron states"
      (possibleElectronConfigs (element 7) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 3]])

    it "Possible oxygen electron states"
      (possibleElectronConfigs (element 8) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 4]])

    it "Possible sulfur electron states"
      (possibleElectronConfigs (element 16) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 6, State 3 S 2, State 3 P 4],
                                                        [State 1 S 2, State 2 S 2, State 2 P 6, State 3 S 2, State 3 P 3, State 3 D 1],
                                                        [State 1 S 2, State 2 S 2, State 2 P 6, State 3 S 1, State 3 P 3, State 3 D 2]])

    it "Possible phosphorus electron states"
      (possibleElectronConfigs (element 15) `shouldBe` [[State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 3],
                                                        [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 1, State 3 P 3, State 3 D 1]])

    it "Possible argon electron states (yeah, I know that all except the first are impossible to obtain)"
      (possibleElectronConfigs (element 18) `shouldBe` [[State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 6],
                                                        [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 5, State 3 D 1],
                                                        [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 4, State 3 D 2],
                                                        [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 3, State 3 D 3],
                                                        [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 1, State 3 P 3, State 3 D 4]])

  describe "Count shell electrons for ionized states" $ do
    it "Ca++"
      (possibleIonElectronConfigs (Ion (element 20) 2) `shouldBe` [[State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 6],
                                                                   [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 5, State 3 D 1],
                                                                   [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 4, State 3 D 2],
                                                                   [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 2, State 3 P 3, State 3 D 3],
                                                                   [State 1 S 2, State 2  S 2, State 2 P 6, State 3 S 1, State 3 P 3, State 3 D 4]])

    it "C+"
      (possibleIonElectronConfigs (Ion (element 6) 1) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 1],
                                                                  [State 1 S 2, State 2 S 1, State 2 P 2]])

    it "C-"
      (possibleIonElectronConfigs (Ion (element 6) (-1)) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 3]])

    it "O+"
      (possibleIonElectronConfigs (Ion (element 8) 1) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 3]])

    it "N+"
      (possibleIonElectronConfigs (Ion (element 7) 1) `shouldBe` [[State 1 S 2, State 2 S 2, State 2 P 2],
                                                                  [State 1 S 2, State 2 S 1, State 2 P 3]])

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
      (possibleValences (element 54) `shouldBe` [0, 2, 4, 6, 8])

  describe "Test ion valences" $ do
    it "Possible valences for C+"
      (possibleIonValences (Ion (element 6) 1) `shouldBe` [1, 3] )

    it "Possible valences for C-"
      (possibleIonValences (Ion (element 6) (-1)) `shouldBe` [3] )

    it "Possible valences for N+"
      (possibleIonValences (Ion (element 7) 1) `shouldBe` [2, 4] )

    it "Possible valences for O+"
      (possibleIonValences (Ion (element 8) 1) `shouldBe` [3] )
