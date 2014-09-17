import Test.Hspec
import Test.QuickCheck
import qualified Chemistry.ElementSpec
import qualified Chemistry.FormulaSpec


main :: IO ()
main = hspec $ do
  describe "Element info" Chemistry.ElementSpec.spec
  describe "Formula info" Chemistry.FormulaSpec.spec
  