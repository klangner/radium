import Test.Hspec
import Test.QuickCheck
import qualified Chemistry.ElementSpec


main :: IO ()
main = hspec $ do
  describe "Element info" Chemistry.ElementSpec.spec
  