import Test.Hspec
import Test.QuickCheck
import qualified Radium.ElementSpec


main :: IO ()
main = hspec $ do
  describe "Element info" Radium.ElementSpec.spec
  