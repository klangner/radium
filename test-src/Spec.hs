import Test.Hspec
import Test.QuickCheck
import qualified Radium.ElementSpec
import qualified Radium.Formats.SmilesSpec


main :: IO ()
main = hspec $ do
  describe "Element info" Radium.ElementSpec.spec
  describe "Simles format reader" Radium.Formats.SmilesSpec.spec
  