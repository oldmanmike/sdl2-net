import Control.Exception (evaluate)
import Foreign.C.Types
import Test.Hspec
import Test.Hspec.Expectations
import Test.QuickCheck
import qualified SDL.Raw.Net as SDL

main :: IO ()
main = hspec $ do
  describe "SDL.Raw.Net.init" $ do
    it "Initialize the network API" $ do
      SDL.init `shouldReturn` (0 :: CInt)
