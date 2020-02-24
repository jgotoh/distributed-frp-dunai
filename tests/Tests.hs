-- import           ServerTest
import           TimeWarpTest
import           BearRiverTimeWarpTest
import           ExtraTest
import           Test.Tasty
import           Test.Tasty.Ingredients.Basic   ( consoleTestReporter )
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  defaultMainWithIngredients [consoleTestReporter] tests

-- TODO add again serverTests when they are fixed.
tests :: TestTree
tests = testGroup "All tests" [bearRiverExtraTests]
-- tests = testGroup "All tests" [timeWarpTests, bearRiverTimeWarpTests, bearRiverExtraTests]

