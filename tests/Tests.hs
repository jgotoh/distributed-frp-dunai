import           ServerTest
import           TimeWarpTest
import           BearRiverDRMTest
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

tests :: TestTree
tests = testGroup "All tests" [serverTests, timeWarpTests, bearRiverTimeWarpTests, bearRiverExtraTests, bearRiverDRMTests]

