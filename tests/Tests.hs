import           ServerTest
import           TimeWarpTest
import           Test.Tasty
import           Test.Tasty.Ingredients.Basic   ( consoleTestReporter )
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  defaultMainWithIngredients [consoleTestReporter] timeWarpTests

