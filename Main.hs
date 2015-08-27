import Reforest
import Data.List
import Data.Either
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  cont <- case args of
            [] -> getContents
            [fn] -> readFile fn
  let Right terms = mapM parseTerm (lines cont)
  mapM_ print $ sort $ findTratGrammar terms
