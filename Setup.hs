import Distribution.Simple

import System.Process
import System.Exit

main :: IO ()
main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests =  runTestSuite }

runTestSuite _ _ _ _ =
 do pid <- runCommand "ghc -e main quickcheck.lhs"
    waitForProcess pid >>= exitWith
