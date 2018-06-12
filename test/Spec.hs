import Test.HUnit
import DeletionTests
import Control.Monad



tests :: Test
tests = TestList [
    TestLabel "deleteFromArray" deleteFromArray,
    TestLabel "deleteFromArrayNoQuotations" deleteFromArrayNoQuotations,
    TestLabel "deleteFromNested" deleteFromNested,
    TestLabel "deleteFromObject" deleteFromObject,
    TestLabel "deleteWhole" deleteWhole,
    TestLabel "deleteForEveryWhole" deleteForEveryWhole,
    TestLabel "deleteWholeRecursive" deleteWholeRecursive,
    TestLabel "deleteForEvery" deleteForEvery,
    TestLabel "deleteFromObjectNoQuotations" deleteFromObjectNoQuotations
    ]

main :: IO ()
main =  void $ runTestTT tests