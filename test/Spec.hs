import Test.HUnit

import DeletionTests
import AdditionTests
import AssignmentTests
import MultiOpTests
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
    TestLabel "deleteFromObjectNoQuotations" deleteFromObjectNoQuotations,
    TestLabel "addFieldNested" addFieldNested,
    TestLabel "addFieldNestedObj" addFieldNestedObj,
    TestLabel "addFieldForEvery" addFieldForEvery,
    TestLabel "assignFieldNested" assignFieldNested,
    TestLabel "assignFieldNestedObj" assignFieldNestedObj,
    TestLabel "assignFieldForEvery" assignFieldForEvery,
    TestLabel "assignArray" assignArray,
    TestLabel "arrayAssignmentRelative" arrayAssignmentRelative,
    TestLabel "relativeAdd" relativeAdd,
    TestLabel "arrayAddition" arrayAddition,
    TestLabel "arrayAdditionRelative" arrayAdditionRelative,
    TestLabel "severalOpsTest" severalOpsTest
    ]

main :: IO ()
main =  void $ runTestTT tests