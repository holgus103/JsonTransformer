module MultiOpTests where 

import Test.HUnit    
import TestCommon

severalOpsTest :: Test
severalOpsTest =
    performParsingTest 
        ["del .[].a,", "add .[1].c=\"17\",", ".[].b=\"newval\""]
        testValue
        "[{\"b\":\"newval\"},{\"b\":\"newval\",\"c\":\"17\"},{\"b\":\"newval\"}]"