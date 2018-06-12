module DeletionTests where 

import Test.HUnit    
import TestCommon

deleteFromArrayNoQuotations :: Test
deleteFromArrayNoQuotations =
    performParsingTest ["del .[2]"] "[0,2,3,4]" "[0,2,4]"

deleteFromArray :: Test
deleteFromArray =
    performParsingTest ["del .[2]"] "[\"0\",\"2\",\"3\",\"4\"]" "[\"0\",\"2\",\"4\"]"

deleteFromObject :: Test
deleteFromObject = 
    performParsingTest ["del .a"] "{\"a\": \"valuea\", \"b\": \"valueb\"}" "{\"b\":\"valueb\"}"

deleteFromObjectNoQuotations :: Test 
deleteFromObjectNoQuotations =
    performParsingTest ["del .a"] "{\"a\": \"valuea\", \"b\": 1111}" "{\"b\":1111}"

deleteFromNested :: Test 
deleteFromNested =
    performParsingTest 
        ["del .a.[0].a"] 
        "{\"a\": [{\"a\": 1, \"b\": \"nested\"}, 1111, \"stringvalue\"], \"b\": 1111}" 
        "{\"a\":[{\"b\":\"nested\"},1111,\"stringvalue\"],\"b\":1111}"

deleteWhole :: Test 
deleteWhole =
    performParsingTest 
        ["del .a.[0].a"] 
        "{\"a\": [{\"a\": 1}, 1111, \"stringvalue\"], \"b\": 1111}" 
        "{\"a\":[1111,\"stringvalue\"],\"b\":1111}"

deleteWholeRecursive :: Test 
deleteWholeRecursive =
    performParsingTest 
        ["del .a.[0].a"] 
        "{\"a\": [{\"a\": 1}], \"b\": 1111}" 
        "{\"b\":1111}"

deleteForEvery :: Test 
deleteForEvery =
    performParsingTest 
        ["del .[].a"] 
        testValue 
        "[{\"b\":\"bvalue\"},{\"b\":111},{\"b\":\"bvalue\"}]"

deleteForEveryWhole :: Test 
deleteForEveryWhole =
    performParsingTest 
        ["del .[].a, del .[].b"] 
        testValue 
        ""

