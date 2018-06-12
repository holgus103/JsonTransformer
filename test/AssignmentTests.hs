module AssignmentTests where 

import TestCommon
import Test.HUnit


assignFieldNested :: Test 
assignFieldNested =
    performParsingTest 
        [".[0].a=\"10\""] 
        testValue
        "[{\"a\":\"10\",\"b\":\"bvalue\"},{\"a\":1,\"b\":111},{\"a\":\"asd\",\"b\":\"bvalue\"}]" 

assignArray :: Test 
assignArray =
    performParsingTest 
        [".[0]=\"10\""] 
        testValue
        "[\"10\",{\"a\":1,\"b\":111},{\"a\":\"asd\",\"b\":\"bvalue\"}]" 


assignFieldNestedObj :: Test 
assignFieldNestedObj =
    performParsingTest 
        [".0.a=\"10\""] 
        "{\"0\":{\"a\":1,\"b\":\"bvalue\"},\"1\":{\"a\":1,\"b\":111},\"2\":{\"a\":\"asd\",\"b\":\"bvalue\"}}"
        "{\"0\":{\"a\":\"10\",\"b\":\"bvalue\"},\"1\":{\"a\":1,\"b\":111},\"2\":{\"a\":\"asd\",\"b\":\"bvalue\"}}" 


arrayAssignmentRelative :: Test
arrayAssignmentRelative = 
    performParsingTest 
        [".[1]=[0]"] 
        testValue
        "[{\"a\":1,\"b\":\"bvalue\"},{\"a\":1,\"b\":\"bvalue\"},{\"a\":\"asd\",\"b\":\"bvalue\"}]" 

        
        

assignFieldForEvery :: Test 
assignFieldForEvery =
    performParsingTest 
        [".[].a=\"10\""] 
        testValue
        "[{\"a\":\"10\",\"b\":\"bvalue\"},{\"a\":\"10\",\"b\":111},{\"a\":\"10\",\"b\":\"bvalue\"}]" 
