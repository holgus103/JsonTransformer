module AdditionTests where

import TestCommon
import Test.HUnit

addFieldNested :: Test 
addFieldNested =
    performParsingTest 
        ["add .[0].c=\"10\""] 
        testValue
        "[{\"c\":\"10\",\"a\":1,\"b\":\"bvalue\"},{\"a\":1,\"b\":111},{\"a\":\"asd\",\"b\":\"bvalue\"}]" 


addFieldNestedObj :: Test 
addFieldNestedObj =
    performParsingTest 
        ["add .0.c=\"10\""] 
        "{\"0\":{\"a\":1,\"b\":\"bvalue\"},\"1\":{\"a\":1,\"b\":111},\"2\":{\"a\":\"asd\",\"b\":\"bvalue\"}}"
        "{\"0\":{\"c\":\"10\",\"a\":1,\"b\":\"bvalue\"},\"1\":{\"a\":1,\"b\":111},\"2\":{\"a\":\"asd\",\"b\":\"bvalue\"}}" 


addFieldForEvery :: Test 
addFieldForEvery =
    performParsingTest 
        ["add .[].c=\"10\""] 
        testValue
        "[{\"c\":\"10\",\"a\":1,\"b\":\"bvalue\"},{\"c\":\"10\",\"a\":1,\"b\":111},{\"c\":\"10\",\"a\":\"asd\",\"b\":\"bvalue\"}]" 
