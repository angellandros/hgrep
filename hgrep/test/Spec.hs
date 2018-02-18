
import Test.HUnit
import Grammar

-- main :: IO ()
main = runTestTT testList

testNull1 = TestCase $ assertEqual "nullable" (nullable RegExEmpty) False
testNull2 = TestCase $ assertEqual "nullable" (nullable (RegExAtom (AtomEpsilon (GEpsilonC)))) True
testNull3 = TestCase $ assertEqual "nullable" (nullable (RegExAtom (AtomLit (GLitC 'a')))) False

testRParse1 = TestCase $ assertEqual "str2regex" (str2regex "c") (RegExAtom (AtomLit (GLitC 'c')))

testMatch1 = TestCase $ assertEqual "match" (match "b" (str2regex "a|b")) True
testMatch2 = TestCase $ assertEqual "match" (match "abb" (str2regex "(a|b)*")) True
testMatch3 = TestCase $ assertEqual "match" (match "a" (str2regex "ab*")) True
testMatch4 = TestCase $ assertEqual "match" (match "abbb" (str2regex "ab*")) True
testMatch5 = TestCase $ assertEqual "match" (match "b" (str2regex "a*")) False

testList = TestList [
    TestLabel "nullable nothing = False" testNull1, 
    TestLabel "nullable epsilon = True" testNull2,
    TestLabel "nullable literal = False" testNull3,
    TestLabel "str2regex 'a' = atom 'a'" testRParse1,
    TestLabel "b ~ a|b" testMatch1,
    TestLabel "abb ~ (a|b)*" testMatch2,
    TestLabel "a ~ ab*" testMatch3,
    TestLabel "abbb ~ ab*" testMatch4,
    TestLabel "b /~ a*" testMatch5]
