-- A unit test to demonstrate the correct functionality of the library for
-- ATXHeaders.
-- By: Andrew Miller

-- Runs the test
-- For each part of the test three things should be printed
-- 1. Whether the test should return true or false, and why
-- 2. What the test is testing (the block)
-- 3. What the test returns (True or False)

import CommonMark.Block.Leaf.ATXHeader

main :: IO()
main = do
        putStrLn "Test 1: This should return true"
        let test1 = "# foo"
        putStrLn test1
        putStrLn (show (isATXHeader test1))

        putStrLn "Test 2: This should return true"
        let test2 = "## foo"
        putStrLn test2
        putStrLn (show (isATXHeader test2))

        putStrLn "Test 3: This should return true"
        let test3 = "### foo"
        putStrLn test3
        putStrLn (show (isATXHeader test3))

        putStrLn "Test 4: This should return true"
        let test4 = "#### foo"
        putStrLn test4
        putStrLn (show (isATXHeader test4))

        putStrLn "Test 5: This should return true"
        let test5 = "##### foo"
        putStrLn test5
        putStrLn (show (isATXHeader test5))

        putStrLn "Test 6: This should return true"
        let test6 = "###### foo"
        putStrLn test6
        putStrLn (show (isATXHeader test6))

        putStrLn "Test 7: This should return false as more than six #'s is invalid"
        let test7 = "###### foo"
        putStrLn test7
        putStrLn (show (isATXHeader test7))

        putStrLn "Test 8: This should return false as there is no space after the #'s"
        let test8 = "#5 bolt"
        putStrLn test8
        putStrLn (show (isATXHeader test8))

        putStrLn "Test 9: This should return false as there is no space after the #'s"
        let test9 = "#foobar"
        putStrLn test9
        putStrLn (show (isATXHeader test9))

        putStrLn "Test 10: This should return false as it has an escape sequence"
        let test10 = "\\## foo"
        putStrLn test10
        putStrLn (show (isATXHeader test10))

        putStrLn "Test 11: This should return true"
        let test11 = "# foo *bar* \\*baz\\*"
        putStrLn test11
        putStrLn (show (isATXHeader test11))

        putStrLn "Test 12: This should return true"
        let test12 = "#       foo              "
        putStrLn test12
        putStrLn (show (isATXHeader test12))
