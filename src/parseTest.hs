import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader (makeSetextHeader, unmakeSetextHeader)
import CommonMark.Block.Leaf.ATXHeader 
import CommonMark.Block.Leaf.Add
import CommonMark.Block.Leaf.Remove
import CommonMark.Block.Leaf.Append
import CommonMark.Block.Leaf.Prepend
import CommonMark.Block.Leaf.Modify


-- user indicates the type of item that they're adding 
dispatch :: [(String, [String] -> [String] ->  String -> IO ())]  
dispatch =  [ ("p", p) 
            , ("l", l)
            , ("h", h)
            ]  
dispatch2 :: [(String, Char -> [String] -> [String] -> String -> IO())]
dispatch2 = [("add", add)
            , ("remove", remove)
            , ("append", append)
            , ("prepend", prepend)
            , ("modify", modify)
            ]


-- the purpose for these secondary arguments is to let the user type one secondary command (i.e., add, append/prepend, etc.) and one letter to indicate
-- the type of object they are hoping to add
-- If this were not allowed, then we would not know if the user was adding a new list item, code block, paragraph, etc. 
-- each one has their own add/remove/append/prepend/modify functions 

-- Function for adding/modifying, etc. paragraphs
p :: [String] -> [String] -> String -> IO()
p args todoList fileName = 
    do
        if (length args > 0)
            then do
                let secondArg = head args
                if (secondArg == "add" || secondArg == "remove" || secondArg == "append" || secondArg == "prepend" || secondArg == "modify")
                    then do
                        let (Just action) = lookup secondArg dispatch2
                        action 'p' (tail args) todoList fileName
                    else do 
                        putStrLn "Invalid secondary command - please type h for the list of commands"
            else do
                putStrLn "Missing arguments"

-- Function for adding/modifying/etc. headers 
h :: [String] -> [String] -> String -> IO()
h args todoList fileName = 
    do
        if (length args > 0)
            then do               
                let secondArg = head args 
                if (secondArg == "add" || secondArg == "remove" || secondArg == "append" || secondArg == "prepend" || secondArg == "modify")
                    then do
                        let (Just action) = lookup secondArg dispatch2
                        action 'h' (tail args) todoList fileName
                    else do 
                        putStrLn "Invalid secondary command - please type h for the list of commands"
             else do
                putStrLn "Missing arguments"

-- Function for adding/modifying/etc. list items 
l :: [String] -> [String] -> String -> IO()
l args todoList fileName = 
    do
        if (length args > 0)
            then do
                let secondArg = head args 
                if (secondArg == "add" || secondArg == "remove" || secondArg == "append" || secondArg == "prepend" || secondArg == "modify")
                    then do
                        let (Just action) = lookup secondArg dispatch2
                        action 'l' (tail args) todoList fileName
                    else do 
                        putStrLn "Invalid secondary command - please type h for the list of commands"
             else do
                putStrLn "Missing arguments"


help_command :: IO()
help_command = 
    do
        putStrLn "To add"

-- Checks to see if the strings are a type A heading - that is, if they are two strings that are a setext header with '='
-- Header
-- ======
checkAHeading :: String -> String -> Bool
checkAHeading xs ys = 
    do
        let index = elemIndex '=' ys -- checks to see if the second line has at least 1 '='
        if (index == Nothing)
            then False
            else do
                let (Just indexOf) = elemIndex '=' ys -- if there is one, we need to extract out the integer using a Just
                if (indexOf > 3) -- if the '=' appears more than 3 spaces from the beginning of the line, this is not a header (too far indented)
                    then False
                    else if (length xs > 0) -- if the first line is not an empty string
                        then True
                        else False

-- Checks to see if the strings are a type A heading - that is, if they are two strings that are a setext header with '='
-- Header
-- ------
checkBHeading :: String -> String -> Bool
checkBHeading xs ys =
    do
        let index = elemIndex '-' ys -- checks to see if the second line has at least 1 '-'
        if (index == Nothing)
            then False
            else do
                 let (Just indexOf) = elemIndex '-' ys -- if there is one, we need to extract out the integer using a Just
                 if (indexOf > 3) -- if the '-' appears more than 3 spaces from the beginning of the line, this is not a header (too far indented)
                    then False
                    else if (length ys < 1) -- if there is only 1 '-', then this is not a setext header. This is actually a list item. We account for this case.
                        then False
                        else if (length xs > 0)
                            then True
                            else False

-- Checks to see if the strings are a type A heading - that is, if they are two strings that are a setext header with '='
-- # Header
checkCHeading :: String -> Bool
checkCHeading xs = 
    do
        let index = elemIndex '#' xs -- checks to see if the second line has at least 1 '#'
        if (index == Nothing)
            then False
            else do
                let (Just indexOf) = elemIndex '#' xs -- if there is one, we need to extract out the integer using a Just
                if (indexOf > 3) -- if the '#' appears more than 3 spaces from the beginning of the line, this is not a header (too far indented)
                    then False
                    else if (length xs < 1) -- if there is only 1 '#', then is is not an ATX header. This is just <p>#</p>
                        then False
                        else True

-- Checks to see how many lines long the paragraph is
-- For now, if it's not a header, it's a section. This is incorrect and will be changed when the "check if item" function is made 
checkParagraph :: [String] -> Int -> Int
checkParagraph [x] count = 
    do
        -- checks to see if x is a list item or a header
        -- if not, it is a paragraph item and it increases count
        -- otherwise, it returns the old count
        if (checkCHeading x == False && length x > 0)
            then 
                do
                    (count+1)
            else 
                do 
                    count


checkParagraph oldList count = 
    -- checks to see if x is a list item or a header
        -- if not, it is a paragraph item and it increases count
        -- otherwise, it reutns the old count
        -- for now, I'm just checking to see if it's a header - we'll need a check for list item later, but that will be in the modules that are yet unwritten
    do
        if (length oldList > 1)
            then do
                let a = head oldList
                let b = head (tail oldList)
                if (checkAHeading a b == True || checkBHeading a b == True)
                    then 
                        (count-1) -- if we found out that part of what we thought was a paragraph before was actually part of a title, we subtract one
                    else do
                        if (checkCHeading a == False && length a > 0)
                            then do
                                checkParagraph (tail oldList) (count+1)
                            else do 
                                count

            else if (length oldList > 0)
                then do
                    let x = head oldList
                    if (checkCHeading x == False && length x > 0)
                        then 
                            do
                                checkParagraph (tail oldList) (count+1)
                        else 
                            do 
                                count
                else do
                    count



create_list :: [String] -> IO()
create_list list = 
    print(list)



preprocessor :: [String] -> [String] -> [String]
preprocessor oldList newList =
    do
        if (length oldList > 1)
            then do
                let xs = oldList !! 0
                let ys = oldList !! 1
                if (checkAHeading xs ys == True) -- if it is a "=" header 
                    then do
                        let a = [xs] ++ [ys]
                        let b = unlines a
                        let list = newList ++ [b]
                        let (c, d) = splitAt 2 oldList
                        preprocessor d list -- gets rid of xs, ys in the remaining list
                    else if (checkBHeading xs ys == True) -- if it is a "-" header 
                        then do
                            let a = [xs] ++ [ys]
                            let b = unlines a
                            let list = newList ++ [b] 
                            preprocessor (tail (tail oldList)) list
                        else if (checkCHeading xs == True)
                            then do
                                 let list1 = tail oldList
                                 let list2 = newList ++ [(xs++"\n")]
                                 preprocessor list1 list2
                            else 
                                do
                                    let numPara = checkParagraph oldList 0 -- checks how many lines are part of the <p> block, if any
                                    if (numPara > 0)
                                        then do
                                            let (a, b) = splitAt (numPara+1) oldList 
                                            let oneString = unlines a -- contains all the lines that need to be concatenated into 1 string
                                            if (length b > 0) -- if the remaining items left to check have at least one element, then we call the function again
                                                then do
                                                    let list = newList ++ [oneString] 
                                                    preprocessor b list
                                                else do -- if not, then we've combined all the strings we needed to. We return this list of combined strings.
                                                    let f = show numPara
                                                    newList ++ [oneString]
                                        -- else, it's a regular list item. These do not need to be concatenated in any special way, so we just add xs to newList and call the function again
                                        -- it could also be an ATX heading - however, these are only on 1 line as well, so it does not matter
                                        else do 
                                             let list1 = tail oldList
                                             let list2 = newList ++ [xs]
                                             preprocessor list1 list2
                                
            else if (length oldList > 0)
                then do
                    let xs = oldList !! 0
                    newList ++ [xs ++ "\n"]
                else do
                    newList



main = do
    (fileName:args) <- getArgs 
    if (length args > 0)
         then do
             let command = head args
             if (command == "p" || command == "l" || command == "h" || command == "create_list")
                then do
                     -- if they're creating a new file/list, then we don't check to see if the file exists 
                     if (command == "create_list")
                         then do 
                             create_list args
                         else do  
                             let (Just action) = lookup command dispatch
                             fileExists <- doesFileExist fileName  
                             if fileExists  
                                then do 
                                     let file = unsafePerformIO . readFile $ fileName
                                     let a = lines file
                                     let preprocessedList = preprocessor a []
                                     action (tail args) preprocessedList fileName 
                                else do
                                    putStrLn "This file does not exist"
                else 
                    putStrLn "Invalid argument -  please type h for command help"
         else if (fileName == "h")
            then do
                help_command
            else do
                putStrLn "Incorrect number of arguments"    