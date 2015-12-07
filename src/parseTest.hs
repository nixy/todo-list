import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List


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

            else do
                let x = head oldList
                if (checkCHeading x == False && length x > 0)
                    then 
                        do
                            checkParagraph (tail oldList) (count+1)
                    else 
                        do 
                            count


preprocessor :: [String] -> [String] -> [String]
preprocessor oldList newList =
    do
        if (length oldList > 1)
            then do
                let xs = head oldList
                let ys = head (tail oldList)
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
                                 let list2 = newList ++ [xs]
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
                                
            else do
                let xs = head oldList
                newList ++ [xs]



main = do
    args <- getArgs 
    if (length args > 0)
        then do
            let fileName = head args
            fileExists <- doesFileExist fileName  
            if fileExists  
                then 
                    do 
                        let file = unsafePerformIO . readFile $ fileName
                        let a = lines file
                        let b = preprocessor a []
                        print b
                else print "no file"
        else print "blah"