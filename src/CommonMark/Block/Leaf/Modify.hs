module CommonMark.Block.Leaf.Modify
( modify
, modify_header
, modify_paragraph

) where

import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader (makeSetextHeader, unmakeSetextHeader)
import CommonMark.Block.Leaf.ATXHeader 
import CommonMark.Block.Leaf.Write


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ MODIFY FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

modify :: Char -> [String] -> [String] -> String -> IO()
modify typeOf args todoList fileName =
    do
        if (typeOf == 'h')
            then do
                let (newList, addCount) = modify_header args todoList todoList 0 0
                if (addCount > 0)
                    then do
                        write_file newList fileName 0
                    else do
                        print "Could not find item to modify"
            else if (typeOf == 'p')
                then do
                    let (newList, addCount) = modify_paragraph args todoList todoList 0 0
                    if (addCount > 0)
                        then do
                            write_file newList fileName 0
                        else do
                            print "Could not find item to modify"
                else do
                    print "Add later"


modify_header :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
modify_header args [x] newList indexCount addCount = 
    do 
         if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing)     
                    then do
                        let x1 = unmakeSetextHeader x
                        if ((args !! 0) == x1)
                            then do
                                let (xs, ys) = splitAt (indexCount) newList
                                let modified = makeSetextHeader (args !! 1) 1
                                let finalList = xs ++ [modified] ++ (tail ys)
                                (finalList, (addCount+1))
                            else do
                                let x2 = unmakeATXHeader x
                                if ((args !! 0) == x2)
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = makeSetextHeader (args !! 1) 1
                                        let finalList = xs ++ [modified] ++ (tail ys)
                                        (finalList, (addCount+1))
                                    else do
                                        (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let checkItem = unmakeSetextHeader x
                        let (itemCheck, _) = splitAt index (args !! 0)
                        if (length x >= index)
                            then do
                                let (removeItem, rest) = splitAt index checkItem
                                if (itemCheck == removeItem)
                                    then do
                                        let modified = (args !! 1) ++ rest
                                        let header = makeSetextHeader modified 1
                                        let (xs, ys) = splitAt (indexCount) newList 
                                        let finalList = xs ++ [header] ++ (tail ys)
                                        (finalList, (addCount+1))
                                    else do
                                        let checkItem2 = unmakeATXHeader x
                                        let (removeItem2, rest2) = splitAt index checkItem
                                        if (itemCheck == removeItem2)
                                            then do
                                                let modified = (args !! 1) ++ rest2
                                                let header = makeSetextHeader modified 1
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let finalList = xs ++ [header] ++ (tail ys)
                                                (finalList, (addCount+1))
                                            else do
                                                (newList, addCount)
                            else do
                                (newList, addCount)
            else do
                ([], 0)
modify_header args oldList newList indexCount addCount = 
    do
        if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) 
                    then do
                        let x1 = oldList !! 0 
                        let x2 = unmakeSetextHeader x1
                        let y = (args !! 0)
                        if (length newList > indexCount+1) 
                            then if (x2 == y)
                                then do
                                    let (xs, ys) = splitAt (indexCount) newList 
                                    let modified = makeSetextHeader (args !! 1) 1
                                    let finalList = xs ++ [modified] ++ tail ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let (xs, ys) = splitAt (indexCount) newList 
                                            let modified = makeSetextHeader (args !! 1) 1
                                            let finalList = xs ++ [modified] ++ tail ys
                                            (finalList, (addCount+1))
                                        else do
                                            modify_header args (tail oldList) newList (indexCount+1) addCount
                            else if (x2 == y)
                                then do
                                    let modified = makeSetextHeader (args !! 1) 1
                                    let finalList = init newList ++ [modified]
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do 
                                            let modified = makeSetextHeader (args !! 1) 1
                                            let finalList = init newList ++ [modified]
                                            (finalList, (addCount+1))
                                        else do 
                                            (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0)
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, rest) = splitAt index x
                                if (checkItem == temp)
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = (args !! 1) ++ rest
                                        let header = makeSetextHeader modified 1
                                        let finalList = xs ++ [header] ++ (tail ys)
                                        modify_header args (tail oldList) finalList (indexCount+1) (addCount+1)
                                    else do
                                        let x2 = unmakeATXHeader x
                                        let (temp2, rest2) = splitAt index x2
                                        if (checkItem == temp2)
                                            then do
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let modified = (args !! 1) ++ rest2
                                                let header = makeSetextHeader modified 1
                                                let finalList = xs ++ [header] ++ (tail ys)
                                                modify_header args (tail oldList) finalList (indexCount+1) (addCount+1)
                                            else do
                                                modify_header args (tail oldList) newList (indexCount+1) addCount
                            else do
                                modify_header args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)

modify_paragraph :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
modify_paragraph args [x] newList indexCount addCount = 
    do 
         if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing)     
                    then do
                        let x1 = unmakeSetextHeader x
                        if ((args !! 0) == x1)
                            then do
                                let (xs, ys) = splitAt (indexCount) newList
                                let modified = args !! 1
                                let finalList = xs ++ [modified] ++ (tail ys)
                                (finalList, (addCount+1))
                            else do
                                let x2 = unmakeATXHeader x
                                if ((args !! 0) == x2)
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = args !! 1
                                        let finalList = xs ++ [modified] ++ (tail ys)
                                        (finalList, (addCount+1))
                                    else do
                                        (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let checkItem = unmakeSetextHeader x
                        let (itemCheck, _) = splitAt index (args !! 0)
                        if (length x >= index)
                            then do
                                let (removeItem, rest) = splitAt index checkItem
                                if (itemCheck == removeItem)
                                    then do
                                        let modified = (args !! 1) ++ rest
                                        let (xs, ys) = splitAt (indexCount) newList 
                                        let finalList = xs ++ [modified] ++ (tail ys)
                                        (finalList, (addCount+1))
                                    else do
                                        let checkItem2 = unmakeATXHeader x
                                        let (removeItem2, rest2) = splitAt index checkItem2
                                        if (itemCheck == removeItem2)
                                            then do
                                                let modified = (args !! 1) ++ rest2
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let finalList = xs ++ [modified] ++ (tail ys)
                                                (finalList, (addCount+1))
                                            else do
                                                (newList, addCount)
                            else do
                                (newList, addCount)
            else do
                ([], 0)
modify_paragraph args oldList newList indexCount addCount = 
    do
        if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) 
                    then do
                        let x1 = oldList !! 0 
                        let x2 = unmakeSetextHeader x1
                        let y = (args !! 0)
                        if (length newList > indexCount+1) 
                            then if (x2 == y)
                                then do
                                    let (xs, ys) = splitAt (indexCount) newList 
                                    let modified = args !! 1
                                    let finalList = xs ++ [modified] ++ tail ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let (xs, ys) = splitAt (indexCount) newList 
                                            let modified = args !! 1
                                            let finalList = xs ++ [modified] ++ tail ys
                                            (finalList, (addCount+1))
                                        else do
                                            modify_paragraph args (tail oldList) newList (indexCount+1) addCount
                            else if (x2 == y)
                                then do
                                    let modified = args !! 1
                                    let finalList = init newList ++ [modified]
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do 
                                            let modified = args !! 1
                                            let finalList = init newList ++ [modified]
                                            (finalList, (addCount+1))
                                        else do 
                                            modify_paragraph args (tail oldList) newList (indexCount+1) addCount
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0)
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, rest) = splitAt index x
                                if (checkItem == temp)
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = (args !! 1) ++ rest
                                        let header = modified
                                        let finalList = xs ++ [modified] ++ (tail ys)
                                        modify_paragraph args (tail oldList) finalList (indexCount+1) (addCount+1)
                                    else do
                                        let x2 = unmakeATXHeader x
                                        let (temp2, rest2) = splitAt index x2
                                        if (checkItem == temp2)
                                            then do
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let modified = (args !! 1) ++ rest2
                                                let header = modified
                                                let finalList = xs ++ [modified] ++ (tail ys)
                                                modify_paragraph args (tail oldList) finalList (indexCount+1) (addCount+1)
                                            else do
                                                modify_paragraph args (tail oldList) newList (indexCount+1) addCount
                            else do
                                modify_paragraph args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)