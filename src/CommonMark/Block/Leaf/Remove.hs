module CommonMark.Block.Leaf.Remove
( remove
, remove_header
, remove_paragraph

) where

import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader (makeSetextHeader, unmakeSetextHeader)
import CommonMark.Block.Leaf.ATXHeader 
import CommonMark.Block.Leaf.Write



-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ REMOVE FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

remove :: Char -> [String] -> [String] -> String -> IO()
remove typeOf args todoList fileName =
    do
        if (typeOf == 'h')
            then do
                let (newList, addCount) = remove_header args todoList todoList 0 0
                if (addCount > 0)
                    then do
                        write_file newList fileName 0
                    else do
                        print "Could not find item to remove"
            else if (typeOf == 'p')
                then do 
                    let (newList, addCount) = remove_paragraph args todoList todoList 0 0
                    if (addCount > 0)
                    then do
                        write_file newList fileName 0
                    else do
                        print "Could not find item to remove"
                else do
                    print "Add later"

remove_header :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
remove_header args [x] newList indexCount addCount = 
    do
        if (length args > 0)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing)     
                    then do
                        let x2 = unmakeSetextHeader x
                        if ((args !! 0) == x2)
                            then do
                                let finalList = init newList 
                                (finalList, (addCount+1))
                            else do
                                let x3 = unmakeATXHeader x
                                if ((args !! 0) == x3)
                                    then do
                                        let finalList = init newList
                                        (finalList, (addCount+1))
                                    else do
                                        (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let checkItem = unmakeSetextHeader x
                        let (itemCheck, _) = splitAt index (args !! 0)
                        if (length x >= index)
                            then do
                                let (removeItem, _) = splitAt index checkItem
                                if (itemCheck == removeItem)
                                    then do
                                        let finalList = init newList 
                                        (finalList, (addCount+1))
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == itemCheck)
                                            then do
                                                let finalList = init newList 
                                                (finalList, (addCount+1))
                                            else do
                                                (newList, addCount)
                            else do
                                (newList, addCount)
            else do
                ([], 0)

remove_header args oldList newList indexCount addCount = 
    do
        if (length args > 0)
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
                                    let finalList = xs ++ (tail ys)
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let (xs, ys) = splitAt (indexCount) newList 
                                            let finalList = xs ++ (tail ys)
                                            (finalList, (addCount+1))
                                        else do
                                            remove_header args (tail oldList) newList (indexCount+1) addCount
                            else if (x2 == y)
                                then do
                                    let finalList = init newList
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let finalList = init newList
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0)
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, _) = splitAt index x
                                if (checkItem == temp)
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let finalList = xs ++  (tail ys)
                                        remove_header args (tail oldList) finalList (indexCount) (addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (checkItem == x4) 
                                            then do
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let finalList = xs ++  (tail ys)
                                                remove_header args (tail oldList) finalList (indexCount) (addCount+1)
                                            else do
                                                remove_header args (tail oldList) newList (indexCount+1) addCount
                            else do
                                remove_header args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)


remove_paragraph :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
remove_paragraph args [x] newList indexCount addCount = 
    do
        if (length args > 0)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing)     
                    then do
                        if ((args !! 0) == x)
                            then do
                                let finalList = init newList 
                                (finalList, (addCount+1))
                            else do
                                (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (itemCheck, _) = splitAt index (args !! 0)
                        if (length x >= index)
                            then do
                                let (removeItem, _) = splitAt index x
                                if (itemCheck == removeItem)
                                    then do
                                        let finalList = init newList 
                                        (finalList, (addCount+1))
                                    else do
                                        (newList, addCount)
                            else do
                                (newList, addCount)
            else do
                ([], 0)

remove_paragraph args oldList newList indexCount addCount = 
    do
        if (length args > 0)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) 
                    then do
                        let x1 = oldList !! 0 
                        let y = (args !! 0)
                        if (length newList > indexCount+1) 
                            then if (x1 == y)
                                then do
                                    let (xs, ys) = splitAt (indexCount) newList 
                                    let finalList = xs ++ (tail ys)
                                    (finalList, (addCount+1))
                                else do
                                    remove_header args (tail oldList) newList (indexCount+1) addCount
                            else if (x1 == y)
                                then do
                                    let finalList = init newList
                                    (finalList, (addCount+1))
                                else do
                                    (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0)
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, _) = splitAt index x
                                if (checkItem == temp)
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let finalList = xs ++  (tail ys)
                                        remove_header args (tail oldList) finalList (indexCount) (addCount+1)
                                    else do
                                        remove_header args (tail oldList) newList (indexCount+1) addCount
                            else do
                                remove_header args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)
