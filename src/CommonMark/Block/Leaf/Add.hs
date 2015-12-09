module CommonMark.Block.Leaf.Add
( add
, add_header
, add_paragraph
) where

import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader (makeSetextHeader, unmakeSetextHeader)
import CommonMark.Block.Leaf.ATXHeader 
import CommonMark.Block.Leaf.Write


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ADD FUNCTIONS  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-- Adds an item to the list at a given location
-- if typeOf is h, it adds a header under a header
-- if typeOf is p, it adds a paragraph under a header
-- if typeOf is l, it adds a list item under a header 
add :: Char -> [String] -> [String] -> String -> IO()
add typeOf args todoList fileName = 
    do
        if (typeOf == 'h')
            then do
                -- Sets newList, addCount to add_header 
                -- if addCount is 0, that means we did not find the location to add the header to 
                let (newList, addCount) = add_header args todoList todoList 0 0
                if (addCount > 0)
                    then do
                        write_file newList fileName 0
                    else do
                        print "Could not find location to add to - try append/prepend if you do not know the structure of the list"
            else if (typeOf == 'p')
                then do
                    -- Sets newList, addCount to add_header 
                    -- if addCount is 0, that means we did not find the location to add the paragraph to 
                    let (newList, addCount) = add_paragraph args todoList todoList 0 0 
                    if (addCount > 0)
                        then do
                            write_file newList fileName 0
                        else do
                            print "Could not find location to add to - try append/prepend if you do not know the structure of the list"
                else do
                    putStrLn "Add later"

add_header :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
add_header args [] newList indexCount addCount = 
    do
        (newList, addCount)
add_header args [x] newList indexCount addCount = 
    do
         if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 1) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if they are not, then we do exact string matching
                    then do
                        let x2 = unmakeSetextHeader x 
                        let y = (args !! 1)
                        if (length newList > indexCount+1) 
                            then if (x2 == y)
                                then do
                                    let newItem = makeSetextHeader (args !! 0) 1
                                    let (xs, ys) = splitAt (indexCount+1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                    if (x3 == y)
                                        then do 
                                            let newItem = makeSetextHeader (args !! 0) 1
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                            else if (x2 == y)
                                then do
                                    let newItem = makeSetextHeader (args !! 0) 1
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                    if (x3 == y)
                                        then do
                                            let newItem = makeSetextHeader (args !! 0) 1
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 1)
                        let (checkItem, _) = splitAt index (args !! 1)
                        if (length x > index-1 && length newList > index)
                            then do 
                                let (temp, _) = splitAt index x
                                let itemCheck = unmakeSetextHeader temp 
                                if (checkItem == itemCheck)
                                    then do
                                        let (xs, ys) = splitAt (indexCount+1) newList
                                        let newItem = makeSetextHeader (args !! 0) 1
                                        let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                        (finalList, addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (checkItem == x4)
                                            then do 
                                                 let (xs, ys) = splitAt (indexCount+1) newList
                                                 let newItem = makeSetextHeader (args !! 0) 1
                                                 let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                                 (finalList, addCount+1)
                                            else do
                                                 (newList, addCount)
                            else do
                                (newList, addCount)

            else do 
                ([], 0)

add_header args oldList newList indexCount addCount =
    do
        if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 1) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) 
                    then do
                        let x1 = oldList !! 0 
                        let x2 = unmakeSetextHeader x1
                        let y = (args !! 1)
                        if (length newList > indexCount+1) 
                            then if (x2 == y)
                                then do
                                    let newItem = makeSetextHeader (args !! 0) 1
                                    let (xs, ys) = splitAt (indexCount+1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let newItem = makeSetextHeader (args !! 0) 1
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            add_header args (tail oldList) newList (indexCount+1) addCount
                            else if (x2 == y)
                                then do
                                    let newItem = makeSetextHeader (args !! 0) 1
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let newItem = makeSetextHeader (args !! 0) 1
                                            let (xs, ys) = splitAt ((length newList)-1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 1)
                        let (checkItem, _) = splitAt index (args !! 1)
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, _) = splitAt index x
                                let itemCheck = unmakeSetextHeader temp 
                                if (checkItem == itemCheck)
                                    then do
                                        let (xs, ys) = splitAt (indexCount+1) newList
                                        let newItem = makeSetextHeader (args !! 0) 1
                                        let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                        add_header args (tail oldList) finalList (indexCount+2) (addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == checkItem)
                                            then do
                                                let (xs, ys) = splitAt (indexCount+1) newList
                                                let newItem = makeSetextHeader (args !! 0) 1
                                                let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                                add_header args (tail oldList) finalList (indexCount+2) (addCount+1)
                                            else do
                                                add_header args (tail oldList) newList (indexCount+1) addCount
                                        
                            else do
                                add_header args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)


add_paragraph :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
add_paragraph args [x] newList indexCount addCount =
    do
        if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 1) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if they are not, then we do exact string matching
                    then do
                        let x2 = unmakeSetextHeader x 
                        let y = (args !! 1)
                        if (length newList > indexCount+1) 
                            then if (x2 == y)
                                then do
                                    let newItem = args !! 0
                                    let (xs, ys) = splitAt (indexCount+1) newList 
                                    let finalList = xs ++ [newItem] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                            else if (x2 == y)
                                then do
                                    let newItem = args !! 0
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 1)
                        let (checkItem, _) = splitAt index (args !! 1)
                        if (length x > index-1 && length newList > index)
                            then do 
                                let (temp, _) = splitAt index x
                                let itemCheck = unmakeSetextHeader temp 
                                if (checkItem == itemCheck)
                                    then do
                                        let (xs, ys) = splitAt (indexCount+1) newList
                                        let newItem = args !! 0
                                        let finalList = xs ++ [newItem] ++ ys
                                        (finalList, addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == checkItem)
                                            then do
                                                let (xs, ys) = splitAt (indexCount+1) newList
                                                let newItem = args !! 0
                                                let finalList = xs ++ [newItem] ++ ys
                                                (finalList, (addCount+1))
                                            else do
                                                (newList, addCount)
                            else do
                                (newList, addCount)
            else do
                ([], 0)

add_paragraph args oldList newList indexCount addCount = 
    do
         if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 1) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) 
                    then do
                        let x1 = oldList !! 0 
                        let x2 = unmakeSetextHeader x1
                        let y = (args !! 1)
                        if (length newList > indexCount+1) 
                            then if (x2 == y)
                                then do
                                    let newItem = args !! 0
                                    let (xs, ys) = splitAt (indexCount+1) newList 
                                    let finalList = xs ++ [newItem] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            add_paragraph args (tail oldList) newList (indexCount+1) addCount
                            else if (x2 == y)
                                then do
                                    let newItem = args !! 0
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt ((length newList)-1) newList 
                                            let finalList = xs ++ [newItem] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 1)
                        let (checkItem, _) = splitAt index (args !! 1)
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, _) = splitAt index x
                                let itemCheck = unmakeSetextHeader temp 
                                if (checkItem == itemCheck)
                                    then do
                                        let (xs, ys) = splitAt (indexCount+1) newList
                                        let newItem = args !! 0
                                        let finalList = xs ++ [newItem] ++ ys
                                        add_paragraph args (tail oldList) finalList (indexCount+2) (addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == checkItem)
                                            then do
                                                let (xs, ys) = splitAt (indexCount+1) newList
                                                let newItem = args !! 0
                                                let finalList = xs ++ [newItem] ++ ys
                                                add_paragraph args (tail oldList) finalList (indexCount+2) (addCount+1)
                                            else do
                                                add_paragraph args (tail oldList) newList (indexCount+1) addCount
                                        
                            else do
                                add_paragraph args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)

