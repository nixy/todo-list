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
                            print newList
                        else do
                            print "Could not find location to add to - try append/prepend if you do not know the structure of the list"
                else do
                    putStrLn "Add later"

-- add_header - recursive function that checks to see if the location that the user has given to add to is valid and adds a new header there
-- if the user has indicated wildcard location matching, it will add the header after every instance of that string
-- if the user has indicated exact string matching, it will add the header after the FIRST header it comes across which matches that string
-- takes in the user arguments, the list to search through, the list it will return, an indexCount (where in the list we found the location) 
-- and addCount (the number of times the item was added to the list)
add_header :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
-- if we have depleted the entire list, we return newlist and its addcount
add_header args [] newList indexCount addCount = 
    do
        (newList, addCount)
-- if there is one item left in the list, we check if the user is doing wildcard matching or not
-- We see if the last location in the list is the location to which to add the header 
-- We check if it is a setext header or an ATX header - if it is, we add the item after the header in the list
-- if not, then we return the newlist without anything added to it, and the addCount
add_header args [x] newList indexCount addCount = 
    do
         if (length args > 1)
            then do
                let potentialWildCard = elemIndex '*' (args !! 1) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if they are not, then we do exact string matching
                    then do
                        let x2 = unmakeSetextHeader x 
                        let y = (args !! 1)
                        -- if statement is for preventing empty lists if indexCount > length newList 
                        if (length newList > indexCount+1) 
                            -- checks if it is a setext header 
                            then if (x2 == y)
                                then do
                                    -- if so, we add to the list 
                                    let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                    let (xs, ys) = splitAt (indexCount+1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                    -- checks if it is an ATX header 
                                    if (x3 == y)
                                        then do 
                                            -- if so, we add to the list 
                                            let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                            -- otherwise, we do not add it to the list 
                                            (newList, addCount)
                              -- checks if it is a setext header    
                            else if (x2 == y)
                                then do
                                    -- if so, we add to the list
                                    let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                     -- checks if it is an ATX header 
                                    if (x3 == y)
                                        then do
                                            -- if so, we add to the list
                                            let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1))
                                        else do
                                             -- otherwise, we do not add it to the list 
                                            (newList, addCount)
                    else do
                        -- wildcard matching 
                        -- strips the location header up to the point of the wildcard symbol and compares the user argument vs. the one in the list 
                        -- removes the wildcard symbol from the argument before comparison 
                        let (Just index) = elemIndex '*' (args !! 1)
                        let (checkItem, _) = splitAt index (args !! 1)
                        if (length x > index-1 && length newList > index)
                            then do 
                                let (temp, _) = splitAt index x
                                let itemCheck = unmakeSetextHeader temp 
                                -- setext header 
                                if (checkItem == itemCheck)
                                    then do
                                        let (xs, ys) = splitAt (indexCount+1) newList
                                        let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                        let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                        (finalList, addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        -- atx header 
                                        if (checkItem == x4)
                                            then do 
                                                 let (xs, ys) = splitAt (indexCount+1) newList
                                                 let newItem = makeSetextHeader (args !! 0) 1  -- makes the user item a header
                                                 let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                                 (finalList, addCount+1)
                                            else do
                                                 (newList, addCount)
                            else do
                                (newList, addCount)

            else do 
                ([], 0)
-- Functions the same as with one element left
-- We see if the last location in the list is the location to which to add the header 
-- We check if it is a setext header or an ATX header - if it is, we add the item after the header in the list
-- if not, then we call add_header again with the tail of the list
-- if we find the exact string prior to the last element in the list, we stop recursing 
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
                        if (length newList > indexCount+1) -- check for preventing empty list if indexCount > length newList 
                            then if (x2 == y) -- setext header 
                                then do
                                    let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                    let (xs, ys) = splitAt (indexCount+1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                    (finalList, (addCount+1)) -- if we found the exact string, we no longer call the function 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- atx header 
                                        then do
                                            let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1)) -- if we found the exact string, we no longer call the function 
                                        else do
                                            add_header args (tail oldList) newList (indexCount+1) addCount -- otherwise, we call the function again 
                            else if (x2 == y) -- checks setext header 
                                then do
                                    let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem] ++ ys
                                    (finalList, (addCount+1)) -- if we found the exact string, we no longer call the function 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- atx header
                                        then do
                                            let newItem = makeSetextHeader (args !! 0) 1 -- makes the user item a header
                                            let (xs, ys) = splitAt ((length newList)-1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                            (finalList, (addCount+1)) -- if we found the exact string, we no longer call the function 
                                        else do
                                            (newList, addCount) -- if the indexCount > length newList, we do not want to continue calling the function. The search ends. 
                    else do -- wildcard checking 
                        -- if we found an location which matches, we add the new item and continue to recurse 
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
                                        let newItem = makeSetextHeader (args !! 0) 1  -- makes the user item a header
                                        let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                        add_header args (tail oldList) finalList (indexCount+2) (addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == checkItem)
                                            then do
                                                let (xs, ys) = splitAt (indexCount+1) newList
                                                let newItem = makeSetextHeader (args !! 0) 1  -- makes the user item a header
                                                let finalList = xs ++ [newItem ++ "\n"] ++ ys
                                                add_header args (tail oldList) finalList (indexCount+2) (addCount+1)
                                            else do
                                                add_header args (tail oldList) newList (indexCount+1) addCount
                                        
                            else do
                                add_header args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)


-- add_paragraph - recursive function to add a paragraph after a given header 
-- functions the same as add_header, except we do not convert the user argument to a header before writing it to the list. 
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
                            then if (x2 == y) -- setext header 
                                then do
                                    let newItem = args !! 0
                                    let (xs, ys) = splitAt (indexCount+1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                            (finalList, (addCount+1))
                                        else do
                                            (newList, addCount)
                            else if (x2 == y)
                                then do
                                    let newItem = args !! 0
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
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
                                        let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                        (finalList, addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == checkItem)
                                            then do
                                                let (xs, ys) = splitAt (indexCount+1) newList
                                                let newItem = args !! 0
                                                let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
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
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt (indexCount+1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                            (finalList, (addCount+1))
                                        else do
                                            add_paragraph args (tail oldList) newList (indexCount+1) addCount
                            else if (x2 == y)
                                then do
                                    let newItem = args !! 0
                                    let (xs, ys) = splitAt ((length newList)-1) newList 
                                    let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                    (finalList, (addCount+1))
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y)
                                        then do
                                            let newItem = args !! 0
                                            let (xs, ys) = splitAt ((length newList)-1) newList 
                                            let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
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
                                        let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                        add_paragraph args (tail oldList) finalList (indexCount+2) (addCount+1)
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == checkItem)
                                            then do
                                                let (xs, ys) = splitAt (indexCount+1) newList
                                                let newItem = args !! 0
                                                let finalList = xs ++ [newItem ++ "\n"] ++ ys -- appends plain item 
                                                add_paragraph args (tail oldList) finalList (indexCount+2) (addCount+1)
                                            else do
                                                add_paragraph args (tail oldList) newList (indexCount+1) addCount
                                        
                            else do
                                add_paragraph args (tail oldList) newList (indexCount+1) addCount

            else do 
                ([], 0)

