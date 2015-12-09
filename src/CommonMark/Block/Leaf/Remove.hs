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

-- remove function: calls the appropriate function based on whether the user has indicated header or paragraph
-- if the item(s) exists to remove, then it writes the new list to the file the user indicated
-- otherwise, it displays a message 
remove :: Char -> [String] -> [String] -> String -> IO()
remove typeOf args todoList fileName =
    do
        -- if it is a header, then we call the remove_header function 
        if (typeOf == 'h')
            then do
                let (newList, addCount) = remove_header args todoList todoList 0 0
                -- if addCount > 0 (at least one item was added) then we write the new list to the file 
                if (addCount > 0)
                    then do
                        write_file newList fileName 0
                        print "Successfully wrote to file"
                    -- else, we tell the user that we could not find the item 
                    else do
                        print "Could not find item to remove"
            -- if it is a paragraph, then we call the remove_parapgraph function 
            else if (typeOf == 'p')
                then do 
                    let (newList, addCount) = remove_paragraph args todoList todoList 0 0
                    -- if addCount > 0 (at least one item was added) then we write the new list to the file 
                    if (addCount > 0)
                    then do
                        write_file newList fileName 0
                        print "Successfully wrote to file"
                    -- else, we tell the user that we could not find the item
                    else do
                        print "Could not find item to remove"
                else do
                    print "Other features not yet implemented"

-- remove_header: recursive function which takes the header to remove, the oldList to iterate through, the newList to send back,
-- the indexcount (where the item was in the oldList) and the addCount (how many items were removed)
-- base case (only one item left in the list to check)
remove_header :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
remove_header args [x] newList indexCount addCount = 
    do
        -- if there is no item to remove, we cannot go any further
        if (length args > 0)
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if nothing, then exact string matching      
                    then do
                        let x2 = unmakeSetextHeader x
                        if ((args !! 0) == x2) -- checks if setext header 
                            then do
                                let finalList = init newList 
                                (finalList, (addCount+1)) -- if so, returns modified list
                            else do
                                let x3 = unmakeATXHeader x
                                if ((args !! 0) == x3) -- checks if ATX header
                                    then do
                                        let finalList = init newList
                                        (finalList, (addCount+1)) -- if so, returns modified list
                                    else do
                                        (newList, addCount) -- else, returns unmodified list 
                    else do -- else, wildcard matching 
                        let (Just index) = elemIndex '*' (args !! 0)
                        let checkItem = unmakeSetextHeader x
                        let (itemCheck, _) = splitAt index (args !! 0) -- we strip away the wildcard symbol '*'
                        if (length x >= index) -- case for preventing empty list operations 
                            then do
                                let (removeItem, _) = splitAt index checkItem -- we strip away the characters after the index of the symbol
                                if (itemCheck == removeItem) -- if setext header 
                                    then do
                                        let finalList = init newList 
                                        (finalList, (addCount+1)) -- if so, returns modified list 
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (x4 == itemCheck) -- if ATX header 
                                            then do
                                                let finalList = init newList 
                                                (finalList, (addCount+1)) -- if so, returns modified list 
                                            else do
                                                (newList, addCount) -- else, returns unmodified list 
                            else do
                                (newList, addCount) -- else, returns unmodified list 
            else do
                ([], 0)

-- remove_header: recursive function which takes the header to remove, the oldList to iterate through, the newList to send back,
-- the indexcount (where the item was in the oldList) and the addCount (how many items were removed)
remove_header args oldList newList indexCount addCount = 
    do
        if (length args > 0) -- if there is no item to remove, we cannot go any further
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if not, they are doing exact string matching 
                    then do
                        let x1 = oldList !! 0 
                        let x2 = unmakeSetextHeader x1
                        let y = (args !! 0)
                        if (length newList > indexCount+1) -- check for preventing operations on empty list 
                            then if (x2 == y) -- if setext header 
                                then do
                                    let (xs, ys) = splitAt (indexCount) newList 
                                    let finalList = xs ++ (tail ys)
                                    (finalList, (addCount+1)) -- if so, returns modified list 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- if ATX header
                                        then do
                                            let (xs, ys) = splitAt (indexCount) newList 
                                            let finalList = xs ++ (tail ys)
                                            (finalList, (addCount+1)) -- if so, returns modified list 
                                        else do
                                            remove_header args (tail oldList) newList (indexCount+1) addCount
                            else if (x2 == y) -- if setext header 
                                then do
                                    let finalList = init newList
                                    (finalList, (addCount+1)) -- if so, returns modified list 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- if ATX header
                                        then do
                                            let finalList = init newList
                                            (finalList, (addCount+1)) -- if so, returns modified list 
                                        else do
                                            (newList, addCount) -- else returns unmodified list 
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0) -- strips wildcard symbol from argument 
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount) -- prevents operations on empty lists
                            then do 
                                let (temp, _) = splitAt index x -- strips characters after wildcard symbols 
                                if (checkItem == temp) -- if setext header 
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let finalList = xs ++  (tail ys)
                                        remove_header args (tail oldList) finalList (indexCount) (addCount+1) -- we remove from the list and we continue recursing 
                                    else do
                                        let x3 = unmakeATXHeader x
                                        let (x4, _) = splitAt index x3
                                        if (checkItem == x4) -- if ATX header 
                                            then do
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let finalList = xs ++  (tail ys)
                                                remove_header args (tail oldList) finalList (indexCount) (addCount+1) -- we remove from the list and we continue recursing 
                                            else do
                                                remove_header args (tail oldList) newList (indexCount+1) addCount -- else, we continue recursing 
                            else do
                                remove_header args (tail oldList) newList (indexCount+1) addCount -- else, we continue recursing 

            else do 
                ([], 0)

-- remove_paragraph removes a paragraph in the list 
-- the indexcount (where the item was in the oldList) and the addCount (how many items were removed)
-- base case (only one item left in the list to check)
-- base case (old list has 1 element)
remove_paragraph :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
remove_paragraph args [x] newList indexCount addCount = 
    do
        if (length args > 0) -- if there is no item to remove, we cannot go any further
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if they are not, we remove an exact string     
                    then do
                        if ((args !! 0) == x) -- if the items are the same, then we remove it from the list 
                            then do
                                let finalList = init newList 
                                (finalList, (addCount+1))
                            else do -- else, we return the old list 
                                (newList, addCount)
                    else do
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (itemCheck, _) = splitAt index (args !! 0) -- strips wildcard symbol from argument 
                        if (length x >= index)
                            then do
                                let (removeItem, _) = splitAt index x -- strips characters after wildcard from list element 
                                if (itemCheck == removeItem)
                                    then do
                                        let finalList = init newList 
                                        (finalList, (addCount+1)) -- removes item from list 
                                    else do
                                        (newList, addCount) -- else, returns old list 
                            else do
                                (newList, addCount) -- else, returns old list 
            else do
                ([], 0)

-- remove_paragraph removes a paragraph in the list 
-- the indexcount (where the item was in the oldList) and the addCount (how many items were removed)
-- base case (only one item left in the list to check)
remove_paragraph args oldList newList indexCount addCount = 
    do
        if (length args > 0) -- if there is no item to remove, we cannot go any further
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if they are not, we remove an exact string
                    then do
                        let x1 = oldList !! 0 
                        let y = (args !! 0)
                        if (length newList > indexCount+1) 
                            then if (x1 == y) -- if the strings are the same, then we remove it from the list 
                                then do
                                    let (xs, ys) = splitAt (indexCount) newList 
                                    let finalList = xs ++ (tail ys)
                                    (finalList, (addCount+1)) -- returns modified list 
                                else do
                                    remove_header args (tail oldList) newList (indexCount+1) addCount -- else, we call the function again 
                            else if (x1 == y)
                                then do
                                    let finalList = init newList
                                    (finalList, (addCount+1)) -- if the strings are the same, then we remove it from the list 
                                else do
                                    (newList, addCount) -- else, we return the old list 
                    else do -- else, they are doing wildcard matching 
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0) -- sttrips the wildcard symbol from the argument 
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, _) = splitAt index x -- strips the characters after the wildcard from the list item 
                                if (checkItem == temp) -- if they are the same, then we remove it from the list 
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let finalList = xs ++  (tail ys)
                                        remove_header args (tail oldList) finalList (indexCount) (addCount+1) -- modifies list and continues recursing 
                                    else do
                                        remove_header args (tail oldList) newList (indexCount+1) addCount -- else, continues recursing 
                            else do
                                remove_header args (tail oldList) newList (indexCount+1) addCount -- else, continues recursing 

            else do 
                ([], 0)
