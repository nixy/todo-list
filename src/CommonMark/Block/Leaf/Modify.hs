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


-- modify function: calls the appropriate function based on whether the user has indicated header or paragraph
-- if the item(s) exists to modify, then it writes the new list to the file the user indicated
-- otherwise, it displays a message 
modify :: Char -> [String] -> [String] -> String -> IO()
modify typeOf args todoList fileName =
    do
        -- if it is a header, then we call the modify_header function 
        if (typeOf == 'h')
            then do
                let (newList, addCount) = modify_header args todoList todoList 0 0
                if (addCount > 0)
                    then do
                        write_file newList fileName 0
                        print "Successfully wrote to file"
                    else do
                        print "Could not find item to modify"
            -- if it is a paragraph, then we call the modify_paragraph function 
            else if (typeOf == 'p')
                then do
                    let (newList, addCount) = modify_paragraph args todoList todoList 0 0
                    if (addCount > 0)
                        then do
                            write_file newList fileName 0
                            print "Successfully wrote to file "
                        else do
                            print "Could not find item to modify"
                else do
                    print "Add later"

-- modify_header
-- Recursive function for modifying a header in the file 
-- takes the header to remove, the oldList to iterate through, the newList to send back,
-- the indexcount (where the item was in the oldList) and the addCount (how many items were modified)
-- base case (only one item left in the list to check)
modify_header :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
modify_header args [x] newList indexCount addCount = 
    do 
         if (length args > 1) -- if there is no item to remove, we cannot go any further
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing)   -- if nothing, then exact string matching     
                    then do
                        let x1 = unmakeSetextHeader x
                        if ((args !! 0) == x1) -- checks if setext header 
                            then do
                                let (xs, ys) = splitAt (indexCount) newList 
                                let modified = makeSetextHeader (args !! 1) 1 -- makes item a header and replaces the old element 
                                let finalList = xs ++ [modified] ++ (tail ys)
                                (finalList, (addCount+1)) -- returns the modified list 
                            else do
                                let x2 = unmakeATXHeader x
                                if ((args !! 0) == x2) -- checks if ATX header 
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = makeSetextHeader (args !! 1) 1 -- makes item a header and replacces the old element 
                                        let finalList = xs ++ [modified] ++ (tail ys)
                                        (finalList, (addCount+1)) -- returns the modified list 
                                    else do
                                        (newList, addCount) -- else, it returns the old list 
                    else do -- else, it is wildcard matching 
                        let (Just index) = elemIndex '*' (args !! 0)
                        let checkItem = unmakeSetextHeader x
                        let (itemCheck, _) = splitAt index (args !! 0)
                        if (length x >= index)
                            then do
                                let (removeItem, rest) = splitAt index checkItem -- because it's wildcard, we want to store the rest of the string to re-add
                                if (itemCheck == removeItem) -- if setext header
                                    then do
                                        let modified = (args !! 1) ++ rest -- splices together user item and remaining half of old list item 
                                        let header = makeSetextHeader modified 1
                                        let (xs, ys) = splitAt (indexCount) newList 
                                        let finalList = xs ++ [header] ++ (tail ys)
                                        (finalList, (addCount+1)) -- returns the modified list 
                                    else do
                                        let checkItem2 = unmakeATXHeader x
                                        let (removeItem2, rest2) = splitAt index checkItem
                                        if (itemCheck == removeItem2) -- checks if atx header 
                                            then do
                                                let modified = (args !! 1) ++ rest2 -- splices together user item and remaining half of old list item 
                                                let header = makeSetextHeader modified 1
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let finalList = xs ++ [header] ++ (tail ys)
                                                (finalList, (addCount+1)) -- returns the modified list 
                                            else do
                                                (newList, addCount) -- else, returns the old list 
                            else do
                                (newList, addCount) -- else, returns the old list 
            else do
                ([], 0)

-- modify_header
-- Recursive function for modifying a header in the file 
-- takes the header to remove, the oldList to iterate through, the newList to send back,
-- the indexcount (where the item was in the oldList) and the addCount (how many items were modified)
modify_header args oldList newList indexCount addCount = 
    do
        if (length args > 1) -- if there is no item to remove, we cannot go any further
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if nothing, then exact string matching 
                    then do
                        let x1 = oldList !! 0 
                        let x2 = unmakeSetextHeader x1
                        let y = (args !! 0)
                        if (length newList > indexCount+1) 
                            then if (x2 == y) -- if setext header 
                                then do
                                    let (xs, ys) = splitAt (indexCount) newList 
                                    let modified = makeSetextHeader (args !! 1) 1
                                    let finalList = xs ++ [modified] ++ tail ys
                                    (finalList, (addCount+1)) -- we add the modified item to the list and return it 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- if atx header 
                                        then do
                                            let (xs, ys) = splitAt (indexCount) newList 
                                            let modified = makeSetextHeader (args !! 1) 1
                                            let finalList = xs ++ [modified] ++ tail ys
                                            (finalList, (addCount+1)) -- we add the modified item to the list and return it 
                                        else do
                                            modify_header args (tail oldList) newList (indexCount+1) addCount -- else, we continue recursing 
                            else if (x2 == y) -- if setext header 
                                then do
                                    let modified = makeSetextHeader (args !! 1) 1
                                    let finalList = init newList ++ [modified]
                                    (finalList, (addCount+1)) -- we add the modified item to the list and return it 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- if atxheader 
                                        then do 
                                            let modified = makeSetextHeader (args !! 1) 1
                                            let finalList = init newList ++ [modified]
                                            (finalList, (addCount+1)) -- we add the modified item to the list and return it 
                                        else do 
                                            (newList, addCount) -- else, we return the old list 
                    else do -- else, they are doing wildcard matching 
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0)
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, rest) = splitAt index x -- because it's wildcard, we want to store the rest of the string to re-add
                                if (checkItem == temp) -- if setext heading 
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = (args !! 1) ++ rest  -- splices together user item and remaining half of old list item 
                                        let header = makeSetextHeader modified 1
                                        let finalList = xs ++ [header] ++ (tail ys)
                                        modify_header args (tail oldList) finalList (indexCount+1) (addCount+1) -- modifies the list and continues recursing 
                                    else do
                                        let x2 = unmakeATXHeader x
                                        let (temp2, rest2) = splitAt index x2 -- splices together user item and remaining half of old list item
                                        if (checkItem == temp2) -- if atx header 
                                            then do
                                                let (xs, ys) = splitAt (indexCount) newList 
                                                let modified = (args !! 1) ++ rest2 -- splices together user item and remaining half of old list item 
                                                let header = makeSetextHeader modified 1
                                                let finalList = xs ++ [header] ++ (tail ys)
                                                modify_header args (tail oldList) finalList (indexCount+1) (addCount+1) -- modifies the list and continues recursing 
                                            else do
                                                modify_header args (tail oldList) newList (indexCount+1) addCount -- else, continues recursing 
                            else do
                                modify_header args (tail oldList) newList (indexCount+1) addCount -- else, continues recursing 

            else do 
                ([], 0)

-- modify_paragraph 
-- Recursive function for modifying a paragraph in the file 
-- takes the paragraph to remove, the oldList to iterate through, the newList to send back,
-- the indexcount (where the item was in the oldList) and the addCount (how many items were modified)
-- base case (only one item left in the list to check)
modify_paragraph :: [String] -> [String] -> [String] -> Int -> Int -> ([String], Int)
modify_paragraph args [x] newList indexCount addCount = 
    do 
         if (length args > 1) -- if there is no item to remove, we cannot go any further
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if nothing, then exact string matching 
                    then do
                        let x1 = unmakeSetextHeader x
                        if ((args !! 0) == x1) -- if setext header 
                            then do
                                let (xs, ys) = splitAt (indexCount) newList
                                let modified = args !! 1
                                let finalList = xs ++ [modified] ++ (tail ys) -- we add the new item at the position of the list 
                                (finalList, (addCount+1)) -- returns modified list 
                            else do
                                let x2 = unmakeATXHeader x
                                if ((args !! 0) == x2) -- if atx header 
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = args !! 1
                                        let finalList = xs ++ [modified] ++ (tail ys) -- we add the new item at the position of the list 
                                        (finalList, (addCount+1)) -- returns modified list 
                                    else do
                                        (newList, addCount) -- returns unmodified list 
                    else do -- else, we are doing wildcard matching 
                        let (Just index) = elemIndex '*' (args !! 0)
                        let checkItem = unmakeSetextHeader x
                        let (itemCheck, _) = splitAt index (args !! 0) -- strips the wildcard symbol from the user item 
                        if (length x >= index)
                            then do
                                let (removeItem, rest) = splitAt index checkItem -- because it's wildcard, we want to store the rest of the string to re-add 
                                if (itemCheck == removeItem)
                                    then do
                                        let modified = (args !! 1) ++ rest  -- splices together user item and remaining half of old list item 
                                        let (xs, ys) = splitAt (indexCount) newList 
                                        let finalList = xs ++ [modified] ++ (tail ys) -- adds modified item to new list 
                                        (finalList, (addCount+1)) -- returns modified list  
                                    else do
                                        let checkItem2 = unmakeATXHeader x
                                        let (removeItem2, rest2) = splitAt index checkItem2 -- because it's wildcard, we want to store the rest of the string to re-add 
                                        if (itemCheck == removeItem2) -- if atx header 
                                            then do
                                                let modified = (args !! 1) ++ rest2 -- splices together user item and remaining half of old list item 
                                                let (xs, ys) = splitAt (indexCount) newList
                                                let finalList = xs ++ [modified] ++ (tail ys)  -- adds modified item to new list 
                                                (finalList, (addCount+1)) -- returns modified list 
                                            else do
                                                (newList, addCount) -- else, returns old list 
                            else do
                                (newList, addCount) -- else, returns old list 
            else do
                ([], 0)

-- modify_paragraph 
-- Recursive function for modifying a paragraph in the file 
-- takes the paragraph to remove, the oldList to iterate through, the newList to send back,
-- the indexcount (where the item was in the oldList) and the addCount (how many items were modified)
modify_paragraph args oldList newList indexCount addCount = 
    do
        if (length args > 1) -- if there is no item to remove, we cannot go any further
            then do
                let potentialWildCard = elemIndex '*' (args !! 0) -- Checks to see if the user is doing wildcard matching
                if (potentialWildCard == Nothing) -- if nothing, then exact string matching 
                    then do
                        let x1 = oldList !! 0 
                        let x2 = unmakeSetextHeader x1
                        let y = (args !! 0)
                        if (length newList > indexCount+1) 
                            then if (x2 == y) -- if atx header 
                                then do
                                    let (xs, ys) = splitAt (indexCount) newList 
                                    let modified = args !! 1
                                    let finalList = xs ++ [modified] ++ tail ys -- we add the modified item to the list 
                                    (finalList, (addCount+1)) -- returns modified list 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- if atx header 
                                        then do
                                            let (xs, ys) = splitAt (indexCount) newList 
                                            let modified = args !! 1
                                            let finalList = xs ++ [modified] ++ tail ys -- we add the modified item to the list 
                                            (finalList, (addCount+1)) -- returns modified list 
                                        else do
                                            modify_paragraph args (tail oldList) newList (indexCount+1) addCount -- else, we continue recursing 
                            else if (x2 == y) -- if setext header 
                                then do
                                    let modified = args !! 1
                                    let finalList = init newList ++ [modified] -- we add the modified item to the list 
                                    (finalList, (addCount+1)) -- returns modified list 
                                else do
                                    let x3 = unmakeATXHeader x1
                                    if (x3 == y) -- we add the modified item to the list
                                        then do 
                                            let modified = args !! 1
                                            let finalList = init newList ++ [modified] -- we add the modified item to the list 
                                            (finalList, (addCount+1)) -- returns modified list 
                                        else do 
                                            modify_paragraph args (tail oldList) newList (indexCount+1) addCount -- else, we continue recursing 
                    else do -- else, we are doing wildcard matching 
                        let (Just index) = elemIndex '*' (args !! 0)
                        let (checkItem, _) = splitAt index (args !! 0) -- strips the wildcard symbol from the user item 
                        let x = oldList !! 0 
                        if (length x > index-1 && length newList > indexCount)
                            then do 
                                let (temp, rest) = splitAt index x -- because it's wildcard, we want to store the rest of the string to re-add 
                                if (checkItem == temp) -- if setext header 
                                    then do
                                        let (xs, ys) = splitAt (indexCount) newList
                                        let modified = (args !! 1) ++ rest  -- splices together user item and remaining half of old list item 
                                        let header = modified
                                        let finalList = xs ++ [modified] ++ (tail ys) -- adds modified item to list 
                                        modify_paragraph args (tail oldList) finalList (indexCount+1) (addCount+1) -- updates list and continues recursing 
                                    else do
                                        let x2 = unmakeATXHeader x
                                        let (temp2, rest2) = splitAt index x2  -- because it's wildcard, we want to store the rest of the string to re-add 
                                        if (checkItem == temp2) -- if setext header 
                                            then do
                                                let (xs, ys) = splitAt (indexCount) newList 
                                                let modified = (args !! 1) ++ rest2 -- splices together user item and remaining half of old list item 
                                                let header = modified
                                                let finalList = xs ++ [modified] ++ (tail ys) -- adds modified item to list 
                                                modify_paragraph args (tail oldList) finalList (indexCount+1) (addCount+1) -- updates list and continues recursing
                                            else do
                                                modify_paragraph args (tail oldList) newList (indexCount+1) addCount -- else, continues recursing 
                            else do
                                modify_paragraph args (tail oldList) newList (indexCount+1) addCount -- else, continues recursing 

            else do 
                ([], 0)