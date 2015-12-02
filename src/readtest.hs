import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import Data.List (intercalate)
import Data.Char
import CreateNewTaskList
import Text.Pandoc 


dispatch :: [(String, [[String]] -> [String] -> IO ())]  
dispatch =  [ ("add_to_list", add_to_list) 
            , ("add_doable", add_doable)
            , ("add_doable_to_section", add_doable_to_section)
            , ("do_item", do_item)
            , ("undo_item", undo_item)
            , ("add_to_section", add_to_section)
            , ("rename_list", rename_list) 
            , ("view", view)  
            , ("delete_section", delete_section)
            , ("delete_item", delete_item)
            , ("delete_from_section", delete_from_section)  
            ]  
callModule = do putStrLn "You have successfully called this module!"
-- adds to the list the item specified in the arguments
-- the first item in the arguments should be the file name, and the second should be the item to append
-- other error checking is needed 
add_to_list :: [[String]] -> [String] -> IO()
add_to_list list args = 
     do   
        if length args > 1
            then 
                do 
                    let newList = list ++ [[""] ++ ["* " ++ (args !! 1)]]
                    print newList
                    write_file newList args   
            else 
                do
                    putStrLn "Incorrect number of arguments - no item to append."



-- adds to the list the item specified in the arguments
-- the first item in the arguments should be the file name, and the second should be the item to append
-- other error checking is needed 
add_doable :: [[String]] -> [String] -> IO()
add_doable list args = 
     do   
        if length args > 1
            then 
                do 
                    let newList = list ++ [[""] ++ ["* [ ] " ++ (args !! 1)]]
                    print newList
                    write_file newList args   
            else 
                do
                    putStrLn "Incorrect number of arguments - no item to append."

add_doable_to_section :: [[String]] -> [String] -> IO()
add_doable_to_section list args =
    do
        if length args > 2
             then 
                 do 
                     -- checks to see if there is a section in the list that matches the one in args 
                     -- if there is, it returns the list with the item written in the last position of the section list
                     -- if not, we'll return an empty list and print a message to the user that the section did not exist 
                     let newList = check_for_section list list args 0 "* [ ] "
                     if length newList > 1
                         then 
                             do 
                                 write_file newList args
                         else 
                             do
                                 putStrLn "That section does not exist."
             else
                 do 
                     putStrLn "Incorrect number of arguments - no item to append to the given section."



-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ADD_TO_section FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

add_to_section :: [[String]] -> [String] -> IO()
add_to_section list args = 
    do
        if length args > 2
             then 
                 do 
                     -- checks to see if there is a section in the list that matches the one in args 
                     -- if there is, it returns the list with the item written in the last position of the section list
                     -- if not, we'll return an empty list and print a message to the user that the section did not exist 
                     let newList = check_for_section list list args 0 "* "
                     if length newList > 1
                         then 
                             do 
                                 write_file newList args
                         else 
                             do
                                 putStrLn "That section does not exist."
             else
                 do 
                     putStrLn "Incorrect number of arguments - no item to append to the given section."


check_for_section :: [[String]] -> [[String]] -> [String] -> Int -> String -> [[String]]
check_for_section [x] oldList args count typeOfItem =
    do  
        if ((head x) == (args !! 1))
            then
                do
                    let newInnerList = x ++ [typeOfItem ++ args !! 2]
                    -- if the section is the first inner list that we come across 
                    -- (not actually a possible case, I'm pretty sure - the head of the list will always be the title of the file) 
                    if count == 0
                         then 
                            do
                                [newInnerList] ++ tail oldList
                         else 
                            do
                                if count == (length oldList)
                                    then
                                        do
                                            (init oldList) ++ [newInnerList]
                                    else
                                        do
                                            let (xs, ys) = splitAt count oldList
                                            xs ++ [newInnerList] ++ (tail ys)
            -- if the last inner list is not the section that we're looking for, it's not in the list at all
            -- we return a list with an empty string 
            else 
                do
                    [[""]]

check_for_section currentList oldList args count typeOfItem =
    do  
        let x = head currentList
        if ((head x) == (args !! 1))
            then
                do
                    -- newInnerList is the sublist + the item we want to append as the last element of the list
                    -- we can do stuff like "append" vs. "prepend" later 
                    let newInnerList = x ++ [typeOfItem ++ args !! 2]
                    -- if the section is the first inner list that we come across  
                    -- (not actually a possible case, I'm pretty sure - the head of the list will always be the title of the file) 
                    if count == 0
                         then 
                            do
                                [newInnerList] ++ tail oldList
                         else 
                            do
                                -- if the section is the last possible line in the list (this IS possible)
                                if count == (length oldList)
                                    then
                                        do
                                            (init oldList) ++ [newInnerList]
                                    -- else, it's somewhere in the middle of the list
                                    -- meaning we need to account for the first half, the new item, and the second half
                                    else
                                        do
                                            let (xs, ys) = splitAt count oldList
                                            xs ++ [newInnerList] ++ (tail ys)
            -- if the current inner list is not the section that we are looking for, then we call the function again with the tail
            else 
                do
                    check_for_section (tail currentList) oldList args (count+1) typeOfItem 
                    
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ END OF ADD_TO_section FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


create_list :: [String] -> IO()
create_list list = 
    print(list)

create_new_list :: IO()
create_new_list = do makeFile


rename_list :: [[String]] -> [String] -> IO()
rename_list list args = 
    do
        if (length args > 1)
            then
                do
                    let newList = [["# " ++ args !! 1]] ++ (tail list)
                    write_file newList args
            else
                putStrLn "Incorrect number of arguments - no new name given."

view :: [[String]] -> [String] -> IO()
view [x] args =
    do
        print(x)
view list args = 
    do
        print(head list)
        view (tail list) args


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ DELETE_ITEM FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
delete_item :: [[String]] -> [String] -> IO()
delete_item list args =
    do   
        if length args > 1
            then 
                do 
                    let newList = check_for_delete list list args 0 
                    if length newList > 0
                        then 
                            do
                                print newList
                                write_file newList args
                        else 
                            do
                                putStrLn "That item cannot be deleted - does not exist."
            else 
                do
                    putStrLn "Incorrect number of arguments - no item to delete."

-- This function assumes that items not in sections are in their own sublists consisting of ["", item]
-- We can access the item directly by doing list !! 1 and we don't have to recursively go through the sublist like we do for deleting an item from a section
check_for_delete :: [[String]] -> [[String]] -> [String] -> Int -> [[String]]
check_for_delete [x] oldList args count =
    do  
        if (length x == 2)
            then 
                do 
                    if ((x !! 1) == ("* " ++ args !! 1))
                        then
                            do
                                -- If the item to delete is the last item in the file/list, we can just pass back the old list sans the last value
                                init oldList
                        -- If the item to delete is not in the last element of our buffer list, then it's not there at all
                        -- We pass the function that called it an empty list 
                        else 
                            do 
                                 [[""]]
            else 
                 do

                     [[""]]
check_for_delete currentList oldList args count = 
    do
        let x = head currentList
        if (length x == 2)
            then 
                do 
                     if ((x !! 1) == ("* " ++ args !! 1))
                        then 
                            do
                                let (xs, ys) = splitAt count oldList
                                -- We pass back the old list sans the element that we wish to remove 
                                xs ++ (tail ys)
                        else 
                            do
                                check_for_delete (tail currentList) oldList args (count+1)
            else 
                do
                    check_for_delete (tail currentList) oldList args (count+1)

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ END OF DELETE_ITEM FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ DELETE_section FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
delete_section :: [[String]] -> [String] -> IO()
delete_section list args =
    if length args > 1
        then
            do
                let newList = delete_check_section list list args 0
                if length newList > 0
                    then 
                        do
                            write_file newList args
                    else 
                        do
                            putStrLn "section to delete not found"
        else 
            do
                putStrLn "Incorrect number of arguments - no section to delete"

delete_check_section :: [[String]] -> [[String]] -> [String] -> Int -> [[String]]
delete_check_section [x] oldList args count =
    do  
        if (length x > 0)
            then
                do 
                    if ((head x) == ("##" ++ args !! 1))
                        then
                            do
                                init oldList
                        else 
                            [[""]]
            else 
                [[""]]
delete_check_section currentList oldList args count = 
    do 
        let x = head currentList
        if (length x > 0)
            then 
                do 
                    if ((head x) == ("##" ++ args !! 1))
                        then 
                            do
                                let (xs, ys) = splitAt count oldList
                                xs ++ tail ys
                        else 
                            do
                                delete_check_section (tail currentList) oldList args (count+1)
            else 
                do
                    delete_check_section (tail currentList) oldList args (count+1)
-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ END OF DELETE_section FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ DELETE_FROM_section FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
delete_from_section :: [[String]] -> [String] -> IO()
delete_from_section list args =
     if length args > 2
        then
            do
                let newList = delete_check_item list list args 0 
                if length newList > 1
                    then 
                        do
                            write_file newList args
                    else 
                        do
                            putStrLn "Either item or section not found"
        else 
            do
                putStrLn "Incorrect number of arguments - no item to delete"

delete_check_item :: [[String]] -> [[String]] -> [String] -> Int -> [[String]]
delete_check_item [x] oldList args count =
    do
         if (length x > 0)
            then
                do 
                    -- If the parameter matches (it is the correct section), then we must check the items in the list to see if 
                    -- the item that the user specified to delete exists
                    if ((head x) == ("##" ++ args !! 1))
                        then
                            do
                                -- Returns either the new list with the item deleted or an empty list if it was not found  
                                let innerList = check_delete_inner x x args 0
                                if (length innerList > 0)
                                    then 
                                        do
                                            (init oldList) ++ [innerList]
                                    else 
                                        do
                                            [[""]]
                        else 
                            [[""]]
            else 
                [[""]]
delete_check_item currentList oldList args count = 
    do
        let x = head currentList 
        if (length x > 0)
            then 
                do 
                    if ((head x) == ("##" ++ args !! 1))
                        then 
                            do
                                let innerList = check_delete_inner x x args 0
                                if (length innerList > 1)
                                    then 
                                        do
                                            let (xs, ys) = splitAt count oldList 
                                            xs ++ [innerList] ++ tail ys
                                    else 
                                        do 
                                            [[""]]
                        else 
                            delete_check_item (tail currentList) oldList args (count+1)
            else 
                 delete_check_item (tail currentList) oldList args (count+1)

check_delete_inner :: [String] -> [String] -> [String] -> Int -> [String] 
check_delete_inner [x] oldList args count =
    do
        -- if the item to delete matches up with the last one
        if (x == ("* " ++ args !! 2))
             then 
                do
                    -- we send back the oldList, sans the one to delete
                    init oldList
            -- else, we haven't found the item to delete - we send back an empty list 
             else
                do
                    []
check_delete_inner currentList oldList args count =
    do
        let x = head currentList
        if (x == ("* " ++ args !! 2))
            then
                do
                    let (xs, ys) = splitAt count oldList
                    xs ++ tail ys
            else
                do
                    check_delete_inner (tail currentList) oldList args (count+1)

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ END OF DELETE_FROM_section FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ DO_ITEM FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
do_item :: [[String]] -> [String] -> IO()
do_item list args = 
    do
        if length args > 1
        then
            do
                let newList = doable_check list list args 0 "do"
                if length newList > 1
                    then 
                         do
                             write_file newList args
                    else 
                        do
                            if (length newList > 0)
                                then 
                                    do
                                         putStrLn (head (head newList))
                                else 
                                     do
                                         putStrLn "There was an error with the item written"

        else 
            do
                putStrLn "Incorrect number of arguments - no item given to do"

doable_check :: [[String]] -> [[String]] -> [String] -> Int -> String -> [[String]]
doable_check [x] oldList args count typeOfItem = 
    do
        if (typeOfItem == "do") 
            then 
                do
                    if (length x > 1)
                        then 
                            do 
                                -- First, we check if the strings are the same and that the item is unchecked
                                if ((x !! 1) == ("* [ ] " ++ args !! 1))
                                    then
                                        do
                                            -- If they are, then we return the item checked off 
                                            init oldList ++ [[head x]] ++ [["* [x] " ++ args !! 1]]
                                    -- If they are equivalent, but it is already done, it cannot be done again 
                                    else if ((x !! 1) == ("* [x] " ++ args !! 1))
                                        then 
                                            do 
                                                [["Item checked off already"]]
                                        -- Else, if they are equivalent, but the item in the list is not a doable item 
                                        else if ((x !! 1) == args !! 1)
                                            then
                                                do
                                                    [["Item is not doable"]]
                                            else 
                                                do
                                                    [["Item does not exist"]]
                        else 
                            [["Item does not exist"]]
            else
                do
                    if (length x > 1)
                        then 
                            do
                                 -- First, we check if the strings are the same and that the item is checked
                                if ((x !! 1) == ("* [x] " ++ args !! 1))
                                    then
                                        do
                                            -- If they are, then we return the item unchecked
                                            init oldList ++ [[head x]] ++ [["* [ ] " ++ args !! 1]]
                                    -- If they are equivalent, but it is already done, it cannot be done again 
                                    else if ((x !! 1) == ("* [ ] " ++ args !! 1))
                                        then 
                                            do 
                                                [["Item already unchecked"]]
                                        -- Else, if they are equivalent, but the item in the list is not a doable item 
                                        else if ((x !! 1) == args !! 1)
                                            then
                                                do
                                                    [["Item is not doable"]]
                                            else 
                                                do
                                                    [["Item does not exist"]]
                        else 
                            [["Item does not exist"]]

doable_check currentList oldList args count typeOfItem = 
    do
        let x = head currentList
        if (typeOfItem == "do")
            then
                do
                    if (length x > 1)
                        then
                            do
                                -- First, we check if the strings are the same and that the item is unchecked
                                if ((x !! 1) == ("* [ ] " ++ args !! 1))
                                    then
                                        do
                                            -- If they are, then we return the item checked off 
                                            let innerList = [head x] ++ ["* [x] " ++ args !! 1]
                                            let (xs, ys) = splitAt count oldList 
                                            xs ++ [innerList] ++ tail ys
                                    -- If they are equivalent, but it is already done, it cannot be done again 
                                    else if ((x !! 1) == ("* [x] " ++ args !! 1))
                                        then 
                                            do 
                                                [["Item checked off already"]]
                                        -- Else, if they are equivalent, but the item in the list is not a doable item 
                                        else if ((x !! 1) == ("* " ++ args !! 1))
                                            then
                                                do
                                                    [["Item is not doable"]]
                                            else 
                                                do
                                                    doable_check (tail currentList) oldList args (count+1) typeOfItem
                        else 
                            do
                                doable_check (tail currentList) oldList args (count+1) typeOfItem
            else
                 do
                    if (length x > 1)
                        then
                            do
                                -- First, we check if the strings are the same and that the item is checked
                                if ((x !! 1) == ("* [x] " ++ args !! 1))
                                    then
                                        do
                                            -- If they are, then we return the item unchecked 
                                            let innerList = [head x] ++ ["* [ ] " ++ args !! 1]
                                            let (xs, ys) = splitAt count oldList 
                                            xs ++ [innerList] ++ tail ys
                                    -- If they are equivalent, but it is already done, it cannot be done again 
                                    else if ((x !! 1) == ("* [ ] " ++ args !! 1))
                                        then 
                                            do 
                                                [["Item already unchecked"]]
                                        -- Else, if they are equivalent, but the item in the list is not a doable item 
                                        else if ((x !! 1) == ("* " ++ args !! 1))
                                            then
                                                do
                                                    [["Item is not doable"]]
                                            else 
                                                do
                                                    doable_check (tail currentList) oldList args (count+1) typeOfItem
                        else 
                            do
                                doable_check (tail currentList) oldList args (count+1) typeOfItem

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ END DO ITEM FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ UNDO ITEM FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

undo_item :: [[String]] -> [String] -> IO()
undo_item list args = 
    do
        if length args > 1
        then
            do
                let newList = doable_check list list args 0 "undo"
                if length newList > 1
                    then 
                         do
                             write_file newList args
                    else 
                        do
                            if (length newList > 0)
                                then 
                                    do
                                         putStrLn (head (head newList))
                                else 
                                     do
                                         putStrLn "There was an error with the item written"

        else 
            do
                putStrLn "Incorrect number of arguments - no item given to undo"

-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ END UNDO ITEM FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-- Writes the given list to the file specified in args
-- "buffer" should be a list of lists, so we pass data to a function to write the inner elements of this list 
write_file :: [[String]] -> [String] -> IO()
write_file [x] args =
    do  
         write_inner_file x args
write_file buffer args =
    do 
         let x = head buffer
         write_inner_file x args
         write_file (tail buffer) args 

-- Writes individual lines to the file specified in args 
-- More logic is needed to put an extra space after the item if it is the last item of a section 
write_inner_file :: [String] -> [String] -> IO()
write_inner_file [x] args =
    do
        if (length x > 1)
            then 
                do
                    -- if it's the head (title) of the file
                    -- we'll probably want to change the "appendFile" in this to "writeFile" 
                    -- for testing, I have it as append, but we'll want it to start from the beginning if it sees the title 
                    if (x !! 0 == '#' && x !! 1 == '#')
                        then 
                            do
                                appendFile (head args) (x ++ "\n" ++ "\n")
                        else
                            do
                                -- if it's a section of the file
                                if (x !! 0 == '#' && x !! 1 /= '#')
                                    then
                                        do
                                            appendFile (head args) ("\n" ++ x ++ "\n")
                                    -- else, it's probably an item (section item or not)
                                    else
                                        do
                                            appendFile (head args) (x ++ "\n")
            -- else, it's probably an empty string - we write that to the file as a newline 
            else 
                do 
                     appendFile (head args) (x ++ "\n")

write_inner_file buffer args = 
    do
        let x = head buffer
        if (length x > 1)
            then 
                do
                    if (x !! 0 == '#' && x !! 1 /= '#')
                        then 
                            do
                                appendFile (head args) (x ++ "\n" ++ "\n")
                                write_inner_file (tail buffer) args
                        else
                            do
                                if (x !! 0 == '#' && x !! 1 == '#')
                                    then
                                        do
                                            appendFile (head args) ("\n" ++ x ++ "\n")
                                            write_inner_file (tail buffer) args
                                    else
                                        do
                                            appendFile (head args) (x ++ "\n")
                                            write_inner_file (tail buffer) args
            else 
                do 
                    appendFile (head args) (x ++ "\n")
                    write_inner_file (tail buffer) args


-- Returns a list with all of the lines from the XML file 
-- I'm just feeding it a file name for now
read_file :: String -> [String]
read_file fileName = 
    do
             let file = unsafePerformIO . readFile $ fileName
             lines file


-- Breaks down sections into their individual items
-- section heading is the first item in the list it returns  
item_breakdown :: [String] -> [String] -> Int -> ([String], Int)
item_breakdown [] elementList count = (elementList, count)
item_breakdown [x] elementList count =
     do 
        -- if the element is a new section header or a newline, then we know that it is not a section item 
         if (length x > 0)
            then 
                 do 
                      let a = head x
                      if (a == '#' || a == ' ')
                           then 
                                do
                                     (elementList, count)
                           -- otherwise, it is an item to be included in the section list 
                           else 
                                do
                                     let newCount = count + 1
                                     let newElementList = elementList ++ [x]
                                     (newElementList, newCount)
             else (elementList, count)


item_breakdown remaining elementList count =
    do
        if (length remaining > 0)
            then 
                do 
                     let h = head remaining
                     let t = tail remaining
                     if (length h > 0)
                        then 
                            do 
                                 let a = head h
                                 if a == '#'
                                     then (elementList, count)
                                     else
                                         do
                                             let newCount = count+1
                                             let newElementList = elementList ++ [h]
                                             let i = item_breakdown t newElementList newCount
                                             let (a, b) = i 
                                             (a, b)
                        else (elementList, count)
             else (elementList, count)

            
breakdown :: [String] -> [[String]] -> [[String]]
breakdown [] buffer = buffer 
breakdown [h] buffer = 
     do 
          buffer ++ [[""]++[h]]
          
breakdown fileData buffer = 
     do
        if (length fileData > 0)
             then 
                 do 
                      let h = head fileData
                      let t = tail fileData 
                      if (length h > 0)
                         then 
                            do 
                                let a = head h
                                if a == '#' -- if we encounter a sublist, we must extract all the elements that belong to the sublist
                                    then
                                        do 
                                             -- item_breakdown gets all of the elements belonging to the sublist and returns them 
                                             -- it also returns a count of how many items were in that list, so that we know how many to remove from t before recursing again 
                                             let (sublist, count) = item_breakdown t [h] 0 
                                             -- We don't want the part of the list with the old items, so we leave them out
                                             let (_, remaining) = splitAt count t
                                             -- newElement is the "section" that we're adding to the buffer. I
                                             -- It consists of the name of the section at list[0], and the items belonging to it from list[1]-[n]
                                             let newBuffer = buffer ++ [sublist]
                                             breakdown remaining newBuffer  
                                     else 
                                         if a == ' '
                                             then 
                                                  do
                                                       -- if it's just a blank line in the text, we don't add that as an element in the list and just move on 
                                                       breakdown t buffer 
                                             else 
                                                  do 
                                                       -- else, it's a section-less element 
                                                       -- we will probably want more elses in here for error-handling, but for now, I'm just assuming these 3 possibilities 
                                                       -- The empty string for the head of the element is just so we can get an extra space in there 
                                                       let newBuffer = buffer ++ [[""]++[h]]
                                                       breakdown t newBuffer 
                         else breakdown t buffer
             else buffer


main = do

     (command:args) <- getArgs 
     if (length args > 0)
         then 
             do 
                 -- if they're creating a new file/list, then we don't check to see if the file exists 
                 if (command == "create_new_list")
                     then 
                         do 
                             -- create_list args
                            currentDir <- getCurrentDirectory
                            let newFileName = args !! 0
                            let newFilePath = currentDir ++ "\\" ++ newFileName

                            if (length args > 1)
                                then 
                                    do
                                        let listName = "#" ++ args !! 1
                                        openFile newFilePath ReadWriteMode -- returns Handle not IO () which is problematic
                                        appendFile newFilePath listName
                                        putStr "Created new list file."
                                else 
                                    do
                                        let listName = "# Default List Name"
                                        openFile newFilePath ReadWriteMode -- returns Handle not IO () which is problematic
                                        appendFile newFilePath listName
                                        putStr "Created new list file."

                            --newFile <- openFile newFilePath ReadWriteMode -- returns Handle not IO () which is problematic
                            --writeFile newFile listName
                            --putStr "Created new list file."
                     else
                         do  
                             let (Just action) = lookup command dispatch
                             let fileName = head args
                             fileExists <- doesFileExist fileName  
                             if fileExists  
                                then 
                                    do 
                                        -- if the file exists, then we read it and pass the resulting list to the action that the user wanted
                                        -- each action takes in the buffer (the list) and the arguments (the name of the section, etc.)
                                        let buffer = read_file fileName
                                        let list = breakdown buffer []
                                        action list args
                                else 
                                    do 
                                        putStrLn "This file does not exist. Creating a new file."
                                        currentDir <- getCurrentDirectory
                                        let newFileName = "todoList.md"
                                        let newFilePath = currentDir ++ "\\" ++ newFileName
                                        openFile newFilePath ReadWriteMode -- returns Handle not IO () which is problematic
                                        putStr "Created new list file."
         else 
             do
                -- Create new list with default name, todoList.md
                currentDir <- getCurrentDirectory
                let newFileName = "todoList.md"
                let newFilePath = currentDir ++ "\\" ++ newFileName
                openFile newFilePath ReadWriteMode -- returns Handle not IO () which is problematic
                putStr "Created new list file."