import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import Data.List (intercalate)
import Data.Char


dispatch :: [(String, [[String]] -> [String] -> IO ())]  
dispatch =  [ ("add_to_list", add_to_list) 
            , ("add_to_subsection", add_to_subsection)
            , ("rename_list", rename_list) 
            , ("view", view)  
            , ("delete_subsection", delete_subsection)
            , ("delete_item", delete_item)
            , ("delete_from_subsection", delete_from_subsection)  
            ]  

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


add_to_subsection :: [[String]] -> [String] -> IO()
add_to_subsection list args = 
    do
        if length args > 2
             then 
                 do 
                     -- checks to see if there is a subsection in the list that matches the one in args 
                     -- if there is, it returns the list with the item written in the last position of the subsection list
                     -- if not, we'll return an empty list and print a message to the user that the subsection did not exist 
                     let newList = check_for_subsection list list args 0
                     if length newList > 1
                         then 
                             do 
                                 write_file newList args
                         else 
                             do
                                 putStrLn "That subsection does not exist."
             else
                 do 
                     putStrLn "Incorrect number of arguments - no item to append to the given subsection."


check_for_subsection :: [[String]] -> [[String]] -> [String] -> Int -> [[String]]
check_for_subsection [x] oldList args count =
    do  
        if ((head x) == (args !! 1))
            then
                do
                    let newInnerList = x ++ [args !! 2]
                    -- if the subsection is the first inner list that we come across 
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
            -- if the last inner list is not the subsection that we're looking for, it's not in the list at all
            -- we return a list with an empty string 
            else 
                do
                    [[""]]

check_for_subsection currentList oldList args count =
    do  
        let x = head currentList
        if ((head x) == (args !! 1))
            then
                do
                    -- newInnerList is the sublist + the item we want to append as the last element of the list
                    -- we can do stuff like "append" vs. "prepend" later 
                    let newInnerList = x ++ [args !! 2]
                    -- if the subsection is the first inner list that we come across  
                    -- (not actually a possible case, I'm pretty sure - the head of the list will always be the title of the file) 
                    if count == 0
                         then 
                            do
                                [newInnerList] ++ tail oldList
                         else 
                            do
                                -- if the subsection is the last possible line in the list (this IS possible)
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
            -- if the current inner list is not the subsection that we are looking for, then we call the function again with the tail
            else 
                do
                    check_for_subsection (tail currentList) oldList args (count+1) 
                    



create_list :: [String] -> IO()
create_list list = 
    print(list)

rename_list :: [[String]] -> [String] -> IO()
rename_list list args = 
    print(list)

view :: [[String]] -> [String] -> IO()
view list args = 
    print(list)

delete_item :: [[String]] -> [String] -> IO()
delete_item list args =
    print(list)

delete_subsection :: [[String]] -> [String] -> IO()
delete_subsection list args =
    print(list)

delete_from_subsection :: [[String]] -> [String] -> IO()
delete_from_subsection list args =
    print(list)


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
-- More logic is needed to put an extra space after the item if it is the last item of a subsection 
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
                                -- if it's a subsection of the file
                                if (x !! 0 == '#' && x !! 1 /= '#')
                                    then
                                        do
                                            appendFile (head args) ("\n" ++ x ++ "\n")
                                    -- else, it's probably an item (subsection item or not)
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


-- Breaks down subsections into their individual items
-- Subsection heading is the first item in the list it returns  
item_breakdown :: [String] -> [String] -> Int -> ([String], Int)
item_breakdown [] elementList count = (elementList, count)
item_breakdown [x] elementList count =
     do 
        -- if the element is a new subsection header or a newline, then we know that it is not a subsection item 
         if (length x > 0)
            then 
                 do 
                      let a = head x
                      if (a == '#' || a == ' ')
                           then 
                                do
                                     (elementList, count)
                           -- otherwise, it is an item to be included in the subsection list 
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
          buffer ++ [[h]]
          
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
                                             -- newElement is the "subsection" that we're adding to the buffer. I
                                             -- It consists of the name of the subsection at list[0], and the items belonging to it from list[1]-[n]
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
                                                       -- else, it's a subsection-less element 
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
                 if (command == "create_list")
                     then 
                         do 
                             create_list args
                     else
                         do  
                             let (Just action) = lookup command dispatch
                             let fileName = head args
                             fileExists <- doesFileExist fileName  
                             if fileExists  
                                then 
                                    do 
                                        -- if the file exists, then we read it and pass the resulting list to the action that the user wanted
                                        -- each action takes in the buffer (the list) and the arguments (the name of the subsection, etc.)
                                        let buffer = read_file fileName
                                        let list = breakdown buffer []
                                        action list args
                                else 
                                    do 
                                        putStrLn "This file does not exist" 
         else 
             do
                 putStrLn "Incorrect number of arguments"    