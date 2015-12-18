module CommonMark.Block.Leaf.Append
( append
, append_header

) where

import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader (makeSetextHeader, unmakeSetextHeader)
import CommonMark.Block.Leaf.ATXHeader 
import CommonMark.Block.Leaf.Write


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ APPEND FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

-- Prepends an item to the list 
-- if typeOf is h, it appends a header of a specified or default type 
-- if typeOf is p, it adds a paragraph 
append :: Char -> [String] -> [String] -> String -> IO()
append typeOf args todoList fileName =
    do
        -- if it is h, it calls the append header function  
        if (typeOf == 'h')
            then do
                let newList = append_header args todoList
                -- if no item was added to the list, then it was unsuccessful 
                if (length newList > 0)
                    then do
                        write_file newList fileName 0
                        print "Successfully wrote to file."
                    else do
                        putStrLn "No item to append"
            -- if it is p, it calls the append paragraph function 
            else if (typeOf == 'p')
                then do
                    let newList = append_paragraph args todoList
                    -- if no item was added to the list, then it was unsuccessful 
                    if (length newList > 0)
                        then do
                            write_file newList fileName 0
                            print "Successfully wrote to file."
                        else do
                            putStrLn "No item to append"
                else do
                    print "add later"

-- Prepend_header - takes the string from the user and adds it to the end of the file 
-- has default and specified types of header 
append_header :: [String] -> [String] -> [String]
append_header args todoList = 
    do
        -- if the argument has a specific type of header to add, then we check 
        if (length args > 1)
            then do
                let hType = args !! 1
                if (hType == "=") 
                    then do 
                        -- adds setext header to end of the list 
                        let newHeader = makeSetextHeader (args !! 0) 1
                        let newList =   todoList ++ [newHeader]
                        newList 
                    else if (hType == "-")
                        then do
                            -- adds setext header to start of the list 
                            let newHeader = makeSetextHeader (args !! 0) 2
                            let newList =  todoList ++ [newHeader] 
                            newList 
                        else do 
                            -- adds atx header to start of the list 
                            let newHeader = makeATXHeader (args !! 0) 1
                            let newList =  todoList ++ [newHeader]
                            newList
             -- else, we assume the default (setext header )
            else if (length args > 0)
                then do
                     let newHeader = makeSetextHeader (args !! 0) 1
                     let newList =   todoList ++ [newHeader]
                     newList 
                else 
                    []

-- Prepend_paragraph - takes the string from the user and adds it to the end of the file 
append_paragraph :: [String] -> [String] -> [String]
append_paragraph args todoList = 
    do
        -- if the user did not specify an argument to prepend, we return an empty list 
        -- else, we write the item 
        if (length args > 0)
            then do
                let newList =  todoList ++ [(args !! 0)]
                newList 
            else do
                []