module CommonMark.Block.Leaf.Write
( write_file

) where

import System.IO
import System.Environment  
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader 
import CommonMark.Block.Leaf.ATXHeader 

-- recursive function for writing the list to the file indicated 
write_file :: [String] -> String -> Int -> IO()
-- if the count is 0, then we want to write to the file (as this is the first element in the list )
write_file todoList fileName 0 = 
    do
        let x = head todoList 
        if (length x > 0)
            -- if it is a header, we want to add an additional space 
            then if (isSetextHeader x == True || isATXHeader x == True)
                then do
                    writeFile fileName (x ++ "\n")
                    write_file (tail todoList) fileName 1
                -- if not, then we just add to file as normal 
                else do
                    writeFile fileName x
                    write_file (tail todoList) fileName 1
            else do
                writeFile fileName x
                write_file (tail todoList) fileName 1
-- base case for the last item in the list 
-- appends to file 
write_file [x] fileName count =
    do
        appendFile fileName x

-- else, we want to append to file (because we do not wish to overwrite the previous additions)
write_file todoList fileName count = 
    do
        let x = head todoList 
        -- if it is a header, we want to add an additional space 
        if (isSetextHeader x == True || isATXHeader x == True)
            then do
                appendFile fileName (x ++ "\n")
                write_file (tail todoList) fileName (count + 1)
             -- if not, then we just add to file as normal 
            else do 
                appendFile fileName x
                write_file (tail todoList) fileName (count + 1)

