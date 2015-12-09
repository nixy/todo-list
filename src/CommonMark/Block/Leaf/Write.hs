module CommonMark.Block.Leaf.Write
( write_file

) where

import System.IO
import System.Environment  
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader 
import CommonMark.Block.Leaf.ATXHeader 

write_file :: [String] -> String -> Int -> IO()
write_file [x] fileName count =
    do
        appendFile fileName x
write_file todoList fileName 0 = 
    do
        let x = head todoList 
        if (length x > 0)
            then if (isSetextHeader x == True || isATXHeader x == True)
                then do
                    writeFile fileName (x ++ "\n")
                    write_file (tail todoList) fileName 1
                else do
                    writeFile fileName x
                    write_file (tail todoList) fileName 1
            else do
                write_file (tail todoList) fileName 1
write_file todoList fileName count = 
    do
        let x = head todoList 
        if (isSetextHeader x == True || isATXHeader x == True)
            then do
                appendFile fileName (x ++ "\n")
                write_file (tail todoList) fileName count
            else do 
                appendFile fileName x
                write_file (tail todoList) fileName count

