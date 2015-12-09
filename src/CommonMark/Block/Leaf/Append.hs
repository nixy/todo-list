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

append :: Char -> [String] -> [String] -> String -> IO()
append typeOf args todoList fileName =
    do
        if (typeOf == 'h')
            then do
                let newList = append_header args todoList
                if (length newList > 0)
                    then do
                        write_file newList fileName 0
                    else do
                        putStrLn "No item to append"
            else if (typeOf == 'p')
                then do
                    let newList = append_paragraph args todoList
                    if (length newList > 0)
                        then do
                            write_file newList fileName 0
                        else do
                            putStrLn "No item to append"
                else do
                    print "add later"

append_header :: [String] -> [String] -> [String]
append_header args todoList = 
    do
        if (length args > 1)
            then do
                let hType = args !! 1
                if (hType == "=")
                    then do 
                        let newHeader = makeSetextHeader (args !! 0) 1
                        let newList =   todoList ++ [newHeader]
                        newList 
                    else if (hType == "-")
                        then do
                            let newHeader = makeSetextHeader (args !! 0) 2
                            let newList =  todoList ++ [newHeader] 
                            newList 
                        else do 
                            let newHeader = makeATXHeader (args !! 0) 1
                            let newList =  todoList ++ [newHeader]
                            newList

            else if (length args > 0)
                then do
                     let newHeader = makeSetextHeader (args !! 0) 1
                     let newList =   todoList ++ [newHeader]
                     newList 
                else 
                    []
append_paragraph :: [String] -> [String] -> [String]
append_paragraph args todoList = 
    do
        if (length args > 0)
            then do
                let newList =  todoList ++ [(args !! 0)]
                newList 
            else do
                []