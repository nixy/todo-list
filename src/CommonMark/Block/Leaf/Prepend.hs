module CommonMark.Block.Leaf.Prepend
( prepend
, prepend_header

) where

import System.IO
import System.Environment  
import System.IO.Unsafe 
import System.Directory
import Data.List
import CommonMark.Block.Leaf.SetextHeader (makeSetextHeader, unmakeSetextHeader)
import CommonMark.Block.Leaf.ATXHeader 
import CommonMark.Block.Leaf.Write


-- @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ PREPEND FUNCTIONS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

prepend :: Char -> [String] -> [String] -> String -> IO()
prepend typeOf args todoList fileName =
    do
        if (typeOf == 'h')
            then do
                let newList = prepend_header args todoList 
                if (length newList > 0)
                    then do
                        write_file (newList) fileName 0
                        -- print newList
                    else do
                        putStrLn "No item to prepend"
            else if (typeOf == 'p')
                then do
                    let newList = prepend_paragraph args todoList 
                    if (length newList > 0)
                        then do
                            write_file newList fileName 0
                        else do
                            putStrLn "No item to prepend"
                else do
                    print "add later"

prepend_header :: [String] -> [String] -> [String]
prepend_header args todoList =
    do
        if (length args > 1)
            then do
                let hType = args !! 1
                if (hType == "=")
                    then do 
                        let newHeader = makeSetextHeader (args !! 0) 1
                        let newList = [newHeader ++ "\n"] ++ todoList 
                        newList 
                    else if (hType == "-")
                        then do
                            let newHeader = makeSetextHeader (args !! 0) 2
                            let newList = [newHeader ++ "\n"] ++ todoList  
                            newList 
                        else do 
                            let newHeader = makeATXHeader (args !! 0) 1
                            let newList = [newHeader ++ "\n"] ++ todoList 
                            newList

            else if (length args > 0)
                then do
                    let newHeader = makeSetextHeader (args !! 0) 1
                    let newList = [newHeader ++ "\n"] ++ todoList 
                    newList 
                else 
                    []

prepend_paragraph :: [String] -> [String] -> [String]
prepend_paragraph args todoList =
    do
        if (length args > 0)
            then do
                let newList = [(args !! 0) ++ "\n"] ++ todoList 
                newList
            else do
                []