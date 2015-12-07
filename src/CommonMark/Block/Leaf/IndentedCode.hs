module CommonMark.Block.Leaf.IndentedCode
( IndentedCode

, makeIndentedCode
, unmakeIndentedCode

, isIndentedCode
, isNotIndentedCode
) where

import CommonMark.BlankLine (isNotBlankLine)
import Data.List (isPrefixOf)

-- Establish a type alias for an indented code block
type IndentedCode = String

-- Turns a string into an indented code block
makeIndentedCode :: String -> IndentedCode 
makeIndentedCode string = init (unlines (map (\x -> "    " ++ x) (lines string)))

-- Turns an indented code block a string
unmakeIndentedCode :: IndentedCode -> String
unmakeIndentedCode code = unlines (map (drop 4) (lines code))

-- Checks if a string is an indented code block
isIndentedCode :: String -> Bool
isIndentedCode string = and (map (isPrefixOf "    ") filteredString)
    where
        filteredString = (filter (isNotBlankLine) (lines string))

-- Checks if a string is not an indented code block
isNotIndentedCode :: String -> Bool
isNotIndentedCode string = not (isIndentedCode string)
