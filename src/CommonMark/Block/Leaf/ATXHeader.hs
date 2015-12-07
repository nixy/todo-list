module CommonMark.Block.Leaf.ATXHeader
( ATXHeader

, makeATXHeader
, unmakeATXHeader

, isATXHeader
, isNotIndentedCode
) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)
import Data.List (isPrefixOf)

-- Establish a type alias for ATX headers
type ATXHeader = String

-- Turns a string into an ATX header
-- TODO: Do error handling for if int is greater than 6
makeATXHeader :: String -> Int -> ATXHeader
makeATXHeader string int 
    | (int <= 6) && (int > 0) = (take int (repeat '#')) ++ " " ++ string
    | otherwise = string --error case

-- Turns an ATX Header into a string
unmakeATXHeader :: ATXHeader -> String
unmakeATXHeader string = drop 1 (dropWhile (== '#') (dropWhile (== ' ') string))

-- Checks if a string is an ATX header
isATXHeader :: String -> Bool
isATXHeader string = isNotIndentedCode string &&
                      (length prefix) > 0 &&
                      (length prefix) < 7 &&
                      (isPrefixOf " " suffix || suffix == "")
             where
                 prefix = takeWhile (== '#') (dropWhile (== ' ') string)
                 suffix = dropWhile (== '#') (dropWhile (== ' ') string)

-- Checks if a string is not an ATX header
isNotATXHeader :: String -> Bool
isNotATXHeader string = not (isATXHeader string)
