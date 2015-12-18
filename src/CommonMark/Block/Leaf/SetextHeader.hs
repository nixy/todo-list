module CommonMark.Block.Leaf.SetextHeader
( SetextHeader

, makeSetextHeader
, unmakeSetextHeader

, isSetextHeader
, isNotIndentedCode
) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isInfixOf)

-- Establish a type alias for Setext headers
type SetextHeader = String

-- Turns a string into a setext Header
-- TODO: Do error handling for ints != 1,2
makeSetextHeader :: Int -> String -> SetextHeader
makeSetextHeader 1 string = string ++ '\n':(take (length string) (repeat '='))
makeSetextHeader 2 string = string ++ '\n':(take (length string) (repeat '-'))
makeSetextHeader int string = string -- Error case

-- Turns a Setext Header into a string
unmakeSetextHeader:: SetextHeader -> String
unmakeSetextHeader header = 
    if (length header > 0)
        then do
            head (lines header)
        else do
            header

-- Checks if a string is a setext header
-- TODO: Make more efficient
isSetextHeader :: String -> Bool
isSetextHeader string = notCodeBlock &&
                         header /= "" &&
                         (isPrefixOf "--" underline ||
                          isPrefixOf "=" underline) &&
                         not (isInfixOf " " underline')
                where
                    notCodeBlock = and (map isNotIndentedCode (lines string))
                    header = dropWhile isSpace (dropWhile (/= '\n') string)
                    underline = dropWhile isSpace (dropWhile (/= '\n') string)
                    underline' = reverse (dropWhile isSpace (reverse underline))

-- Checks if a string is not a setext header
isNotSetextHeader :: String -> Bool
isNotSetextHeader string = not (isSetextHeader string)
