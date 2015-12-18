module CommonMark.BlankLine
( BlankLine

, isBlankLine
, isNotBlankLine
) where

import Data.Char (isSpace)

-- Establish a type alias for blank lines
type BlankLine = String

-- Checks if a string is a blank line
isBlankLine :: String -> Bool
isBlankLine string = and (map (isSpace) string)

-- Checks if a string is not a blank line
isNotBlankLine :: String -> Bool
isNotBlankLine string = not (isBlankLine string)
