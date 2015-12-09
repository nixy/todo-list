module CommonMark.BlankLine
( BlankLine

, isBlankLine
, isNotBlankLine
) where

import Data.Char (isSpace)

-- Establish a type alias for blank lines
type BlankLine = String

isBlankLine :: String -> Bool
isBlankLine string = and (map (isSpace) string)

isNotBlankLine :: String -> Bool
isNotBlankLine string = not (isBlankLine string)
