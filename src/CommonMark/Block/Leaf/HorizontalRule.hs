module CommonMark.Block.Leaf.HorizontalRule
( HorizontalRule

, makeHorizontalRule

, isHorizontalRule
, isNotHorizontalRule
) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)
import Data.List (intersperse)

-- Establish a type alias for horizontal rules
type HorizontalRule = String

-- Creates a horizontal rule given certain parameters
makeHorizontalRule int bool char = if (validInteger && validCharacter) then
                                    horizontalRule else
                                    "Error" --Error handling
    where
        horizontalRule = if (bool) then 
                         take (int * 2 - 1) (intersperse ' ' (repeat char)) else
                         take int (repeat char)
        validInteger = (int >=3)
        validCharacter = (char == '*' || char == '-' || char == '_')
                         
-- Checks if a string is a horizontal rule
isHorizontalRule :: String -> Bool
isHorizontalRule string = isNotIndentedCode string &&
                          length invalidCharacters == 0 &&
                          length validCharacters >= 3 &&
                          ((all (== '*') validCharacters) ||
                           (all (== '-') validCharacters) ||
                           (all (== '_') validCharacters))
    where
        validCharacterFilter = \x -> x == '*' || x == '-' || x == '_'
        invalidCharactersFilter = \x -> x /= '*' && x /= '-' && x /= '_' &&
                                        x /= ' ' && x /= '\n'

        validCharacters = filter (validCharacterFilter) string
        invalidCharacters = filter (invalidCharactersFilter) string

-- Checks if a string is not a horizontal rule
isNotHorizontalRule :: String -> Bool
isNotHorizontalRule string = not (isHorizontalRule string)
