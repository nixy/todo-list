module CommonMark.Block.Leaf.HorizontalRule
( HorizontalRule

--, makeHorizontalRule
--, unmakeHorizontalRule
, isHorizontalRule
) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)

-- Establish a type alias for horizontal rules
type HorizontalRule = String

isHorizontalRule :: String -> Bool
isHorizontalRule string = isNotIndentedCode string &&
    where
        trimString = (dropWhile (== ' ') string)
