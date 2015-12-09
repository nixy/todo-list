module CommonMark.Block.Leaf.LinkReference
( LinkReference

, makeLinkReference

, isLinkReference
, isNotLinkReference
) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)

-- Establish a type alias for link reference definitions
type LinkReference = String

-- Creates a link reference given certain parameters
-- TODO: Make more flexible. Pretty rudimentary.
makeLinkReference :: String -> String -> String -> LinkReference
makeLinkReference label destination title = linkLabel ++ destination ++ linkTitle
    where
        linkLabel = "[" ++ label ++ "]: "
        linkTitle = if not (null title) then
                    " \"" ++ title ++ "\"" else
                    ""
-- Checks if a string is a link reference
isLinkReference :: String -> Bool
isLinkReference string = 

-- Checks if a string is not a link reference
isNotLinkReference :: String -> Bool
isNotLinkReference string = not (isLinkReference string)
