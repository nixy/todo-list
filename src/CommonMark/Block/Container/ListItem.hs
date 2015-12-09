module CommonMark.Block.Container.ListItem
( ListItem

, makeListItem
, unmakeListItem

, isListItem
, isNotListItem
) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)

-- Establish a type alias for list items
type ListItem = String

-- A list of valid bullet list markers
bulletListMarkers = ['-','+','*']

-- A list of valid ordered list markers
orderedListPrefix = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
orderedListSuffix = ['.', ')']


