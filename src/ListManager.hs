module ListManager
( ListTitle
, List

, makeListTitle
, makeList
) where

import ItemManager
import SectionManager

type ListTitle = String

type List = (ListTitle, [Section], [Item], [Doable])

makeListTitle :: String -> ListTitle
makeListTitle string = string ++ "\n" ++ (take (length string) (repeat '='))

makeList :: ListTitle -> [Section] -> [Item] -> [Doable] -> List
makeList listTitle sections items doables = (listTitle, sections, items, doables)

--isListTitle :: String -> Bool
--isListTitle string = 

--parseList :: String -> List
--parseList
