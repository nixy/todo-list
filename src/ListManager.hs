module ListManager
( ListTitle
, List

, makeListTitle
, makeList

--, parseList
) where

import ItemManager
import SectionManager

type ListTitle = String

type List = (ListTitle, [Section])

makeListTitle :: String -> ListTitle
makeListTitle string = string ++ "\n" ++ (take (length string) (repeat '='))

makeList :: ListTitle -> [Section] -> List
makeList listTitle sections = (listTitle, sections)

--parseList :: String -> List
--parseList string = 
