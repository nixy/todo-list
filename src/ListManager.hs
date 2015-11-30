module ListManager
( ListTitle
, List

, makeListTitle
, makeList
) where

import ItemManager
import SectionManager

listStyle = '='
--listStyle = '#'

type ListTitle = String

type List = [String]

makeListTitle :: String -> ListTitle
makeListTitle string 
    | listStyle == '=' = string ++ '\n':(take (length string) (repeat listStyle))
    | listStyle == '#' = listStyle:string
    | otherwise = "ERROR DEAL WITH"

-- Makes a List from a ListTitle, a list of Items, and a list of Sections
makeList :: ListTitle -> [Item] -> [Section] -> List
makeList listTitle items sections = ([listTitle] ++ items) ++ (concat sections)

--parseList :: [String] -> ListTitle -> [Items] -> [Sections] -> (ListTitle, [Items], [Sections])
--parseList string "" [] [] = parseListTitle string
--                            parseList (string) title [] []
--parseList string title [] [] = parseItems string
--                               parseList (string) title items []
--parseList string title items [] = parseSections string
--                                  parseList "" title items sections
--parseList string title items sections = (title, items, sections)
