module ListManager
( ListTitle
, List

, makeListTitle
, makeList
) where

import ItemManager
import SectionManager

import Data.List (findIndex, findIndices, isPrefixOf, intersperse, groupBy)
import Data.Maybe (fromJust)

listStyle = '='
--listStyle = '#'

type ListTitle = String

type List = (ListTitle, [Item], [Section])

makeListTitle :: String -> ListTitle
makeListTitle string 
    | listStyle == '=' = string ++ '\n':(take (length string) (repeat listStyle))
    | listStyle == '#' = listStyle:string

-- Makes a List from a ListTitle, a list of Items, and a list of Sections
makeList :: ListTitle -> [Item] -> [Section] -> List
makeList listTitle items sections = (listTitle, items, sections)

-- Checks if a string is a ListTitle
isListTitle :: String -> Bool
isListTitle string 
    | listStyle == '=' = isPrefixOf "=" identifierString
    | listStyle == '#' = isPrefixOf "#" titleString &&
                         isPrefixOf "##" titleString
    where
        titleString = takeWhile (/= '\n') string
        identifierString = filter (/= '\n') (dropWhile (/= '\n') string)

-- Merges the list title into a single string if it isn't already
mergeListTitles :: [String] -> [String]
mergeListTitles stringList 
    | listStyle == '=' = map (concat) (map (intersperse "\n") (groupBy (\x y -> isListTitle (x ++ "\n" ++ y)) stringList))
    | listStyle == '#' = stringList

-- Merges the section title into a single string if it isn't already
mergeSectionTitles :: [String] -> [String]
mergeSectionTitles stringList
    | sectionStyle == '-' = map (concat) (map (intersperse "\n") (groupBy (\x y -> isSectionTitle (x ++ "\n" ++ y)) stringList))
    | sectionStyle == '#' = stringList

-- ALL PARSING SHOULD BE FIXED AT A LATER DATE
-- MOST OF THESE ARE HACKY AND IMPERFECT

-- Returns the ListTitle in a list of strings
parseListTitle :: [String] -> ListTitle
parseListTitle stringList =
    stringList !! (fromJust (findIndex (isListTitle) stringList))

-- Returns the indices of all the section titles in a list of strings
parseSectionTitles :: [String] -> [Int]
parseSectionTitles stringList =
    (findIndices (isSectionTitle) stringList) ++ [(length stringList)]
    
-- Returns a list of free items given a list of strings
parseItems :: [String] -> [Item]
parseItems stringList = ["1","2"]


-- Returns a list of sections given a list of strings
parseSections :: [String] -> [Int] -> [Section] -> [Section]
parseSections stringList [] [] = 
    parseSections stringList (parseSectionTitles stringList) [] 
parseSections stringList [x] sections =
    sections
parseSections stringList (x:xs) sections = 
        parseSections stringList xs (sections ++ section)
    where
        y = head xs
        section = [take (y - x) (drop x stringList)]

parseList :: [String] -> ListTitle -> [Item] -> [Section] -> (ListTitle, [Item], [Section])
parseList stringList "" [] [] =
    parseList stringList (parseListTitle stringList) [] []
parseList stringList title [] [] =
    parseList stringList title (parseItems stringList) []
parseList stringList title items [] = 
    parseList stringList title items (parseSections stringList [] [])
parseList string title items sections = (title, items, sections)
