module SectionManager
( SectionTitle
, Section

, sectionStyle

, makeSectionTitle
, makeSection

, isSectionTitle
, isSection

, renameSection

, appendItemToSection
, prependItemToSection
) where

import ItemManager

import Data.List (isPrefixOf)

--TESTING ONLY
sectionTitle = makeSectionTitle "This is a section"
itemList = [makeItem "Item 1", makeItem "Item 2"]
section = makeSection sectionTitle itemList
stringList = ["##Section", "-------", "* Item 1", "* Item 2", "* Item 3"]
--END TESTING

sectionStyle = '-' :: Char
--sectionStyle = '#' :: Char

-- A section title is a title with a markdown identifier
-- EX: ##Section1
-- EX: Section1
--     --------
type SectionTitle = String

-- A section is a list of Items starting with a SectionTitle
type Section = [String]
-- type Section = (SectionTitle, [Item])

makeSectionTitle :: String -> SectionTitle
makeSectionTitle string 
    | sectionStyle == '-' = string ++ '\n':(take (length string) (repeat sectionStyle))
    | sectionStyle == '#' = sectionStyle:sectionStyle:string

-- Makes a section given a SectionTitle and list of Items
makeSection :: SectionTitle -> [Item] -> Section
makeSection title items = 
    (title ++ "\n"):items

-- Checks if a string is a SectionTitle
isSectionTitle :: String -> Bool
isSectionTitle string 
    | sectionStyle == '-' = isPrefixOf "--" identifierString
    | sectionStyle == '#' = isPrefixOf "##" titleString && 
                            not (isPrefixOf "###" titleString)
    | otherwise = False
    where
        titleString = takeWhile (/= '\n') string
        identifierString = filter (/= '\n') (dropWhile (/= '\n') string)
        
-- Checks if a string array is a Section
isSection :: [String] -> Bool
isSection stringList
    | isSectionTitle (head stringList) = and (map (isItem) (tail stringList))
    | otherwise = False 

-- Changes the SectionTitle
renameSection :: SectionTitle -> Section -> Section
renameSection sectionTitle section =
    (sectionTitle ++ "\n"):(drop 1 section)

-- Appends an item to a section
appendItemToSection :: Item -> Section -> Section
appendItemToSection item section =
    section ++ [item]

-- Prepends an item to a section prependItemToSection :: Item -> Section -> Section
prependItemToSection :: Item -> Section -> Section
prependItemToSection item (head:tail) =
    [head] ++ [item] ++ tail
