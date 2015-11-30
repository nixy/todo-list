module SectionManager
( SectionTitle
, Section

, makeSectionTitle
, makeSection
) where

import ItemManager

sectionStyle = '-' :: Char
-- sectionStyle = '#' :: Char

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
    | sectionStyle == '#' = (take 2 (repeat sectionStyle)) ++ string
    | otherwise = "ERROR DEAL WITH"

-- Makes a section given a SectionTitle and list of Items
makeSection :: SectionTitle -> [Item] -> Section
makeSection title items = (title ++ "\n"):items

-- Changes the SectionTitle
--renameSection :: SectionTitle -> Section -> Section

-- Appends an item to a section
--appendItemToSection :: Item -> Section -> Section
--appendItemToSection item section = section ++ '\n':item

-- Prepends an item to a section
--prependItemToSection :: Item -> Section -> Section
--prependItemToSection item section = gt

--isSectionTitle :: String -> Bool
--isSectionTitle string

--parseSection :: [String]
