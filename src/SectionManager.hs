module SectionManager
( SectionTitle
, Section

, makeSectionTitle
, makeSection
) where

import ItemManager

--
type SectionTitle = String

type Section = (SectionTitle, [Item], [Doable])

makeSectionTitle :: String -> SectionTitle
makeSectionTitle string = string ++ "\n" ++ (take (length string) (repeat '-'))

makeSection :: SectionTitle -> [Item] -> [Doable] -> Section
makeSection sectionTitle items doables= (sectionTitle, items, doables)

--isSectionTitle :: String -> Bool
--isSectionTitle string
