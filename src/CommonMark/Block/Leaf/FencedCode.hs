module CommonMark.Block.Leaf.FencedCode
( FencedCode

, isFencedCode
, isNotFencedCode
) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)
import Data.Char (isPrint, isSpace)
import Data.List (isInfixOf, isPrefixOf)

-- Establish a type alias for a fenced code block
type FencedCode = String

-- Checks if a string is a fenced code block
-- TODO: Probably a better way to do this
-- TODO: Fenced code block should be closed by the end of the document or
-- containing structure
isFencedCode :: String -> Bool
isFencedCode string = notCodeBlock &&
                      ((isPrefixOf "```" firstFence && 
                        isPrefixOf "```" lastFence &&
                        lengthOfFirstFence <= lengthOfLastFence &&
                        not (isInfixOf " " firstFence'') &&
                        not (isInfixOf " " lastFence'')) ||
                       (isPrefixOf "~~~" firstFence &&
                        isPrefixOf "~~~" lastFence &&
                        lengthOfFirstFence <= lengthOfLastFence &&
                        not (isInfixOf " " firstFence'') &&
                        not (isInfixOf " " lastFence'')))

    where
        notCodeBlock = and (map isNotIndentedCode (lines string))

        filterFunc = \x -> isPrint x && not (isSpace x)

        firstFence = dropWhile (== ' ') (head (lines string))
        firstFence' = dropWhile filterFunc (reverse firstFence)
        firstFence'' = reverse (dropWhile isSpace (firstFence'))

        lastFence = dropWhile (== ' ') (last (lines string))
        lastFence' = dropWhile filterFunc (reverse lastFence)
        lastFence'' = reverse (dropWhile isSpace (lastFence'))

        lengthFunc = \x -> if (x == '`' || x == '~') then True else False

        lengthOfFirstFence = length (takeWhile lengthFunc firstFence)
        lengthOfLastFence = length (takeWhile lengthFunc lastFence)

getFence :: String -> String
getFence string = firstFence''
    where
        firstFence = dropWhile (== ' ') (head (lines string))
        firstFence' = dropWhile filterFunc (reverse firstFence)
        firstFence'' = reverse (dropWhile isSpace (firstFence'))
        filterFunc = \x -> isPrint x && not (isSpace x)
        

-- Checks if a string is not a fenced code block
isNotFencedCode :: String -> Bool
isNotFencedCode string = not (isFencedCode string)
