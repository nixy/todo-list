module CommonMark.Block.Leaf.FencedCode
( FencedCode

, isFencedCode
, isNotFencedCode) where

import CommonMark.Block.Leaf.IndentedCode (isNotIndentedCode)
import Data.Char (isPrint, isSpace)
import Data.List (isInfixOf, isPrefixOf)

-- Establish a type alias for a fenced code block
type FencedCode = String

-- Checks if a string is a fenced code block
-- TODO: Deal with the fact that backticks cannot be on info strings
-- TODO: Should split string on words for one line code fencces
-- TODO: Fenced code block should be closed by the end of the document or
-- containing structure, possible solution in the preprocessor
isFencedCode :: String -> Bool
isFencedCode string = notIndentedCode &&
                      (length (lines string) > 1) &&
                      matchingFences &&
                      noGapsInFences &&
                      lastFenceIsLeastSameLength
    where
        isInfoString = \x -> (isPrint x) && (x /= '`') && (x /= '~')
        lengthFunc = \x -> if (x == '`' || x == '~') then True else False

        firstFence = dropWhile (== ' ') (head (lines string))
        firstFence' = reverse (dropWhile (isInfoString) (reverse firstFence))
        lengthOfFirstFence = length (takeWhile lengthFunc firstFence)

        lastFence = dropWhile (== ' ') (last (lines string))
        lastFence' = reverse (dropWhile (isInfoString) (reverse lastFence))
        lengthOfLastFence = length (takeWhile lengthFunc lastFence)

        firstFenceNotIndentedCode = isNotIndentedCode (head (lines string))
        lastFenceNotIndentedCode = isNotIndentedCode (last (lines string))

        notIndentedCode = firstFenceNotIndentedCode && lastFenceNotIndentedCode

        matchingFences = (isPrefixOf "```" firstFence && 
                          isPrefixOf "```" lastFence) ||
                         (isPrefixOf "~~~" firstFence &&
                          isPrefixOf "~~~" lastFence)

        noGapsInFences = not (isInfixOf " " firstFence') &&
                         not (isInfixOf " " lastFence')

        lastFenceIsLeastSameLength = lengthOfFirstFence <= lengthOfLastFence
-- Checks if a string is not a fenced code block
isNotFencedCode :: String -> Bool
isNotFencedCode string = not (isFencedCode string)
