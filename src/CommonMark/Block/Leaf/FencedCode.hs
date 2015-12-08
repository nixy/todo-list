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
-- TODO: DEFINETELY a better way to do this
-- TODO: Deal witht he fact that backticks cannot be on info strings
-- TODO: Should split string on words for one line code fencces
-- TODO: Fenced code block should be closed by the end of the document or
-- containing structure, possible solution in the preprocessor
isFencedCode :: String -> Bool
isFencedCode string = (firstFenceNotIndentedCode && lastFenceNotIndentedCode) &&
                      (length (lines string) > 1) &&
                      ((isPrefixOf "```" firstFence &&
                        isPrefixOf "```" lastFence) ||
                       (isPrefixOf "~~~" firstFence &&
                        isPrefixOf "~~~" lastFence)) &&
                      (not (isInfixOf " " firstFence') &&
                       not (isInfixOf " " lastFence') &&
                       lengthOfFirstFence <= lengthOfLastFence)
    where
        infoStringTrim = \x -> (isPrint x) && (x /= '`') && (x /= '~')
        lengthFunc = \x -> if (x == '`' || x == '~') then True else False

        firstFence = dropWhile (== ' ') (head (lines string))
        firstFence' = reverse (dropWhile (infoStringTrim) (reverse firstFence))
        lengthOfFirstFence = length (takeWhile lengthFunc firstFence)
        firstFenceNotIndentedCode = isNotIndentedCode (head (lines string))

        lastFence = dropWhile (== ' ') (last (lines string))
        lastFence' = reverse (dropWhile (infoStringTrim) (reverse lastFence))
        lengthOfLastFence = length (takeWhile lengthFunc lastFence)
        lastFenceNotIndentedCode = isNotIndentedCode (last (lines string))



-- Checks if a string is not a fenced code block
isNotFencedCode :: String -> Bool
isNotFencedCode string = not (isFencedCode string)
