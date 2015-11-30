module ItemManager
( Item
, Doable

, makeItem
, makeDoable

, isItem
, isDoable
, isDone

, doDoable
, undoDoable

, itemToDoable
, doableToItem
) where

import Data.List (isPrefixOf)

-- The style of the bullets to use
-- TEMPORARY
bulletStyle = '*' :: Char
-- bulletStyle = '-' :: Char
-- bulletStyle = '+' :: Char

-- An Item in a list
-- EX: * This is an item
type Item = String

-- A Doable Item in a list
-- EX: * [ ] Do this item
type Doable = String

-- Makes an Item from a string
makeItem :: String -> Item 
makeItem string = bulletStyle:" " ++ string

-- Makes a Doable from a string
makeDoable :: String -> Doable
makeDoable string = bulletStyle:" [ ] " ++ string

-- Checks if a string is an Item
isItem :: String -> Bool
isItem string
    | isDoable string = False
    | isPrefixOf (bulletStyle:" ") string = True
    | otherwise = False

-- Checks if a string is a Doable
isDoable :: String -> Bool
isDoable string
    | isPrefixOf (bulletStyle:" [ ] ") string = True
    | isPrefixOf (bulletStyle:" [x] ") string = True
    | otherwise = False

-- Checks if a Doable is done
isDone :: Doable -> Bool
isDone string
    | isPrefixOf (bulletStyle:" [ ] ") string = False
    | isPrefixOf (bulletStyle:" [x] ") string = True
    | otherwise = False

-- Marks a Doable as done
doDoable :: Doable -> Doable
doDoable doable = (head doable):" [x]" ++ (drop 5 doable)

-- Marks a Doable as undone
undoDoable :: Doable -> Doable
undoDoable doable = (head doable):" [ ]" ++ (drop 5 doable) 

-- Converts an Item to a Doable
itemToDoable :: Item -> Doable
itemToDoable item = (head item):" [ ]" ++ (tail item)

-- Converts a Doable to an Item
doableToItem :: Doable -> Item
doableToItem doable = (head doable):(drop 5 doable)
