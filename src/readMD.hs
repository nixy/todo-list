import System.IO
import System.Environment  
import System.IO.Unsafe 
import Data.List
import Data.List (intercalate)
import Data.Char

-- Returns a list with all of the lines from the XML file 
-- I'm just feeding it a file name for now
read_file :: String -> [String]
read_file fileName = 
    do
             let file = unsafePerformIO . readFile $ fileName
             lines file

-- Breaks down subsections into their individual items
-- Subsection heading is the first item in the list it returns  
item_breakdown :: [String] -> [String] -> Int -> ([String], Int)
item_breakdown [] elementList count = (elementList, count)
item_breakdown [x] elementList count =
     do 
        -- if the element is a new subsection header or a newline, then we know that it is not a subsection item 
         if (length x > 0)
            then 
                 do 
                      let a = head x
                      if (a == '#' || a == ' ')
                           then 
                                do
                                     (elementList, count)
                           -- otherwise, it is an item to be included in the subsection list 
                           else 
                                do
                                     let newCount = count + 1
                                     let newElementList = elementList ++ [x]
                                     (newElementList, newCount)
             else (elementList, count)


item_breakdown remaining elementList count =
    do
        if (length remaining > 0)
            then 
                do 
                     let h = head remaining
                     let t = tail remaining
                     if (length h > 0)
                        then 
                            do 
                                 let a = head h
                                 if a == '#'
                                     then (elementList, count)
                                     else
                                         do
                                             let newCount = count+1
                                             let newElementList = elementList ++ [h]
                                             let i = item_breakdown t newElementList newCount
                                             let (a, b) = i 
                                             (a, b)
                        else (elementList, count)
             else (elementList, count)

            
breakdown :: [String] -> [[String]] -> [[String]]
breakdown [] buffer = buffer 
breakdown [h] buffer = 
     do 
          buffer ++ [[h]]
          
breakdown fileData buffer = 
     do
        if (length fileData > 0)
             then 
                 do 
                      let h = head fileData
                      let t = tail fileData 
                      if (length h > 0)
                         then 
                            do 
                                let a = head h
                                if a == '#' -- if we encounter a sublist, we must extract all the elements that belong to the sublist
                                    then
                                        do 
                                             -- item_breakdown gets all of the elements belonging to the sublist and returns them 
                                             -- it also returns a count of how many items were in that list, so that we know how many to remove from t before recursing again 
                                             let (sublist, count) = item_breakdown t [h] 0 
                                             -- We don't want the part of the list with the old items, so we leave them out
                                             let (_, remaining) = splitAt count t
                                             -- newElement is the "subsection" that we're adding to the buffer. I
                                             -- It consists of the name of the subsection at list[0], and the items belonging to it from list[1]-[n]
                                             let newBuffer = buffer ++ [sublist]
                                             breakdown remaining newBuffer  
                                     else 
                                         if a == ' '
                                             then 
                                                  do
                                                       -- if it's just a blank line in the text, we don't add that as an element in the list and just move on 
                                                       breakdown t buffer 
                                             else 
                                                  do 
                                                       -- else, it's a subsection-less element 
                                                       -- we will probably want more elses in here for error-handling, but for now, I'm just assuming these 3 possibilities 
                                                       let newBuffer = buffer ++ [[h]]
                                                       breakdown t newBuffer 
                         else breakdown t buffer
             else buffer


main = do
             -- Takes in the text file with the list of coordinates, and returns a tuple with the list of coordinates,
             -- and an int, which is the number of evolutions to print
             args <- getArgs
             let [file] = args
             let items = read_file (file)
             if (length items > 0)
                then 
                    do
                        let buffer = breakdown (tail items) []
                        let a = head items
                        let printList = [[a]] ++ buffer
                        print (printList)
                else print ("gaahhhhhh")