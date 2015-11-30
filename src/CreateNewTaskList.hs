module CreateNewTaskList(makeFile) where
import System.Directory
import System.IO

makeFile = do
			putStrLn "Please type a name for the task list:"
			filename <- getLine
			let fileName = filename

			currentDir <- getCurrentDirectory

			let newFilePath = currentDir ++ "\\" ++ fileName
			openFile newFilePath ReadWriteMode -- returns Handle not IO () which is problematic
			putStr "Created new list file: " 
			putStrLn newFilePath