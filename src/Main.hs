{-
 _______          _____          _      _     _     __  __                                   
|__   __|        |  __ \        | |    (_)   | |   |  \/  |                                  
   | | ___ ______| |  | | ___   | |     _ ___| |_  | \  / | __ _ _ __   __ _  __ _  ___ _ __ 
   | |/ _ \______| |  | |/ _ \  | |    | / __| __| | |\/| |/ _` | '_ \ / _` |/ _` |/ _ \ '__|
   | | (_) |     | |__| | (_) | | |____| \__ \ |_  | |  | | (_| | | | | (_| | (_| |  __/ |   
   |_|\___/      |_____/ \___/  |______|_|___/\__| |_|  |_|\__,_|_| |_|\__,_|\__, |\___|_|   
                                                                              __/ |          
                                                                             |___/  
-}
import System.Environment

main :: IO()
main = do
   putStrLn "=============================="
   putStrLn "INITIATING TO-DO LIST MANAGER"
   putStrLn "=============================="

   let startPrompt = unlines ["Hello, enter your Daily, Weekly, or Monthly tasks.",
                              ">>>Type 'help' for a general list of commands & instructions. Press CTRL/CMD + C to quit."]

   putStrLn startPrompt   
   cmd <- getLine
   if cmd == "help" 
      then putStrLn (unlines [">>> List of commands:"
                              , "\t add_to_list"
                              , "\t add_to_subsection"
                              , "\t rename_list"
                              , "\t view"
                              , "\t delete_subsection"
                              , "\t delete_item"
                              , "\t delete_from_subsection"
                              , "\t create_new_todo_list"])
      else putStrLn ""
   getArgs >>= print . head
