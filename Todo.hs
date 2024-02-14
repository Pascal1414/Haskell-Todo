import System.Console.Haskeline (getInputLine)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)

data Todo = Todo { id :: Int, title :: String, doUntil :: String }

main :: IO()
main = do
    putStrLn "Welcome to Todo-App"
    menu

data MenuOption = MenuOption { name :: String, function :: IO () }
menuOptions :: [MenuOption]
menuOptions = [MenuOption "Add a new task" addTask, MenuOption "Show all tasks" showTasks, MenuOption "Remove a task" removeTask, MenuOption "Exit" exit]

menu :: IO()
menu = do
    printMenu
    menuSelection <-  getMenuSelection
    function (menuOptions !! (menuSelection - 1))
    menu

printMenu :: IO ()
printMenu = do
    mapM_ putStrLn $ zipWith (\i opt -> show i ++ ". " ++ opt) [1..] (map name menuOptions)

getMenuSelection :: IO Int
getMenuSelection = do
    number <- getNumberInput
    if number > length menuOptions || number < 1
    then do
        putStrLn "Option does not exist"
        getMenuSelection
    else return number


getNumberInput :: IO Int
getNumberInput = do
    putStrLn "Please enter a number:"
    input <- getLine
    case readMaybe input of
        Just number -> return number
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid number."
            getNumberInput


addTask :: IO ()
addTask = putStrLn "Adding a new task"

showTasks :: IO ()
showTasks = putStrLn "Showing all tasks"

removeTask :: IO ()
removeTask = putStrLn "Removing a task"

exit :: IO ()
exit = do
    putStrLn "Ok Bye Bye!"
    exitSuccess