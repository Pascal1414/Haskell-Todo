import System.Console.Haskeline (getInputLine)
import Text.Read (readMaybe)

data Todo = Todo { id :: Int, title :: String, doUntil :: String }

main :: IO()
main = do
    putStrLn "Welcome to Todo-App"   
    menu


menuOptions :: [String]
menuOptions = ["Print Todos", "Add Todo", "Remove Todo"]

menu :: IO()
menu = do
    printMenu
    getMenuSelection
    menu

printMenu :: IO ()
printMenu = do
    mapM_ putStrLn $ zipWith (\i opt -> show i ++ ". " ++ opt) [1..] menuOptions

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

