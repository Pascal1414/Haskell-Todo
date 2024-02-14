import System.Console.Haskeline (getInputLine)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import Control.Monad.RWS (MonadState(put))

data Todo = Todo { title :: String, doUntil :: String }

todos :: [Todo]
todos = [Todo  "Buy milk" "2020-12-24", Todo  "Learn Haskell" "2020-12-25"]

main :: IO()
main = do
    putStrLn "Welcome to Todo-App"
    menu

data MenuOption = MenuOption { name :: String, function :: IO () }
menuOptions :: [MenuOption]
menuOptions = [MenuOption "Add a new todo" addTodo, MenuOption "Show all todos" showTodos, MenuOption "Remove a todo" removeTodo, MenuOption "Exit" exit]

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

getTodoSelection :: IO Todo
getTodoSelection = do
    number <- getNumberInput
    if number > length todos || number < 1
    then do
        putStrLn "Todo does not exist"
        getTodoSelection
    else return (todos !! (number - 1))

addTodo :: IO ()
addTodo = putStrLn "Adding a new task"

showTodos :: IO ()
showTodos = do
    if length todos > 0
    then do
        putStrLn "-----------------"
        putStrLn "Todos:"
        mapM_ putStrLn $ zipWith (\i todo -> "- " ++ show i ++ " " ++  title todo) [1..] todos
        putStrLn "-----------------"
    else putStrLn "No todos yet"

removeTodo :: IO ()
removeTodo = do
    showTodos
    putStrLn "Which todo do you want to remove?"
    removeTodo <- getTodoSelection
    putStrLn "test"

exit :: IO ()
exit = do
    putStrLn "Ok Bye Bye!"
    exitSuccess