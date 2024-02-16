import System.Console.Haskeline (getInputLine)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import Control.Monad.RWS (MonadState(put))
import Data.List(delete)

data Todo = Todo { title :: String, doUntil :: String } deriving (Eq)

main :: IO()
main = do
    putStrLn "Welcome to Todo-App"
    finalTodos <- menu []
    putStrLn "Goodbye!"

data MenuOption = MenuOption { name :: String, function :: [Todo] ->  IO [Todo] }
menuOptions :: [MenuOption]
menuOptions = [MenuOption "Add a new todo" addTodo, MenuOption "Show all todos" showTodos, MenuOption "Remove a todo" removeTodo, MenuOption "Exit" exit]

menu :: [Todo] -> IO () 
menu todos  = do   
    putStrLn "-----------------"
    printMenu
    menuSelection <- getMenuSelection
    putStrLn "-----------------"
    newTodos <- function (menuOptions !! (menuSelection - 1)) todos
    menu newTodos
    

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

getTodoSelection :: [Todo] -> IO Int
getTodoSelection todos = do
    number <- getNumberInput
    if number > length todos || number < 1
    then do
        putStrLn "Todo does not exist"
        getTodoSelection todos
    else return (number - 1)

addTodo :: [Todo] -> IO [Todo]
addTodo todos = do
    putStrLn "Title:"
    title <- getLine
    putStrLn "DoUntil:"
    doUntil <- getLine
    let newTodo = Todo {title = title, doUntil = doUntil}
    putStrLn "Todo added"
    return (newTodo : todos)

showTodos :: [Todo] -> IO [Todo] 
showTodos todos = do
    if length todos > 0
    then do
        putStrLn "Todos (index, title, doUntil):"
        mapM_ putStrLn $ zipWith (\i todo -> "- " ++ show i ++ ", " ++  title todo ++ ", " ++ doUntil todo) [1..] todos
    else putStrLn "No todos yet"
    return todos

removeTodo :: [Todo] -> IO [Todo]
removeTodo todos = do   
    if not (null todos) && length todos /= 0  
    then do
        showTodos todos
        putStrLn "-----------------"
        putStrLn "Which todo do you want to remove?"  
        removeTodoIndex <- getTodoSelection todos 
        putStrLn "The Todo got removed"
        return  (delete (todos !! removeTodoIndex) todos)
    else do
        putStrLn "There are no todos"
        return todos

exit :: [Todo] -> IO [Todo]
exit todos = do
    putStrLn "Ok Bye Bye!"
    exitSuccess