import System.Console.Haskeline (getInputLine)
import Text.Read (readMaybe)
import System.Exit (exitSuccess)
import Control.Monad.RWS (MonadState(put))
import Data.IORef
import Data.List(delete)

data Todo = Todo { title :: String, doUntil :: String } deriving (Eq)

main :: IO()
main = do
    ref <- newIORef []
    putStrLn "Welcome to Todo-App"
    menu ref

data MenuOption = MenuOption { name :: String, function ::IORef [Todo] ->  IO () }
menuOptions :: [MenuOption]
menuOptions = [MenuOption "Add a new todo" addTodo, MenuOption "Show all todos" showTodos, MenuOption "Remove a todo" removeTodo, MenuOption "Exit" exit]

menu :: IORef [Todo] -> IO() 
menu ref  = do   
    putStrLn "-----------------"
    printMenu
    menuSelection <- getMenuSelection
    putStrLn "-----------------"
    function (menuOptions !! (menuSelection - 1)) ref
    menu ref

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

addTodo :: IORef [Todo] -> IO ()
addTodo ref = do
    todos <- readIORef ref
    putStrLn "Title:"
    title <- getLine
    putStrLn "DoUntil"
    doUntil <- getLine
    let newTodo = Todo {title = title, doUntil = doUntil}
    writeIORef ref (newTodo : todos)
    putStrLn "Todo added"

showTodos :: IORef [Todo] -> IO ()
showTodos ref = do
    todos <- readIORef ref
    if length todos > 0
    then do
        putStrLn "Todos (index, title, doUntil):"
        mapM_ putStrLn $ zipWith (\i todo -> "- " ++ show i ++ ", " ++  title todo ++ ", " ++ doUntil todo) [1..] todos
    else putStrLn "No todos yet"

removeTodo :: IORef [Todo] -> IO ()
removeTodo ref = do   
    todos <- readIORef ref
    if not (null todos) && length todos /= 0  
    then do
        showTodos ref
        putStrLn "-----------------"
        putStrLn "Which todo do you want to remove?"  
        removeTodoIndex <- getTodoSelection todos
        writeIORef ref (delete (todos !! removeTodoIndex) todos)
        putStrLn "The Todo got removed"
    else putStrLn "There are no todos"

exit :: IORef [Todo] -> IO ()
exit ref = do
    putStrLn "Ok Bye Bye!"
    exitSuccess