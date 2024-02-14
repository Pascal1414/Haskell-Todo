data Todo = Todo { id :: Int, title :: String, doUntil :: String }

main :: IO()
main = do
    putStrLn "Welcome to Todo-App"
    getMenuSelection


menuOptions :: [String]
menuOptions = [
    "Print Todos",
    "Add Todo",
    "Remove Todo"
]

getMenuSelection :: IO ()
getMenuSelection = do
    putStrLn "What do you want to do?"
    
