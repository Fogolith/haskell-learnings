import System.Environment
import System.Directory
import System.IO
import Data.List

type Command = String

dispatch :: [(Command, [String]) -> IO())]
dispath = [("add", add)
          ,("view", view
          ,("remove", remove)
          ]

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

add :: [String] -> IO ()
add [fileName, items] = return ()

view :: [String] -> IO ()
view _ = return ()

remove :: [String] -> IO ()
remove _ = return()
