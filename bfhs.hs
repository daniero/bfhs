import System.Environment
import Data.List
import Data.Char

data State = Init { program :: String }
           | State { tape :: [Int],
                     dataPointer :: Int,
                     program :: String,
                     instructionPointer :: Int,
                     jumpList :: [Int]
                   }
           | Print State
           | Read State
           | Terminate deriving (Show)

step :: State -> State
step (Init program) = State [] 0 program 0 []
-- TODO

run :: State -> IO State
run Terminate = return Terminate

run (Print state) = do
                    let c = tape state !! dataPointer state
                    putChar $ chr c
                    return state

-- TODO
run (State _ _ _ _ _) = do
                        return Terminate


main = do
  args <- getArgs
  content <- readFile $ args !! 0

  let start = Init content
  print $ step start
