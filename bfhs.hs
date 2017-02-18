import System.Environment
import Data.List
import Data.Char

data Instruction = Inc
                 | Dec
                 | MoveRight
                 | MoveLeft
                 | OpenLoop
                 | LoopStart { end :: Int }
                 | LoopEnd { start :: Int }
                 | Input
                 | Output
                 deriving (Eq, Show)

parse' :: String -> [Instruction] -> [Int] -> [Instruction]
parse' "" insts [] = insts
parse' "" _ _ = error "Unmatched '['"
parse' ('+' : s) insts jumps = parse' s (insts ++ [Inc]) jumps
parse' ('-' : s) insts jumps = parse' s (insts ++ [Dec]) jumps
parse' ('>' : s) insts jumps = parse' s (insts ++ [MoveRight]) jumps
parse' ('<' : s) insts jumps = parse' s (insts ++ [MoveLeft]) jumps
parse' ('.' : s) insts jumps = parse' s (insts ++ [Input]) jumps
parse' (',' : s) insts jumps = parse' s (insts ++ [Output]) jumps
parse' ('[' : s) insts jumps = parse' s (insts ++ [OpenLoop]) (length insts : jumps)
parse' (']' : s) insts (curr : jumps) = let index = length insts
                                            before = take curr insts
                                            after = drop (curr + 1) insts
                                            replaceOpenLoop = before ++ (LoopStart index) : after
                                         in parse' s (replaceOpenLoop ++ [LoopEnd curr]) jumps
parse' (']' : _) _ [] = error "Unmatched ']'"
parse' (_ : s) insts jumps = parse' s insts jumps

parse :: String -> [Instruction]
parse s = parse' s [] []

data State = Init { program :: [Instruction] }
           | State { tape :: [Int],
                     dataPointer :: Int,
                     program :: [Instruction],
                     instructionPointer :: Int
                   }
           | Print State
           | Read State
           | Terminate deriving (Show)

step :: State -> State
step (Init program) = State [] 0 program 0
-- TODO

run :: State -> IO State
run Terminate = return Terminate

run (Print state) = do
                    let c = tape state !! dataPointer state
                    putChar $ chr c
                    return state

-- TODO
run (State _ _ _ _) = do
                        return Terminate


main = do
  args <- getArgs
  content <- readFile $ args !! 0

  let start = Init $ parse content
  print $ step start
