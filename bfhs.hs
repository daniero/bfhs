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


data Tape = Tape { left :: [Int], cursor :: Int, right :: [Int] }
          deriving (Show)

moveRight :: Tape -> Tape
moveRight (Tape left cursor (next : right)) = (Tape (left ++ [cursor]) next right)
moveRight (Tape left cursor []) = (Tape (left ++ [cursor]) 0 [])

moveLeft :: Tape -> Tape
moveLeft (Tape [] cursor right) = (Tape [] 0 (cursor : right))
moveLeft (Tape left cursor right) = (Tape (init left) (last left) (cursor : right))


data State = Init { program :: [Instruction] }
           | State { tape :: Tape,
                     program :: [Instruction],
                     instructionPointer :: Int
                   }
           | Print State
           | Read State
           | Terminate deriving (Show)

step :: State -> State
step (Init program) = State (Tape [] 0 []) program 0
-- TODO

run :: State -> IO State
run Terminate = return Terminate

run (Print state) = do
                    let (Tape _ cursor _) = tape state
                    putChar $ chr cursor
                    return state

-- TODO
run (State _ _ _) = do
                    return Terminate


main = do
  args <- getArgs
  content <- readFile $ args !! 0

  let start = Init $ parse content
  print $ step start

  let tape = Tape [1,2,3] 4 [5,6,7]
  print $ tape
  print $ moveRight $ moveRight $ moveRight $ moveRight $ moveRight $ moveRight $ moveRight tape
  print $ moveLeft $ moveLeft $ moveLeft tape
