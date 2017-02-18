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

inc :: Tape -> Tape
inc (Tape left cursor right) = (Tape left (cursor + 1) right)

dec :: Tape -> Tape
dec (Tape left cursor right) = (Tape left (cursor - 1) right)


data State = Init { program :: [Instruction] }
           | Run { tape :: Tape,
                   program :: [Instruction],
                   instructionPointer :: Int
                 }
           | Print State
           | Read State
           | Terminate deriving (Show)

step :: State -> State
step (Init program) = Run (Tape [] 0 []) program 0
step (Run tape program ip) = if ip >= length program
                               then Terminate
                               else let instruction = program !! ip in
                                    case instruction of
                                       Inc -> Run (inc tape) program (ip + 1)
                                       Dec -> Run (dec tape) program (ip + 1)
                                       MoveRight -> Run (moveRight tape) program (ip + 1)
                                       MoveLeft -> Run (moveLeft tape) program (ip + 1)
                                       LoopStart end -> let jump = if cursor tape == 0
                                                                   then end + 1
                                                                   else ip + 1
                                                        in Run tape program jump
                                       LoopEnd start -> let jump = if cursor tape == 0
                                                                   then ip + 1
                                                                   else start + 1
                                                        in Run tape program jump
                                       _ -> Run tape program (ip + 1)

-- TODO

run :: State -> IO State
run Terminate = return Terminate

run (Print state) = do
                    let (Tape _ cursor _) = tape state
                    putChar $ chr cursor
                    return state

-- TODO
run (Run _ _ _) = do
                    return Terminate


main = do
  args <- getArgs
  content <- readFile $ args !! 0

  let start = Init $ parse content
  mapM print $ take 10 $ iterate step start
