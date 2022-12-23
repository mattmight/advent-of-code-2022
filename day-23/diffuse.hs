import qualified Data.Set as S
import qualified Data.Map as M


-- Aliases:
type Set a = S.Set a
type Map k a = M.Map k a

-- For holding x and y coordinates:
type XPos = Int
type YPos = Int

-- For the position of an elf:
type Pos = (XPos,YPos)

-- When proposing a move:
type ProposedMove = (Pos, Maybe Pos)

-- Directions in which an elf can move:
data Direction = North | South | East | West
  deriving (Show, Eq, Ord)

-- The direction to consider after this one:
nextDir :: Direction -> Direction
nextDir North = South
nextDir South = West
nextDir West = East
nextDir East = North

-- Note: This order applies within a round as an elf tries to move
--       and after the round, when the next lead direction is picked.


-- Create the order in which to consider the directions within a round:
dirCycleFrom :: Direction -> [Direction]
dirCycleFrom start = [start, 
                      nextDir start,
                      nextDir $ nextDir $ start,
                      nextDir $ nextDir $ nextDir $ start]

-- Generate all positions surrounding this elf:
surrounding (x, y) = 
 [ (x+i, y+j) | i <- [-1,0,1], j <- [-1,0,1], (i,j) /= (0,0) ]

-- Check if any position in the list is already in the provided set:
collidesWith :: [Pos] -> Set Pos -> Bool
collidesWith points pointset = any (\ p -> p `S.member` pointset) points

-- Generate the northern, southern, eastern and western borders of an elf:
border :: Direction -> Pos -> [Pos]
border North (x, y) = [ (x+1,y+1), (x,y+1), (x-1,y+1) ]
border South (x, y) = [ (x+1,y-1), (x,y-1), (x-1,y-1) ]
border East  (x, y) = [ (x+1,y+1), (x+1,y), (x+1,y-1) ]
border West  (x, y) = [ (x-1,y+1), (x-1,y), (x-1,y-1) ]

-- The new position of the elf in the given direction:
move :: Direction -> Pos -> Pos
move North (x,y) = (x,  y+1)
move South (x,y) = (x,  y-1)
move East  (x,y) = (x+1,y  )
move West  (x,y) = (x-1,y  )


-- Try to move the elf (if needed):
tryMoves :: [Direction] -> Pos -> Set Pos -> Maybe Pos

-- Don't even try to move if there's no need to move:
tryMoves _ pos elves | not (surrounding pos `collidesWith` elves) = Nothing

-- Try moving in the provided order of directions:
tryMoves [] pos elves = Nothing
tryMoves (dir:rest) pos elves = 
   if (border dir pos) `collidesWith` elves 
   then tryMoves rest pos elves
   else Just (move dir pos)

-- Generate propsoed moves based on the provided order of directions:
generateProposals :: [Direction] -> [Pos] -> Set Pos -> [ProposedMove]
generateProposals dircycle poses elves = map (\ pos -> (pos, tryMoves dircycle pos elves)) poses

-- Count how many elves have proposed moving to each position,
-- so that we can block elves from moving if more than one wants that position.
sumProposals :: [ProposedMove] -> Map Pos Int
sumProposals []                   = M.empty
sumProposals ((_,Nothing):rest)   = sumProposals rest
sumProposals ((_,Just pos):rest)  = M.insertWith (\ a b -> a + b) pos 1 (sumProposals rest)

-- Checks a proposed move against the number of proposed moves to that space,
-- cutting off the proposed move if another elf has also proposed it.
pruneProposal :: Map Pos Int -> ProposedMove -> ProposedMove
pruneProposal propCount (curr,Just pos) | (M.findWithDefault 0 pos propCount) <= 1 = (curr,Just pos)
pruneProposal propCount (curr,_) = (curr,Nothing)

-- Execute the proposed move -- or stay in the same place:
nextPos :: ProposedMove -> Pos
nextPos (curr,Nothing)      = curr
nextPos (_   ,Just new_pos) = new_pos

-- Find the minimum bounding rectangle that encloses all elves:
dimensions :: Set Pos -> (XPos,YPos,XPos,YPos)
dimensions elves =
 S.foldr' (\ (x,y) (xmin,ymin, xmax,ymax) ->
             (min x xmin, min y ymin, max x xmax, max y ymax))
          (0,0,0,0)
          elves

-- Render the character at a given position -- empty (.) or elf (#):
renderChar :: Set Pos -> Pos -> Char
renderChar elves pos = 
  if pos `S.member` elves then '#' else '.'

-- Render a line of positions at a given y:
renderLine :: Set Pos -> YPos -> XPos -> XPos -> String
renderLine elves y xmin xmax | xmin <= xmax 
  = (renderChar elves (xmin,y)):(renderLine elves y (xmin+1) (xmax))
renderLine _ _ _ _ = []  

-- Render all of the lines in the rectangle, top down:
renderLines :: Set Pos -> (XPos,YPos,XPos,YPos) -> String
renderLines elves (xmin,ymin, xmax,ymax)  | ymax >= ymin
  = (renderLine elves ymax xmin xmax) ++ 
    ('\n':(renderLines elves (xmin,ymin, xmax,ymax-1)))
renderLines _ _ = []

-- Render a state:
renderState :: State -> String
renderState (State _ elves) = renderLines elves (dimensions elves) 

-- Count the number of ground tiles in a bounding rectangle:
countGrounds :: State -> Int
countGrounds (State _ elves) = 
  let (xmin,ymin,   xmax,ymax) = dimensions elves
      elf_count = S.size elves
   in ((xmax - xmin + 1)*(ymax-ymin + 1) - elf_count)

-- A state is both the next leading direction and the position of all elves:
data State = State { next_dir :: Direction , elves :: Set Pos }
  deriving (Eq,Show,Ord)

-- A state to state transition function:
next :: State -> State
next (State dir elves) = 
  let proposed_moves = generateProposals (dirCycleFrom dir) (S.toList elves) elves
      sum = sumProposals proposed_moves
      pruned_proposals = map (pruneProposal sum) proposed_moves
      new_positions = map nextPos pruned_proposals
      elves' = S.fromList new_positions
   in State (nextDir dir) elves' 

-- Find the nth state:
next_n :: Int -> State -> State
next_n 0 state = state
next_n n state = next_n (n-1) (next state)

-- Find the first stable state, in which no elf tries to move:
next_stable :: Int -> State -> (Int,State)
next_stable n state = 
 let state' = next state
  in if (renderState state) == (renderState state')
     then (n,state)
     else next_stable (n+1) state'


-- Parse a string into set of elf positions:
parseLine :: YPos -> XPos -> String -> Set Pos
parseLine y x []       = S.empty
parseLine y x ('#':tl) = S.insert (x,y) (parseLine y (x+1) tl)
parseLine y x ('.':tl) = parseLine y (x+1) tl

parseLines' :: YPos -> [String] -> Set Pos
parseLines' y []      = S.empty
parseLines' y (hd:tl) = (parseLine y 0 hd) `S.union` (parseLines' (y+1) tl)

-- Parse a list of strings into elf positions:
parseLines :: [String] -> Set Pos
parseLines lines = parseLines' 0 (reverse lines)



main :: IO ()
main = do

 -- Read in the input:
 input <- readFile "input.txt"
 let inputLines = lines input
 let initElves = parseLines inputLines

 -- Create the initial state:
 let state0 = State North initElves

 let n = 10
 let state_n = next_n n state0

 putStrLn ("Part 1, ground tiles: " ++ (show (countGrounds state_n)))

 -- Part 2

 let (steps,final) = next_stable 1 state0
 
 putStrLn ("Part 2: " ++ (show steps) ++ " is the first round without moves.")

