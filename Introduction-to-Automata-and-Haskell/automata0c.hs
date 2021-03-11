-- automata0c.hs
newtype State = State Int
  deriving Show
  
-- define the automaton
initial = State 0

final :: State -> Bool
final (State 1) = True
final (State 0) = False

delta :: State -> Char -> State
delta (State 0) 'a' =  State 0
delta (State 0) 'b' =  State 1
delta (State 1) 'a' =  State 0
delta (State 1) 'b' =  State 1

-- testing
main = do
  print $ delta (State 0) 'a'
