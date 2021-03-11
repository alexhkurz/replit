-- automata0d.hs
newtype State = State Int
  deriving Show
  
data Symbol = A | B

-- define the automaton
initial = State 0

final :: State -> Bool
final (State 1) = True
final (State 0) = False

delta :: State -> Symbol -> State
delta (State 0) A =  State 0
delta (State 0) B =  State 1
delta (State 1) A =  State 0
delta (State 1) B =  State 1

-- testing
main = do
  print $ delta (State 0) A
