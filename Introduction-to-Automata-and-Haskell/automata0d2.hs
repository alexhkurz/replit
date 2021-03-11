-- automata0d2.hs
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

main = do
  putStr "delta (State 0) A = " ; print (delta (State 0) A)
  putStr "delta (State 0) B = " ; print (delta (State 0) B)
  putStr "delta (State 1) A = " ; print (delta (State 1) A)
  putStr "delta (State 1) B = " ; print (delta (State 1) B)
  putStr "delta (State 0) C = " ; print (delta (State 0) C)
