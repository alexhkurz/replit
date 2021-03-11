-- automata0c2.hs
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

main = do
  putStr "delta (State 0) 'a' = " ; print (delta (State 0) 'a')
  putStr "delta (State 0) 'b' = " ; print (delta (State 0) 'b')
  putStr "delta (State 1) 'a' = " ; print (delta (State 1) 'a')
  putStr "delta (State 1) 'b' = " ; print (delta (State 1) 'b')
  putStr "delta (State 0) 'c' = " ; print (delta (State 0) 'c')
