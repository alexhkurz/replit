-- automata0b.hs
-- define an automaton
initial = 0

final :: Int -> Bool
final 1 = True
final 0 = False

delta :: Int -> Char -> Int
delta  0 'a' = 0
delta  0 'b' = 1
delta  1 'a' = 0
delta  1 'b' = 1

-- testing
main = do
  print $ delta 0 'a'
