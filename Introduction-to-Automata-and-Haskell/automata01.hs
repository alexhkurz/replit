-- automata01.hs
newtype State = State Int

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

-- run the transition function on a word and a state
run :: (State -> Char -> State) -> State -> [Char] -> State
run delta q [] = q
run delta q (c:cs) = run delta (delta q c) cs 

-- the language of a state
semantics :: State -> ([Char] -> Bool)
semantics q w = final (run delta q w)

-- search for the regexp represented by (initial,accept,delta) in a word
recognize :: [Char] -> Bool
recognize word = semantics initial word

main = do
  print (recognize "abab")
  print (recognize "aba")
  print $ semantics (State 1) "abab"
