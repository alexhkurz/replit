-- automata03.hs
newtype State = State Int

-- define the automaton that finds "ab" 
initial = State 0

final :: State -> Bool
final (State 2) = True
final (State _) = False

delta :: State -> Char -> State
delta (State 0) 'a' =  State 1 -- with 'a' start the pattern
delta (State 0) _ = State 0 -- else stay in the prefix
delta (State 1) 'b' =  State 2 -- with 'b' move to the final state
delta (State 1) _ =  State 0 -- else go back to 0
delta _ _ = State 3 -- else go to/stay in the suffix

-- run the transition function on a word and a state
run :: (State -> Char -> State) -> State -> [Char] -> (Bool,Int) -> (Bool,Int)
run delta q [] (found,position) = (found,position)
run delta q (c:cs) (found,position) = 
    case (found, final (delta q c)) of
        (True, _) -> run delta (delta q c) cs (True, position)
        (False, True) -> run delta (delta q c) cs (True,position)
        (False, False) -> run delta (delta q c) cs (False,position+1)

-- search for the regexp represented by (initial,accept,delta) in a word
search_in :: [Char] -> IO()
search_in word = case run delta initial word (False,1) of 
    (True, n) -> do putStr "Pattern found ending at position " ;  putStr (show n) ; putStr " in " ; print word 
    (False, _) -> do putStr "Pattern not found in " ; print word
    
main = do
  search_in ""
  search_in "a"
  search_in "ab"
  search_in "abab"
  search_in "cacbabc"

