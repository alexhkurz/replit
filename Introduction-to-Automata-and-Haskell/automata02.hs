-- automata02.hs
newtype State = State Int

-- define the automaton
initial = State 0

final :: State -> Bool
final (State 1) = True
final (State 0) = False

delta :: State -> Char -> Maybe State
delta (State 0) 'a' =  Just (State 0)
delta (State 0) 'b' =  Just (State 1)
delta (State 1) 'a' =  Just (State 0)
delta (State 1) 'b' =  Just (State 1)
delta _ _ = Nothing

-- run the transition function on a word and a state
run :: (State -> Char -> Maybe State) -> State -> [Char] -> Maybe State
run delta q [] = Just q
run delta q (c:cs) = do
    q' <- delta q c
    run delta q' cs

-- search for the regexp represented by (initial,accept,delta) in a word
recognize :: [Char] -> Maybe Bool
recognize word = do
    q <- run delta initial word
    return (final q)

main = do
  print (recognize "abab")
  print (recognize "aba")
  print (recognize "abc")

