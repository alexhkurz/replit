-- automata04.hs
newtype State = State Int
  deriving (Show, Eq)

-- define the automaton
-- recognize all strings that have at least two 'a' and finish on 'a'
initial = State 0

final :: State -> Bool
final (State 0) = False
final (State 1) = False
final (State 2) = True

delta :: State -> Char -> [State]
delta (State 0) 'a' =  [State 0, State 1]
delta (State 0) _ =  [State 0]
delta (State 1) 'a' =  [State 1,State 2]
delta (State 1) _ =  [State 1]
delta (State 2) _ =  []

-- run the transition function on a word and a state
run :: (State -> Char -> [State]) -> State -> [Char] -> [State]
run delta q [] = return q
run delta q (c:cs) = do
  next <- delta q c 
  run delta next cs 

-- take the disjunction of a list of Booleans
disjunction :: [Bool] -> Bool
disjunction [] = False
disjunction (p:ps) = p || (disjunction ps)

-- the language of a state
semantics :: State -> ([Char] -> Bool)
semantics q w = disjunction (map final (run delta q w))

-- search for the regexp represented by (initial,accept,delta) in a word
recognize :: [Char] -> Bool
recognize word = semantics initial word

--for testing 
--from https://stackoverflow.com/a/16108856/4600290
--https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-List.html#v:foldl 
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\ seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

main = do
  let word = "abba" -- change for testing
  print $ recognize word
  print $ removeDuplicates $ run delta (State 0) word
