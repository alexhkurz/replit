--arithmetic_OnePlus.hs
data Exp = One | Add Exp Exp 
    deriving (Show, Eq)

-- normalise expressions 
norm :: Exp -> Exp
norm One = One
norm (Add e1 (Add e2 e3)) = norm (Add (norm (Add (norm e1) (norm e2))) (norm e3))
norm  e = e 

-- evaluate expressions
eval :: Exp -> Integer
eval One = 1
eval (Add e1 e2) = (eval e1) + (eval e2)

main = do 
  let one = One
  let two = Add one one
  let three = Add two one
  let four = Add three one
  let five = Add four one
  let six = Add five one
  let test = Add two three

  putStrLn $ "Test with "++ show(test)
  putStrLn " "
  putStrLn "normalise:"
  print $ norm $ test
  putStrLn " "
  putStrLn "evaluate:"
  print $ eval $ test
