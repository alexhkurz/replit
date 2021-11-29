-- the sequence of harmonic numbers
oneover n = 1/n

-- the harmonic series
harmonic n = sum [oneover i | i <- [1..n]]

-- the length of the nth piece
piece_length 1 = 2
piece_length n = (n-1) + piece_length (n-1)

-- the end of the n-th piece
piece_end 1 = 2
piece_end n = piece_length (n-1) + piece_end (n-1)

-- the n-th piece
piece n = [oneover i | i <- [(1+ piece_end n)..(piece_end (n+1))]]

main = do
  let end = 5
  print end
  putStrLn "---------------"
  putStr "oneover n: " ; print $ [oneover n | n <- [1..end]]
  putStr "harmonic n: " ; print $ [harmonic n | n <- [1..end]]
  putStrLn "---------------"
  putStr "piece_length n: " ; print $ [piece_length n | n <- [1..end]]
  putStr "piece_end: " ; print $ [piece_end n | n <- [1..end]]
  putStr "piece 1: " ; print $ piece 1
  putStr "piece 2: " ; print $ piece 2
  putStr "piece 3: " ; print $ piece 3
  putStrLn "---------------"
  putStrLn "[ sum (piece n) | n <- [1..10]]: " ; print $ [ sum (piece n) | n <- [1..10]]
  putStrLn "---------------"

