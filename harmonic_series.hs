-- the list [f 1, f 2, ..., f n]
list_seq 0 _ = []
list_seq n f = list_seq (n-1) f ++ [f n]

-- the sequence of harmonic numbers
oneover n = 1/n

-- the harmonic series
harmonic n = sum (list_seq n oneover)

-- the length of the nth piece
seqone 1 = 2
seqone n = (n-1) + seqone (n-1)

-- the end of the n-th piece
seqend 1 = 2
seqend n = seqone (n-1) + seqend (n-1)

--make a list from [f n, ... f m]
make_list f n m = [ f k | k <-[n..m]]

-- the n-th piece
piece n = make_list oneover (1+seqend (n)) (seqend (n+1))

-- the sum of the n-th piece

-- seqsum n = sum (list_seq n )

main = do
  let n = 5
  putStr "n: " ; print $ n
  putStr "list_seq n oneover: " ; print $ list_seq n oneover 
  putStr "harmonic n: " ; print $ list_seq n harmonic 
  putStr "list_seq n seqone: " ; print $ list_seq n (seqone)
  putStr "list_seq n seqend: " ; print $ list_seq n (seqend)
  putStr "make_list oneover 2 4: " ; print $ make_list oneover 2 4
  putStr "piece 1: " ; print $ piece 1
  putStr "piece 2: " ; print $ piece 2
  putStr "piece 3: " ; print $ piece 3
  putStr "[ sum (piece n) | n <- [1..10]]: " ; print $ [ sum (piece n) | n <- [1..10]]
  
