{-
class Semigroup a where
    (<>) :: a -> a -> a

class Semigroup a => Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mappend = (<>)
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
-}

instance Semigroup Integer where
  n <> m = n * m

instance Monoid Integer where
  mempty = 1

main = do
  print $ (mempty :: Integer)
  print $ 2 <> 3
  print $ mappend 3 4
  print $ mconcat [1,2,3,4]
