{- https://hackage.haskell.org/package/base-4.15.0.0/docs/src/GHC-Base.html
-- https://stackoverflow.com/questions/6948166/javas-interface-and-haskells-type-class-differences-and-similarities
-- https://stackoverflow.com/a/8123973/4600290
-- Phil Wadler https://www.youtube.com/watch?v=8frGknO8rIg

class Semigroup a where
    (<>) :: a -> a -> a

class Semigroup a => Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
    mappend = (<>)
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty

-}

-- We define our own lists in order to keep them separate
-- from Haskell's built-in lists
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- List as an instance of semigroup
instance Semigroup (List a) where
  Nil <> xs = xs
  (Cons y ys) <> xs = Cons y (ys <> xs)

-- List as an instance of monoid
instance Monoid (List a) where
  mempty = Nil

main = do
  let l123 = (Cons 1 (Cons 2 (Cons 3 Nil)))
  let l456 = (Cons 4(Cons 5 (Cons 6 Nil)))
  let l789 = (Cons 7(Cons 8 (Cons 9 Nil)))
  print $ l123
  print $ mappend l123 l789
  print $ mconcat [l123,l456,l789]
