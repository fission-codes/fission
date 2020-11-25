module Demo where

import Prelude

add :: Int -> Int -> Int
add x y = x + y

subtract x y = x - y

foo = add 10 . subtract 50
foo' x  = add 10 (subtract 50 x)
foo'' x  = add 10 $ subtract 50 x

foo = fmap (add 10 . subtract 6) [1,2,3]
foo = fmap (\x -> add 10 $ subtract 6 x) [1,2,3]
foo = fmap add10 [1,2,3]

bar = add10 <$> [1,2,3]
bar' = add10  $  1


class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap fun [] = []
  fmap fun (x : xs) = fun x : fmap fun xs

-- fmap f . fmap g = fmap (f . g)


fun list

data Maybe a = Nothing | Just a

(* 10) <$> maybeInt

(* 10) <$> Nothing -> Nothing
(* 10) <$> Just 5  -> Just 50

(*) <$> Just 5 <*> Just 6 = Just 30
(*) <$> Nothing <*> Just 6 = Nothing



data Either a b = Left a | Right b
               -- ^^^^^ err ^^^^^^ success




x <$> y = fmap x y


add10 :: Int -> Int
add10 = add (double constant)
  where
    constant = 10 + foo
    foo = 5

    double :: Int -> Int
    double = (* 2)


concat :: String -> String -> String
concat x y = x ++ y

concatAny :: Monoid a => a -> a -> a
concatAny x y = x <> y

foo = x `concatAny` y

class Monoid a where
  mempty :: a
  x <> y :: a -> a -> a

-- | This is theidentity function
id :: a -> a -- here
-- id x = x

type Email = String -- alias

data Book = Book Text Int

data Book' = Book'
  { authorName    :: Text
  , yearPublished :: Int
  }

pure :: Applicative f => a -> f a
pure = return

list :: [Int]
list = pure 1

dunno :: Maybe Int
dunno = pure 1

foo :: JSON -> Maybe Book'
foo json = do
  authorName    <- extractAuthor json
  yearPublished <- extractYear json
  pure Book' {..}

bar = do
  first  <- return [1,2,3]
  second <- return $ [4,5,6, first]

  if isOdd first
    then return first
    else return second * 2


foundation = Book' { authorName = "Asimov", yearPublished = 1952 }

isAsimov = authorName foundation == "Asimov"

foundation' = foundation {yearPublished = 1954}

foo :: Book' -> Int
foo Book' {yearPublished, authorName} = yearPublished + length authorName

data Light
  = Red
  | Yellow
  | Green

canDrive :: Light -> Bool
canDrive Red = False
canDrive Yellow = False
canDrive Green = True

canDrive :: Light -> CanDrive
canDrive Green = CanDrive True
canDrive _     = CanDrive False

foo = x |> \case
    Yellow -> bar
    Red -> baz
    Green -> quux

newtype CanDrive = CanDrive { toBool :: Bool }

flag = CanDrive True

baz = toBool flag
