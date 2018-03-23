{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Data.List
import           Data.Semigroup
import           System.Random  (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- 1.1
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (x, y, z) = let [a, b, c] = sort [x, y, z] in (a, b, c)

-- 1.2
highestBit :: Int -> Int
highestBit = fst . highestBitH

highestBitH :: Int -> (Int, Int)
highestBitH x = let k = unpow x in (2 ^ k, k)
  where
    unpow :: Int -> Int
    unpow y
          | y <= 1    = 0
          | otherwise = unpow (div y 2) + 1

-- 1.3
smartReplicate :: [Int] -> [Int]
smartReplicate = foldMap (\x -> replicate x x)

-- 1.4
contains :: Eq a => a -> [[a]] -> [[a]]
contains a = filter (elem a)

-- 2.1
removeAt :: Int -> [a] -> [a]
removeAt i (x:xs)
         | i > 0     = x : removeAt (i - 1) xs
         | i == 0    = xs
         | otherwise = x:xs
removeAt _ [] = []

-- 2.2
collectEvery :: Int -> [a] -> ([a], [a])
collectEvery a b = collectEveryK a a b [] []
  where
    collectEveryK :: Int -> Int -> [a] -> [a] -> [a]-> ([a], [a])
    collectEveryK 1 k (x:xs) y z = collectEveryK k k xs y (z ++ [x])
    collectEveryK n k (x:xs) y z = collectEveryK (n - 1) k xs (y ++ [x]) z
    collectEveryK _ _ _ y z      = (y, z)

-- 2.3
stringSum :: String -> Int
stringSum s = sum (map read (words s))

-- 2.4
mergeSort :: Ord a => [a] -> [a]
mergeSort [x] = [x]
mergeSort xs = let (a, b) = splitAt (div (length xs) 2) xs in merge (mergeSort a) (mergeSort b)
  where
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] b = b
    merge a [] = a
    merge (a:as) (b:bs) = if a < b then a : merge as (b : bs) else b : merge (a : as) bs

-- 3.1
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Eq, Ord, Enum, Show, Read)

nextDay :: Day -> Day
nextDay Sun = Mon
nextDay d   = succ d

afterDays :: Day -> Int -> Day
afterDays d 0 = d
afterDays d n = afterDays (nextDay d) (rem (n - 1) 7)

isWeekend :: Day -> Bool
isWeekend = (> Fri)

daysToParty :: Day -> Int
daysToParty Fri = 0
daysToParty d   = daysToParty (nextDay d) + 1

-- 3.2
data Fighter
  = Knight { name :: String, lvl :: Integer, maxHealth :: Integer -> Integer, health :: Integer, damage :: Integer -> Integer }
  | Monster { name :: String, lvl :: Integer, health :: Integer, damage :: Integer -> Integer }

hit :: Fighter -> Integer -> Maybe Fighter
hit (Knight n l mh h d) dm = if h - dm > 0 then Just (Knight n l mh (h - dm) d) else Nothing
hit (Monster n l h d) dm = if h - dm > 0 then Just (Monster n l (h - dm) d) else Nothing

lvlUp :: Fighter -> Fighter
lvlUp (Knight n l mh h d) = Knight n (l + 1) mh h d
lvlUp m                   = m

fightR :: Fighter -> Fighter -> Integer -> (Fighter, Integer)
fightR f1 f2 n = case hit f2 (damage f1 (lvl f1)) of
                    Nothing -> (lvlUp f1, n)
                    Just k  -> fightR k f1 (n + 1)

fight :: Fighter -> Fighter -> (Fighter, String)
fight m@Monster{} k@Knight{} = let (a, b) = fightR k m 0 in (a, "Turns: " ++ show b)
fight f1 f2 = let (a, b) = fightR f1 f2 0 in (a, "Turns: " ++ show b)

relax :: Fighter -> Fighter
relax (Knight n l mh _ d) = Knight n l mh (mh l) d
relax m                   = m

alfred = Knight "Alfred" 1 (\x -> 20 * x + 280) 300 (\x -> 5 * x + 30)
garen = Knight "Garen" 1 (\x -> 15 * x + 235) 250 (\x -> 10 * x + 30)
bug = Monster "Bug" 1 100 const 50
dragon = Monster "Dragon" 15 500 const 50

instance Show Fighter
  where
    show (Knight n l mh h d) = n ++ ", lvl: " ++ show l ++ ", health: " ++ show h ++ "/" ++ show (mh l) ++ ", damage: " ++ show (d l)
    show (Monster n l h d) = n ++ ", lvl: " ++ show l ++ ", health: " ++ show h ++ ", damage: " ++ show (d l)

-- 3.3
data Vector a = Vector2D a a | Vector3D a a a deriving (Eq, Show, Read)

packVector :: Num a => Vector a -> [a]
packVector (Vector2D x y)   = [x, y, 0]
packVector (Vector3D x y z) = [x, y, z]

unpackVector :: (Eq a, Num a) => [a] -> Vector a
unpackVector [x, y]      = Vector2D x y
unpackVector [x, y, z]
             | z == 0    = Vector2D x y
             | otherwise = Vector3D x y z
unpackVector _           = Vector2D 0 0

zipVectorsWith :: (Eq a, Num a) => (a -> a -> a) -> Vector a -> Vector a -> Vector a
zipVectorsWith f v1 v2 = unpackVector $ zipWith f (packVector v1) (packVector v2)

vectorLength :: Vector Double -> Double
vectorLength = sqrt . sum . map (\x -> x * x) . packVector

sumOfVectors :: (Eq a, Num a) => Vector a -> Vector a -> Vector a
sumOfVectors = zipVectorsWith (+)

scalarProduct :: (Eq a, Num a) => Vector a -> Vector a -> a
scalarProduct v1 v2 = sum $ packVector (zipVectorsWith (*) v1 v2)

distanceBetweenVectors :: Vector Double -> Vector Double -> Double
distanceBetweenVectors v1 v2 = vectorLength (zipVectorsWith (-) v1 v2)

vectorProduct :: (Eq a, Num a) => Vector a -> Vector a -> Vector a
vectorProduct v1 v2 = let [x1, y1, z1] = packVector v1
                          [x2, y2, z2] = packVector v2
                          in unpackVector [z1 * y2 - y1 * z2, x1 * z2 - z1 * x2, y1 * x2 - y2 * x1]

-- 3.4
data Nat = Z | S Nat deriving Show

natToInteger :: Nat -> Integer
natToInteger (S n) = natToInteger n + 1
natToInteger Z     = 0

instance Eq Nat
  where
    (S a) == (S b) = a == b
    Z == Z = True
    _ == _ = False

instance Ord Nat
  where
    compare (S a) (S b) = compare a b
    compare (S _) Z     = GT
    compare Z (S _)     = LT
    compare Z Z         = EQ

instance Num Nat
  where
    (S a) + (S b) = S (S (a + b))
    (S n) + Z = S n
    Z + (S n) = S n
    Z + Z = Z

    (S a) - (S b) = a - b
    Z - _ = Z
    n - Z = n

    (S a) * b = (a * b) + b
    Z * _ = Z

    negate _ = Z

    abs a = a

    signum (S _) = S Z
    signum Z     = Z

    fromInteger z
                | z <= 0    = Z
                | otherwise = S (fromInteger (z - 1))

-- 3.5
data Tree a = Leaf | Node a (Tree a) (Tree a)

isEmpty :: Ord a => Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Ord a => Tree a -> Integer
size Leaf             = 0
size (Node _ ch1 ch2) = size ch1 + size ch2 + 1

findInTree :: Ord a => Tree a -> a -> Bool
findInTree Leaf _ = False
findInTree (Node e ch1 ch2) n = case compare e n of
                               EQ -> True
                               GT -> findInTree ch1 n
                               LT -> findInTree ch2 n

insertInTree :: Ord a => Tree a -> a -> Tree a
insertInTree Leaf a = Node a Leaf Leaf
insertInTree n@(Node e ch1 ch2) a = case compare e a of
                                    EQ -> n
                                    GT -> Node e (insertInTree ch1 a) ch2
                                    LT -> Node e ch1 (insertInTree ch2 a)

fromList :: Ord a => [a] -> Tree a
fromList (x:xs) = insertInTree (fromList xs) x
fromList []     = Leaf

toList :: Ord a => Tree a -> [a]
toList Leaf             = []
toList (Node e ch1 ch2) = toList ch1 ++ [e] ++ toList ch2

-- 4.1
instance Foldable Tree
  where
    foldr _ z Leaf             = z
    foldr f z (Node e ch1 ch2) = foldr f (f e (foldr f z ch2)) ch1

    foldMap _ Leaf = mempty
    foldMap f (Node e ch1 ch2) = mappend (mappend (foldMap f ch1) (f e)) (foldMap f ch2)

-- 4.2
splitOn :: forall a . Eq a => a -> [a] -> [[a]]
splitOn c = foldr f [[]]
  where
    f :: Eq a => a -> [[a]] -> [[a]]
    f a b = if a == c
            then [] : b
            else (a : head b) : tail b

joinWith :: a -> [[a]] -> [a]
joinWith n = foldMap (++ [n])

-- 5.1
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldMap f
  where
    f :: Maybe [a] -> [a]
    f (Just x) = x
    f Nothing  = []

-- 5.2
data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a)
  where
    (a :| as) <> (b :| bs) = a :| (as <> (b : bs))

newtype Identity a = Identity { runIdentity :: a }

instance Semigroup a => Semigroup (Identity a)
  where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a)
  where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y)

-- 5.3
instance Ord a => Semigroup (Tree a)
  where
    t <> (Node e ch1 ch2) = insertInTree t e <> ch1 <> ch2
    t <> Leaf = t

instance Ord a => Monoid (Tree a)
  where
    mempty = Leaf

    mappend t (Node e ch1 ch2) = mappend (mappend (insertInTree t e) ch1) ch2
    mappend t Leaf             = t
