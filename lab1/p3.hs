type IntSet = (Int -> Bool)

isMember :: IntSet -> Int -> Bool
isMember f x = f x

-- part A
emptySet :: IntSet
emptySet x = False

allSet :: IntSet
allSet x = True

-- part B
interval :: Int -> Int -> IntSet
interval lBound uBound =
  let f x = x `elem` [lBound .. uBound]
  in
    f

-- part C
-- euclidean algorith to find the greatest common divisor of two numbers
-- expects two positive numbers
euclid :: Int -> Int -> Int
euclid 0 b = b
euclid a 0 = a
euclid 1 b = 1
euclid a 1 = 1
euclid a b
  | a > b = euclid (a `mod` b) b
  | otherwise = euclid a (b `mod` a)
    

primes_helper k x
  | x >= k = []
  | gcd == 1 = primes_helper k (x + 1)
  | otherwise = [gcd] ++ primes_helper k (x + 1)
  where
    gcd = euclid k x

primes k
  | k >= 0 = primes_helper k 2
  | otherwise = primes_helper (-k) 2

-- part D
setIntersection :: IntSet -> IntSet -> IntSet
setIntersection f g x = f x && g x

setUnion :: IntSet -> IntSet -> IntSet
setUnion f g x = f x || g x

setComplement :: IntSet -> IntSet
setComplement f x = not (f x)

addToSet :: Int -> IntSet -> IntSet
addToSet x f y = f x || x == y

deleteFromSet :: Int -> IntSet -> IntSet
deleteFromSet x f y = f x && x /= y

-- Driver
main =
  putStrLn (show(primes (-192)))
