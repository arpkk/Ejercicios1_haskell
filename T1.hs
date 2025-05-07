module T1 where
import Test.Hspec
import Test.QuickCheck


{-------------------------------------------}
{--------------  EJERCICIO 1  --------------}
{-------------------------------------------}

primosHastaN :: Int -> [Int]
primosHastaN n = eratostenes [2..n] where
  eratostenes [] = []
  eratostenes (x:xs)
    | not (null xs) && x^2 > last xs = (x:xs)
    | otherwise                      = x : eratostenes [y | y <- xs, y `mod` x /= 0]
{---------------------a---------------------}

divisoresPrimos :: Int -> [Int]
divisoresPrimos x = [y | y <- primosHastaN x, x `mod` y == 0]

{---------------------b---------------------}

exponente :: Int -> Int -> Int
exponente x y
  | y `notElem` divisoresPrimos x = 0
  | otherwise                     = contar x 0
  where
    contar :: Int -> Int -> Int
    contar n cont
      | n `mod` y /= 0 = cont
      | otherwise      = contar (n `div` y) (cont + 1)


{---------------------c---------------------}

factorizar :: Int -> [(Int,Int)]
factorizar x = [(y, exponente x y) | y <- divisoresPrimos x]

{---------------------d---------------------}

factorizaCorrectamente :: Int -> Property
factorizaCorrectamente x =
  x >= 2 ==> producto (factorizar x) == x
  where
    producto :: [(Int, Int)] -> Int
    producto = product . map (\(p, e) -> p ^ e)

{-------------------------------------------}
{--------------  EJERCICIO 2  --------------}
{-------------------------------------------}

{---------------------a---------------------}

summary :: Eq a => [a] -> [(a, Int)]
summary = foldr insertar [] where
    insertar x [] = [(x,1)]
    insertar x ((y,n):ys)
      | x == y    = (y, n+1) : ys
      | otherwise = (y, n) : insertar x ys

{---------------------b---------------------}

numVeces :: Eq a => a -> [a] -> Int
numVeces x = foldr (\y cont -> if y == x then cont + 1 else cont) 0

{---------------------c---------------------}

numVeces_doble :: Int -> [Int] -> Bool
numVeces_doble x xs = numVeces x (xs ++ xs) == 2 * numVeces x xs

numVeces_reverse :: Int -> [Int] -> Bool
numVeces_reverse x xs = numVeces x xs == numVeces x (reverse xs)

{---------------------d---------------------}

mapAlt :: (a -> a) -> (a -> a) -> [a] -> [a]
mapAlt _ _ [] = []
mapAlt f _ [x] = [f x]
mapAlt f g (x:y:xs) = f x : g y : mapAlt f g xs

{---------------------d---------------------}

subLenPred :: Int -> Int -> (a -> Bool) -> [a] -> Bool
subLenPred l i p xs =
  length sub >= l && all p sub
  where
    sub = take l (drop i xs)

{-------------------------------------------}
{--------------  EJERCICIO 3  --------------}
{-------------------------------------------}

{---------------------a---------------------}

applyUntil :: (a -> a) -> (a -> Bool) -> a -> a
applyUntil f p x
  | p x       = x
  | otherwise = applyUntil f p (f x)

{---------------------b---------------------}

leastPow2 :: Int -> Int
leastPow2 n
  | n >= 0 = applyUntil (^2) (>=n) 2
  | otherwise = error "Exception: argumento negativo"

{---------------------c---------------------}
 
bigOp :: (a -> a -> a) -> Int -> (Int -> a) -> a
bigOp p 1 f = f 1
bigOp p n f = p (bigOp p (n - 1) f) (f n)


{-------------------------------------------}
{-----------------  MAIN  ------------------}
{-------------------------------------------}

main :: IO ()
main = hspec $ do
  describe "factorizar" $ do
    it "recompone correctamente el número original" $
      property factorizaCorrectamente
    it "devuelve [(2,5),(3,1)] para 96" $
      factorizar 96 `shouldBe` [(2,5),(3,1)]

  describe "summary / numVeces" $ do
    it "duplicar la lista duplica las ocurrencias" $
      property numVeces_doble
    it "reverso de la lista conserva las ocurrencias" $
      property numVeces_reverse
