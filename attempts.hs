
import Control.Monad.Writer
import Data.Ratio

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do tell ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell [show x]



newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

-- fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

--instance Monad Prob where
--    return x = Prob [(x,1%1)]
--    m >>= f = flatten (fmap f m)
--    fail _ = Prob []

undefined_bottom :: a
undefined_bottom = undefined_bottom

newtype ClockwisePairEnum a b = ClockwisePairEnum { getPair :: (a,b) } 
--    deriving Show

instance (Enum a, Enum b) => Enum (ClockwisePairEnum a b) where

    fromEnum (ClockwisePairEnum (a, b)) = clockwise (fromEnum a) (fromEnum b)
        where 
            clockwise_recursive 0 0 = 0
            clockwise_recursive 0 b_index = 1 + clockwise (b_index-1) 0 
            clockwise_recursive a_index 0 = a_index + clockwise 0 a_index 
            -- previous equations yield the following closed formula:
            -- clockwise_recursive 0 a_index = div (a_index * (a_index + 1)) 2
            clockwise_recursive a_index b_index = a_index + clockwise 0 (a_index + b_index)

            clockwise a_index b_index = 
                let sum index = div (index * (index + 1)) 2
                in a_index + sum (a_index + b_index)
    
    toEnum 0 = ClockwisePairEnum (toEnum 0, toEnum 0)
    toEnum x =  let (ps, n:_) = break (\a -> x < a) cumulate 
                    p = last ps
                    a_index = x - p
                    b_index = n - x - 1
                in  ClockwisePairEnum (toEnum a_index, toEnum b_index)
        where cumulate = scanl (+) 0 [1,2..]

instance (Show a, Show b) => Show (ClockwisePairEnum a b) where
    
    showsPrec index (ClockwisePairEnum (x, y)) = \existing ->
        let representation = undefined
        in representation ++ existing           

fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

g n = (fibonacci (n+1), fibonacci n)

fibonacci' n = x + y
    where (x,y) = g (n-2)

-- remove every reference to function `fibonacci` within function `g`
g 0 = (fibonacci 1, fibonacci 0) = (1, 1)
g n = (x + y, fibonacci n) = (x + y, x)
    where (x,y) = (fibonacci n, fibonacci (n-1)) = g (n-1)

-- therefore the final implementation of function `g` is the following:
g 0 = (1, 1)
g n = (x + y, x) where (x,y) = g (n-1)

------------------------------------------------------------------------

prodsum x = prod x + sum x

prod 0 = 1
prod n = n * prod (n-1)

sum 0 = 0
sum m = m + sum (m-1)

p n = (prod n, sum n)
--p n = (prodsum (n+1), prodsum n)

prodsum' n = n * x + (n + y) where (x, y) = p (n-1)

-- looking at base cases:
p 0 = (1, 0) 
-- mutual step for closing the knot:
p n = (prod n, sum n) = (n*x, n+y) where (x,y) = (prod (n-1), sum (n-1)) = p (n-1)
-- therefore, without referencing functions `prod` and `sum`:
p n = (n*x, n+y) where (x,y) = p (n-1)

    
catalan 0 = 1
catalan n = sum [(catalan k) * catalan (n-1-k) | k <- [0..(n-1)]]

c n = [(catalan k) * catalan (n-1-k) | k <- [0..(n-1)]]

catalan' n = sum r where r = c (n-1)

c 0 = [catalan 0 * catalan 0] = [1]
c n = [(catalan k) * catalan (n-1-k) | k <- [0..(n-1)]]
    = (catalan 0 * catalan (n-1) : ) . 
        map (\(i,x) -> (div x (catalan (n-i)))*catalan (n+1-i)) . 
            zip [2..] $ x
    where x = c (n-1)












