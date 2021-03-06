
{-|
Module      : W
Description : Short description
Copyright   : (c) Some Guy, 2013
Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module W where

import Control.Monad.Writer
import Data.Ratio

finalCountDown :: Int -> Writer [String] ()
finalCountDown 0 = do tell ["0"]
finalCountDown x = do
    finalCountDown (x-1)
    tell [show x]



newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

{-|
  The 'square' function squares an integer.
  It takes one argument, of type 'Int'.
-}
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
    deriving Show

instance (Enum a, Enum b) => Enum (ClockwisePairEnum a b) where

    fromEnum (ClockwisePairEnum (a, b)) = clockwise (fromEnum a) (fromEnum b)
        where 
            clockwise_recursive 0 0 = 0
            clockwise_recursive 0 b_index = 1 + clockwise (b_index-1) 0 
            clockwise_recursive a_index 0 = a_index + clockwise 0 a_index 
            -- previous equations yield the following closed formula:
            -- clockwise_recursive 0 a_index = div (a_index * (a_index + 1)) 2
            clockwise_recursive a_index b_index = a_index + clockwise 0 (a_index + b_index)

            clockwise a_index b_index = a_index + gauss (a_index + b_index)
                where gauss n = div (n * (n + 1)) 2
    
    toEnum 0 = ClockwisePairEnum (toEnum 0, toEnum 0)
    toEnum x =  let cumulate = scanl (+) 0 [1..]
                    (ps, n:_) = break (x < ) cumulate 
                    p:_ = reverse ps -- `p` is the last element of `ps`
                    a_index = x - p
                    b_index = n - x - 1
                in  ClockwisePairEnum (toEnum a_index, toEnum b_index)


catalan 0 = 1
catalan n = sum [catalan k * (catalan $ n-1-k) | k <- [0..n-1]]

g 0 = ([1], [1])
g n = let v = sum $ zipWith (*) r s 
      in (r ++ [v], v : s)
      where (r, s) = g $ n-1

catalan' = head . snd . g

prodsum' n = n * x + (n + y) where (x, y) = p (n-1)

-- looking at base cases:
p 0 = (1, 0) 
-- mutual step for closing the knot:
--p n = (prod n, sum n) = (n*x, n+y) where (x,y) = (prod (n-1), sum (n-1)) = p (n-1)
-- therefore, without referencing functions `prod` and `sum`:
p n = (n*x, n+y) where (x,y) = p (n-1)

prodsum x = prod x + sum' x

prod 0 = 1
prod n = n * prod (n-1)

sum' 0 = 0
sum' m = m + sum' (m-1)

quick_checks 0 = 0
quick_checks n = n + 1 + (2 * div cs n)
    where cs = sum . map quick_checks $ [0..(n-1)]

--q n = map quick_checks [0..(n-1)]

quick_checks' 0 = 0
--quick_checks' n = n + 1 + (2 * div (sum cs) n) where cs = q (n-1)
quick_checks' n = c where c : _ = q n

q 0 = [0]
q n =   let new = n + 1 + (2 * div (sum cs) n) 
        in  new : cs where cs = q (n-1)

lift_constant :: a -> () -> a
lift_constant n = \_ -> n

