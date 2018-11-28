
data Chain a = Empty | Link a (a -> Chain a)

next :: Chain a -> Chain a
next Empty = Empty
next (Link a α) = α a 

proj :: Chain a -> Maybe a
proj Empty = Nothing
proj (Link a α) = Just a

skipped :: Int -> Int -> Chain Int
skipped offset v = Link (v+offset) (skipped offset)

instance Functor Chain where
    fmap g Empty = Empty
    fmap g (Link a α) = Link (g a) (\b -> fmap g (α a))

at_most :: Int -> Chain a -> [a]
at_most 0 _ = []
at_most n Empty = []
at_most n (Link a α) = a : (at_most (n-1) (α a))

list_to_chain :: [a] -> Chain a
list_to_chain [] = Empty
list_to_chain (a:as) = Link a (\b -> list_to_chain as)

sequential :: Chain a -> Chain a -> Chain a
sequential Empty b = b
sequential (Link a α) b = Link a (\c -> sequential (α a) b) -- essentially, `mplus` with swapped arguments in the rhs

mplus :: Chain a -> Chain a -> Chain a
mplus Empty a = a
mplus (Link a α) b = Link a (\c -> mplus b (α a))

bind :: Chain a -> (a -> Chain a) -> Chain a
bind Empty g = Empty
bind (Link a α) g = mplus (g a) $ bind (α a) g

ten_nine :: [Int]
ten_nine = at_most 10 (Link 10 (\a -> Link 9 (\a -> Empty))) -- [10,9]

ints :: Int -> Chain Int
ints i = skipped 1 (i-1)

chain_repeat :: Int -> Chain Int
chain_repeat = skipped 0

evens :: Chain Int
evens = skipped 2 (-2)

odds :: Chain Int
odds = skipped 2 (-1)

nats :: Chain Int
nats = mplus evens odds

fibs :: Chain Int
fibs = f 0 1 where f n m = Link (m+n) (f m)

chain_gcd :: Int -> Int -> Chain (Int, Int)
chain_gcd m 0 = Link (0, m) (\_ -> Empty)
chain_gcd m n = Link (div m n, mod m n) (\(a, b) -> chain_gcd n b)

collatz :: Int -> Chain Int
collatz n = if even n then Link (div n 2) collatz else Link (3*n+1) collatz

boh :: Chain Int
boh = bind nats ints

upto20 :: [Int]
upto20 = (at_most 20) $ sequential (list_to_chain [1,2,3,4]) (list_to_chain [5..])
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]

upto20_interleaved :: [Int]
upto20_interleaved = (at_most 20) $ mplus (list_to_chain [1,2,3,4]) (list_to_chain [5..])
-- [1,5,2,6,3,7,4,8,9,10,11,12,13,14,15,16,17,18,19,20]

three_chains :: Chain Int
three_chains = mplus (list_to_chain . repeat $ 0) 
                     (mplus (list_to_chain . repeat $ 1) 
                            (list_to_chain . repeat $ 2))
{-
*Main> at_most 30 $ three_chains
[0,1,0,2,0,1,0,2,0,1,0,2,0,1,0,2,0,1,0,2,0,1,0,2,0,1,0,2,0,1]
in matrix notation, associating index positions as objs appear in previous list:
0₀, 0₂, 0₄, 0₆, 0₈, 0₁₀, 0₁₂, 0₁₄, 0₁₆, 0₁₈, 0₂₀, 0₂₂, 0₂₄, 0₂₆, 0₂₈
1₁, 1₅, 1₉, 1₁₃, 1₁₇, 1₂₁, 1₂₅, 1₂₉
2₃, 2₇, 2₁₁, 2₁₅, 2₁₉, 2₂₃, 2₂₇
-}

doubles = bind nats (\i -> chain_repeat (i*2))

boh' = fmap (list_to_chain . repeat) $ [0,1,2]
    
θ :: [Chain a] -> ([a] -> [a]) -> ([Chain a] -> [Chain a]) -> ([a], [Chain a])
θ [] as_col cs_col = (as_col [], cs_col [])
θ (c:cs) as_col cs_col = case c of Empty -> θ cs as_col cs_col
                                   Link a α -> θ cs (\as -> as_col (a:as)) (\cs -> cs_col((α a):cs))



dovetail :: Chain (Chain a) -> [a]
dovetail Empty = []
dovetail link = d link [] []
    where   d Empty as cs = case θ cs id id of
                                ([], []) -> as
                                (ass, css) -> d Empty (as ++ ass) css  
            d (Link l κ) as cs = let (ass, css) = θ (l:cs) id id in d (κ l) (as ++ ass) css
