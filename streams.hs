
module Streams where

data Stream α = E | S (() -> (α, Stream α))

hd :: Stream α -> Maybe α
hd E = Nothing
hd (S s) = Just . fst . s $ ()

tl :: Stream α -> Maybe (Stream α)
tl E = Nothing
tl (S s) = Just . snd . s $ ()

consˢ :: α -> Stream α -> Stream α
consˢ v vs = S (\_ -> (v,vs))

stream_to_list :: Stream α -> [α]
stream_to_list E = []
stream_to_list (S s) = α₀ : (stream_to_list αs)
  where (α₀, αs) = s ()

list_to_stream :: [α] -> Stream α
list_to_stream [] = E
list_to_stream (v:vs) = S (\_ -> (v, β))
  where β = list_to_stream vs

generate :: (α -> α) -> α -> Stream α
generate f v = S (\_ -> (v, α))
  where α = generate f $ f v

instance Monoid (Stream a) where
  mempty = E
  mappend E α = α
  mappend (S s) α = S (\_ -> (v, β))
    where
      (v, vs) = s ()
      β = mappend vs α

instance Functor Stream where
  fmap f E = E
  fmap f (S s) = S (\_ -> (f v, β))
    where (v, vs) = s ()
          β = fmap f vs

instance Applicative Stream where
  pure v = S (\_ -> (v, E))
  (<*>) E _ = E
  (<*>) (S α) β = appendˢ γ $ fs <*> β
    where
      (f, fs) = α ()
      γ = fmap f β

instance Monad Stream where
  E >>= _ = E
  (S s) >>= f = appendˢ α β
    where
      (s₀, γ) = s ()
      α = f s₀
      β = γ >>= f


nats :: Stream Integer
nats = generate succ 0

squares :: Stream Integer
squares = fmap (^ 2) nats

firstˢ :: (α -> Bool) -> Stream α -> Maybe (α, Stream α)
firstˢ _ E = Nothing
firstˢ p (S s) = if p v then Just (v, vs) else firstˢ p vs
  where (v, vs) = s ()

filterˢ :: (α -> Bool) -> Stream α -> Stream α
filterˢ _ E = E
filterˢ p (S s) = if p v then S (\_ -> (v, β)) else β
  where (v, vs) = s ()
        β = filterˢ p vs

zipˢ :: (α -> β -> γ) -> Stream α -> Stream β -> Stream γ
zipˢ _ E _ = E
zipˢ _ _ E = E
zipˢ f (S s) (S r) = S (\_ -> (f v w, α))
  where ((v,vs), (w,ws)) = (s (), r ())
        α = zipˢ f vs ws

whileˢ :: (α -> Bool) -> Stream α -> (Stream α, Stream α)
whileˢ _ E = (E, E)
whileˢ p s₀@(S s) = if p v then (consˢ v α, β) else (E, s₀)
  where (v, vs) = s ()
        (α, β) = whileˢ p vs

nextˢ :: (α -> (β, α)) -> α -> Stream β
nextˢ r a = S (\_ -> (s, β))
  where (s, α) = r a
        β = nextˢ r α

filter'ˢ p = nextˢ  ((\(Just x) -> x) . (firstˢ p))

concatˡ :: Stream [α] -> Stream α
concatˡ E = E
concatˡ (S s) = cc α
  where (α, αs) = s ()
        cc [] = concatˡ αs
        cc (a:as) = S (\_ -> (a, γ))
          where γ = concatˡ $ S (\_ -> (as, αs))

concatˢ :: Stream (Stream α) -> Stream α
concatˢ E = E
concatˢ (S s) = cc α
  where (α, αs) = s ()
        cc E = concatˢ αs
        cc (S r) = S (\_ -> (b, γ))
          where (b, bs) = r ()
                γ = concatˢ $ S (\_ -> (bs, αs))

fmap₂ˢ :: (t -> a -> α) -> Stream t -> Stream a -> Stream α
fmap₂ˢ f s r = concatˢ . fmap g $ s
  where g z = fmap (f z) r


data BTree α = Leaf | Node α (BTree α) (BTree α)
  deriving Show

instance Functor BTree where
  fmap f = g
    where g Leaf = Leaf
          g (Node n l r) = Node (f n) α β
            where (α, β) = ((g l), (g r))

--btree_visit :: (β -> α -> α -> α) -> (γ -> β) -> BTree γ -> α
btree_visit g b = v
  where v Leaf = b
        v (Node n l r) = g n α β
          where (α, β) = ((v l), (v r))

instance Foldable BTree where
  foldr g b Leaf = b
  foldr g b (Node n l r) = foldr g (g n α) l
    where α = (foldr g b r)

uˢ :: α -> Stream α
uˢ v = S (\_ -> (v, E))

appendˢ :: Stream α -> Stream α -> Stream α
appendˢ E y = y
appendˢ (S s) y = S (\_ -> (v, β))
  where (v, vs) = s ()
        β = appendˢ vs y

flattenˢ :: (Foldable t, Functor t) => t α -> Stream α
flattenˢ = foldr appendˢ E . fmap uˢ

make_BTree :: (α -> α) -> (α -> α) -> α -> Stream (BTree α)
make_BTree l r x = S (\_ -> (Node x a b, appendˢ as bs))
  where (S α) = make_BTree l r $ l x
        (S β) = make_BTree l r $ r x
        (a, as) = α ()
        (b, bs) = β ()

partitions :: BTree [Integer]
partitions = let (S s) = make_BTree l r [1] in fst $ s ()
  where l (v:vs) = (v+1):vs
        r vs = 1:vs

prune :: (α -> Bool) -> BTree α -> BTree α
prune p Leaf = Leaf
prune p (Node v l r) = if p v then Node v α β else Leaf
  where α = prune p l
        β = prune p r

levels :: (Foldable t, Ord a, Num a) => a -> BTree (t a) -> BTree (t a)
levels n = prune ((< n) . sum)         

fringe :: BTree t -> [t]
fringe Leaf = []
fringe (Node n Leaf Leaf) = [n]
fringe (Node _ l r) = α ++ β
  where (α, β) = (fringe l, fringe r)

data Treeˡ α = Leafˡ | Nodeˡ α (Treeˡ α) (Stream α)

generateˡ :: (α -> α) -> (α -> α) -> α -> Stream (Treeˡ α)
generateˡ l r x = S (\_ -> (Nodeˡ x a β, as))
  where (S α) = generateˡ l r $ l x
        β = generate r $ r x
        (a, as) = α ()

lattice :: Treeˡ (Integer, Integer)        
lattice = let (S s) = generateˡ l r (0,0) in fst $ s ()
  where l (x,y) = (x+1, y+1)
        r (x,y) = (x, y+1)

instance Functor Treeˡ where
  fmap f Leafˡ = Leafˡ
  fmap f (Nodeˡ v t s) = Nodeˡ (f v) α β
    where (α, β) = (fmap f t, fmap f s)

pruneˡ p Leafˡ = Leafˡ
pruneˡ p (Nodeˡ n t s) = if p n then Leafˡ else (Nodeˡ n α β)
  where α = pruneˡ p t
        β = fst . whileˢ (not . p) $ s

treeˡ_to_treeᵇ Leafˡ = Leaf
treeˡ_to_treeᵇ (Nodeˡ v t s) = Node v α (toᵇ s)
  where α = treeˡ_to_treeᵇ t
        toᵇ E = Leaf
        toᵇ (S s) = Node β Leaf (toᵇ βs)
          where (β, βs) = s ()

sortˡ (Nodeˡ v t s) = S (\_ -> (v, α))
  where α = sortˡ $ merge t s
        merge t₀@(Nodeˡ n t₁ s₁) β@(S b₀) =
          if n < b then Nodeˡ n γ β else Nodeˡ b t₀ bs
          where (b, bs) = b₀ ()
                γ = merge t₁ s₁

repeatedˢ p (S s) = if p v w then S (\_ -> ((v, w), α)) else α
  where (v, β@(S vs)) = s ()
        (w, _) = vs ()
        α = repeatedˢ p β
                
dijkstra_weizenbaum n = repeatedˢ r . sortˡ . fmap h
  where h p@(x,y) = (x^n + y^n, p)
        r = \v w -> fst v == fst w

combinations :: Integer -> Integer -> Stream (Stream Integer)
combinations n m | m > n || m < 0 = E
                 | n == 0 = uˢ E
                 | otherwise = appendˢ α β
  where 
    α = fmap (consˢ 1) $ combinations (n-1) (m-1)
    β = fmap (consˢ 0) $ combinations (n-1) m
    
permutations :: Integer -> Stream (Stream Integer)
permutations 0 = uˢ E
permutations n = insertˢ n $ permutations (n - 1)
  where insertˢ n = concatˢ . fmap (put n)
          where put n E = uˢ $ uˢ n
                put n α@(S s) = let h = consˢ n α in consˢ h β
                  where (v, vs) = s ()
                        β = fmap (consˢ v) $ put n vs

uptoˢ :: Integer -> Stream Integer
uptoˢ n = fst . whileˢ (<= n) $ generate (+ 1) 0

downfromˢ :: Integer -> Stream Integer
downfromˢ n = fst . whileˢ (>= 0) $ generate (subtract 1) n

genalt :: Integer -> (Stream (Stream Integer))
genalt n = let α = uptoˢ n
               β = downfromˢ n
           in consˢ α $ consˢ β γ
  where
    γ = genalt n
    
data Treeᶠ α = Leafᶠ | Nodeᶠ α (Stream (Treeᶠ α))

signature_tree
  :: (Integer -> Stream (Stream α))
     -> Integer -> Stream (Stream (Treeᶠ α))
signature_tree gen n =  t n 0 
  where
    t n x | n == x = generate id E
          | otherwise = nextˢ (chunkˢ $ x + 1) α
      where
        α = zipˢ Nodeᶠ β γ
        β = concatˢ $ gen x
        γ = t n $ x + 1

chunkˢ :: Integer -> Stream α -> (Stream α, Stream α)
chunkˢ 0 η = (E, η)
chunkˢ n (S s) = (consˢ v κ, ε)
  where
    (v, vs) = s ()
    (κ, ε) = chunkˢ (n-1) vs

prefix :: Stream (Stream (Treeᶠ α)) -> Stream α
prefix (S s) = p $ s ()
  where
    p (E, _) = E
    p ((S β), βs) = S (\_ -> (v, α))
      where
        (Nodeᶠ v γ, γs) = β ()
        α = prefix . consˢ (appendˢ γ γs) $ βs

remove_0s :: Stream Integer -> Stream Integer
remove_0s = filterˢ non_zero
  where
    non_zero 0 = False
    non_zero n = True

swapˢ _ E = E
swapˢ 1 (S s) = consˢ b (consˢ a α)
  where
    (a, (S r)) = s ()
    (b, α) = r ()
swapˢ n (S s) = consˢ a $ swapˢ (n-1) as
  where
    (a, as) = s ()


permutations₁ :: Integer -> Stream (Stream Integer)
permutations₁ n = let p₀ = list_to_stream [1..n]
                  in consˢ p₀ $ permutations₂ swaps p₀ 
  where
    swaps = remove_0s . prefix $ signature_tree genalt n
    
    permutations₂ :: Stream Integer -> Stream Integer -> Stream (Stream Integer)
    permutations₂ E p = consˢ p E
    permutations₂ (S ι) p₀ = consˢ p₁ β
      where
        (i, γ) = ι ()
        p₁ = swapˢ (n-i) p₀
        β = permutations₂ γ p₁
  
