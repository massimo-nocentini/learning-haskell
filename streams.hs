

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

instance Functor Stream where
  fmap f E = E
  fmap f (S s) = S (\_ -> (f v, β))
    where (v, vs) = s ()
          β = fmap f vs

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

whileˢ :: (α -> Bool) -> Stream α -> ([α], Stream α)
whileˢ _ E = ([], E)
whileˢ p s₀@(S s) = if p v then (v:α, β) else ([], s₀)
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
