

fmap = <$> :: (a -> b) -> f a -> f b

-- if the functor is (-> r) then
fmap = <$> :: (a -> b) -> (r -> a) -> (r -> b)

-- let type b be b' -> b'' -> b in
fmap = <$> :: (a -> b' -> b'' -> b) -> (r -> a) -> (r -> b' -> b'' -> b)

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b, by pattern matching a = b' and b = b'' -> b'''
(r -> b' -> b'' -> b) -> (r -> b') -> (r -> b'' -> b)

-- and then:
(r -> b'' -> b) -> (r -> b'') -> (r -> b)
