
import Control.Monad.Cont

sum_cps lst = worker lst id
    where   worker [] k = k 0
            worker (x:xs) k = worker xs (k . (+ x)) 

foldr' f b [] = b
foldr' f b (x:xs) = f x (foldr' f b xs)

foldr'_cps f b lst = worker lst id
    where   worker [] k = k b
            worker (x:xs) k = worker xs (k . (f x))


{- 
    Prove by induction on list `s`, for any continuation `k`, function `f` and boundary `b`, 
    that `worker s k = k (foldr' f b s)` holds.

    Base, s = []:
        worker [] k = k (foldr' f b [])
        k b = k b                           {by base patterns}

    Induction step, s = x:xs, so assume that `worker xs k = k (foldr' f b xs)` holds:
        worker (x:xs) k = k (foldr' f b (x:xs))
        worker xs (k . (f x)) = k (f x (foldr' f b xs))             {by pattern matching}
        (k . (f x)) $ foldr' f b xs = (k . (f x)) $ foldr' f b xs   {by induction hp on lhs and composition on rhs}

    Hence, in particular if `k = (\x -> x) = id` then:
        worker s id = foldr' f b s
    therefore the two versions are equivalent.

foldr'_cont f b s = call
foldr'_cont f b [] = b
foldr'_cont f b (x:xs) = f x (foldr' f b xs)
-}

call_cc doer = runCont (callCC doer) id
    --where receiver exit = do doer exit

call_cc' :: ((a -> r) -> r) -> (a -> r) -> r
call_cc' f last = runCont (cont f) last

{-
Control.Monad.Cont.cont :: ((a -> r) -> r) -> Cont r a
Prelude Data.Bits Control.Monad.Cont> :t Control.Monad.Cont.runCont
Control.Monad.Cont.runCont :: Cont r a -> (a -> r) -> r
-}

-- callCC :: MonadCont m => ((a -> m b) -> m a) -> m a
call_cc'' doer = runCont (callCC receiver) id
    where receiver hop = return $ doer hop


whatsYourName :: String -> String
whatsYourName name =
    (`runCont` id) $ do                      -- 1
        response <- callCC $ \exit -> do       -- 2
            validateName name exit               -- 3
            return $ "Welcome, " ++ name ++ "!"  -- 4
        return response                        -- 5

validateName name exit = do
    when (null name) (exit "You forgot to tell me your name!")
