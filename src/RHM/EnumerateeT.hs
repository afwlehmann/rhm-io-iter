module RHM.EnumerateeT where

import RHM.Input
import RHM.Intern.Trans

import Prelude hiding (map, filter, takeWhile)

type EnumerateeT eo ei m a = IterateeT ei m a -> IterateeT eo m (IterateeT ei m a)

-- | Map the elements of the input stream before feeding them to the inner iteratee.
map :: Monad m => (a -> b) -> EnumerateeT a b m c
map f m = IterateeT $ do
  iter <- runIterateeT m
  return $ case iter of
             ContM k -> ContM $ step k
             _       -> DoneM m Empty
  where
    step k (Elem x) = map f . k . Elem . f $ x
    step k EOF      = IterateeT . return $ DoneM (k EOF) EOF
    step _ Empty    = map f m

-- | Feed only those elements from the input stream to the inner iteratee that
-- satisfy a given predicate @p@.
filter :: Monad m => (e -> Bool) -> EnumerateeT e e m a
filter p m = IterateeT $ do
  iter <- runIterateeT m
  return $ case iter of
             ContM k -> ContM $ step k
             _       -> DoneM m Empty
  where
    step k el@(Elem x) | p x = filter p (k el)
    step k EOF               = IterateeT . return $ DoneM (k EOF) EOF
    step _ _                 = filter p m

-- | Take elements from the input stream as long as they satisfy a given
-- predicate @p@, and feed them to the inner iteratee.
takeWhile :: Monad m => (e -> Bool) -> EnumerateeT e e m a
takeWhile p m = IterateeT $ do
  iter <- runIterateeT m
  return $ case iter of
             ContM k -> ContM $ step k
             _       -> DoneM m Empty
  where
    step k el@(Elem x) | p x       = takeWhile p (k el)
                       | otherwise = return m
    step k EOF                     = IterateeT . return $ DoneM (k EOF) EOF
    step _ Empty                   = takeWhile p m
