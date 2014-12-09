module RHM.Enumeratee where

import RHM.Intern.Pure

import Prelude hiding (map, filter, takeWhile)

type Enumeratee eo ei a = Iteratee ei a -> Iteratee eo (Iteratee ei a)

-- | Map the elements of the input stream before feeding them to the inner iteratee.
map :: (a -> b) -> Enumeratee a b c
map f (Cont k) = Cont step
  where
    step Empty    = map f (Cont k)
    step EOF      = Done (k EOF) EOF
    step (Elem x) = map f . k . Elem . f $ x
map _ iter@_   = return iter

-- | Feed only those elements from the input stream to the inner iteratee that
-- satisfy a given predicate @p@.
filter :: (e -> Bool) -> Enumeratee e e a
filter p (Cont k) = Cont step
  where
    step el@(Elem x) | p x = filter p (k el)
    step EOF               = Done (k EOF) EOF
    step _                 = Cont step
filter _ iter     = return iter

-- | Take elements from the input stream as long as they satisfy a given
-- predicate @p@, and feed them to the inner iteratee.
takeWhile :: (e -> Bool) -> Enumeratee e e a
takeWhile p (Cont k) = Cont step
  where
    step el@(Elem x) | p x       = takeWhile p (k el)
                     | otherwise = return $ Cont k
    step EOF   = Done (k EOF) EOF
    step Empty = Cont step
takeWhile _ iter     = return iter
