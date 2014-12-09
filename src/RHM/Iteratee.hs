module RHM.Iteratee
       ( Iteratee
       , evalIteratee

       , count
       , drop
       , head
       , maybeHead
       , repeat
       , fold
       , foldPartial

       , acceptElem
       , acceptElems) where

import RHM.Input
import RHM.Intern.Pure

import Prelude hiding (drop, head, repeat, or)

-- | The number of elements in the input stream.
count :: Iteratee e Int
count = Cont $ step 0
  where
    step n EOF      = Done n EOF
    step n (Elem _) = Cont $ step (n+1)
    step n _        = Cont $ step n

-- | Drop a given number of elements from the input stream.
drop :: Int -> Iteratee e ()
drop 0 = Done () Empty
drop n = Cont step
  where
    step EOF      = Done () EOF
    step (Elem _) = drop (n-1)
    step _        = drop n


-- | The first element of the input stream.
head :: Iteratee e e
head = Cont step
  where
    step (Elem x) = Done x Empty
    step EOF      = Error "Unexpected end of stream"
    step _        = head

-- | Maybe the first element of the input stream.
maybeHead :: Iteratee e (Maybe e)
maybeHead = Cont step
  where
    step (Elem x) = Done (Just x) Empty
    step EOF      = Done Nothing EOF
    step Empty    = maybeHead

-- | Repeat the given Iteratee until receiving EOF.
-- If EOF is received while the Iteratee is currently not in state Done, it will
-- be discarded. Note that `sequence . repeat` would not terminate.
repeat :: Iteratee e a -> Iteratee e [a]
repeat iter = Cont  step
  where
    step Empty = Cont step
    step EOF   = Done [] EOF
    step el    = case iter of
                   Done r rst -> Done [r] rst
                   Error msg  -> Error msg
                   Cont k     -> do
                     h <- k el
                     t <- repeat iter
                     return (h:t)

-- | Fold a given function over the input stream.
-- This is probably what you want.
fold :: (b -> a -> b) -> b -> Iteratee a b
fold f zero = Cont step
  where
    step Empty    = fold f zero
    step EOF      = Done zero EOF
    step (Elem x) = fold f (f zero x)

-- | Fold a given function over the input stream, providing means for early stop.
foldPartial :: (b -> a -> Maybe b) -> b -> Iteratee a b
foldPartial f zero = Cont step
  where
    step Empty    = foldPartial f zero
    step EOF      = Done zero EOF
    step (Elem x) = case f zero x of
                      Just x' -> foldPartial f x'
                      Nothing -> Done zero Empty

-- | Accept a given element from the input stream, else fail.
-- Note that `Show e =>` is a bad idea because it restricts the types of
-- elements. It is merely used for demonstration (and somewhat meaningful error
-- messages).
acceptElem :: (Eq e, Show e) => e -> Iteratee e e
acceptElem x = head >>= go
  where
    go x' | x == x'   = Done x Empty
          | otherwise = Error $ "Expected: " ++ show x

-- | Accept a given list of elements fro mthe input stream, else fail.
-- Again, `Show e` is a bad idea.
acceptElems :: (Eq e, Show e) => [e] -> Iteratee e [e]
acceptElems = mapM acceptElem
