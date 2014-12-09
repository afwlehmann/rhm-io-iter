{-# LANGUAGE LambdaCase #-}
module Main where

import RHM.Iteratee
import RHM.Enumerator
import RHM.Enumeratee
import qualified RHM.IterateeT as RT
import qualified RHM.EnumeratorT as RT

import Prelude hiding (drop, head, repeat, filter, map, sum)
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid

-- | A tuple consisting of the first and second elements of the input stream.
tuple :: Iteratee e (e,e)
tuple = liftA2 (,) head head

-- | A list consisting of the first n successive pairs of input elements.
nTuples :: Int -> Iteratee e [(e,e)]
nTuples n = replicateM n tuple

-- | A list of alternate elements from the input stream.
alternates :: Iteratee e [e]
alternates = repeat $ drop 1 >> head

-- | A list consisting of tuples of successive pairs of input elements
--
-- Note that
-- > sequence . Prelude.repeat tuple
-- will not terminate.
tuples :: Iteratee e [(e,e)]
tuples = repeat tuple

-- | Decodes a run-length encoded input stream.
rlDecode :: Iteratee Char String
rlDecode = do
  n <- head
  x <- head
  t <- rlDecode
  return $ replicate (digitToInt n) x ++ t

-- | Alternate implementation of rlDecode using Applicative
rlDecode' :: Iteratee Char String
rlDecode' = liftA3 go head head rlDecode'
  where
    go n x t = replicate (digitToInt n) x ++ t

-- | Accept either "Hello world!" or "Foo", else fail.
helloWorldOrFoo :: Iteratee Char String
helloWorldOrFoo = acceptElems "Hello world!" <|> acceptElems "Foo"

-- | Sum up elements from a stream of Monoid elements.
sum :: Monoid e => Iteratee e e
sum = fold mappend mempty

-- | Filter and sum up digits from a character stream.
--
-- Note the order of the function composition.
filterMapSum :: Iteratee Char (Iteratee Char (Iteratee (Sum Int) (Sum Int)))
filterMapSum = filter isDigit . map (Sum . digitToInt) $ sum

-- | The sum of the number elements in the given chunks.
countBytesInChunks :: Iteratee [e] Int
countBytesInChunks = fold (\acc xs -> acc + length xs) 0

example1 :: IO ()
example1 = print . evalIteratee . enumerate [1..10] $ alternates

example2 :: IO ()
example2 = print . evalIteratee . enumerate "Hello world!" $ helloWorldOrFoo

example2' :: IO ()
example2' = print . evalIteratee . enumerate "Foo2" $ helloWorldOrFoo

example3 :: IO ()
example3 = RT.evalIterateeT . RT.enumerate msg $ RT.typeWriter
  where
    msg = "\n\
          \Welcome!\n\
          \\n\
          \You may want to begin with looking at the pure implementation in\n\
          \\n\
          \    RHM.Intern.Pure and\n\
          \    RHM.Iteratee .\n"

main :: IO ()
main = example3
