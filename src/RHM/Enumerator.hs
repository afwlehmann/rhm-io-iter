module RHM.Enumerator
       ( Enumerator
       , enumerate
       , (>>>>)
       , enumTextFile
       , enumStdIn )
       where

import RHM.Input
import RHM.Intern.Pure

import Control.Monad
import System.IO

type Enumerator e a = Iteratee e a -> Iteratee e a

-- | Folds the given iteratee over an input stream consisting of the elements of
-- the given list.
enumerate :: [e] -> Enumerator e a
enumerate (x:xs) (Cont k) = enumerate xs (k (Elem x))
enumerate _ iter          = iter

infixl 6 >>>>

-- | Concatenates the two given enumerators.
(>>>>) :: Enumerator e a -> Enumerator e a -> Enumerator e a
e1 >>>> e2 = e2 . e1

-- Input-only IO can be done with pure iteratees.
type EnumeratorIO e a = Iteratee e a -> IO (Iteratee e a)

-- | Fold the given iteratee over the lines from a given handle.
enumHandle :: Handle -> EnumeratorIO String a
enumHandle handle (Cont k) = do
  eof <- hIsEOF handle
  if eof
     then return $ k EOF
     else liftM (k . Elem) (hGetLine handle) >>= enumHandle handle
enumHandle _      iter     = return iter

-- | Fold the given iteratee over the lines of a given input file.
enumTextFile :: FilePath -> EnumeratorIO String a
enumTextFile fn iter = withFile fn ReadMode $ flip enumHandle iter

-- | Fold the given iteratee over input from stdin until an empty line is
-- entered.
enumStdIn :: EnumeratorIO String a
enumStdIn (Cont k) = do
  str <- getLine
  return $ if null str
              then k EOF
              else k (Elem str)
enumStdIn iter     = return iter
