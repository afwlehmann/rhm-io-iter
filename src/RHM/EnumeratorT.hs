module RHM.EnumeratorT
       ( enumerate
       , enumHandle
       , enumTextFile ) where

import RHM.Input
import RHM.Intern.Trans

import Control.Monad.IO.Class
import System.IO

type EnumeratorT e m a = IterateeT e m a -> IterateeT e m a

enumerate :: Monad m => [e] -> EnumeratorT e m a
enumerate (x:xs) m = IterateeT $ do
  iter <- runIterateeT m
  case iter of
    ContM k -> runIterateeT . enumerate xs $ k (Elem x)
    _       -> return iter
enumerate _      m = m

enumHandle :: Handle -> EnumeratorT String IO a
enumHandle handle m = do
  eof <- liftIO $ hIsEOF handle
  if eof
     then m
     else IterateeT $ do
       iter <- runIterateeT m
       case iter of
         ContM k -> hGetLine handle >>= runIterateeT . enumHandle handle . k . Elem
         _       -> return iter

enumTextFile :: FilePath -> EnumeratorT String IO a
enumTextFile fn m = IterateeT $ withFile fn ReadMode $ runIterateeT . flip enumHandle m
