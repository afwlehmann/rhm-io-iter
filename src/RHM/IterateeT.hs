module RHM.IterateeT
       ( IterateeT(..)
       , evalIterateeT

       , liftIteratee

       , count
       , head
       , maybeHead
       , fold
       , typeWriter )
       where

import RHM.Input
import RHM.Intern.Pure
import RHM.Intern.Trans

import Prelude hiding (head)
import Control.Concurrent
import Control.Monad.Trans
import System.IO


-- | Lift a pure Iteratee into an IterateeT.
liftIteratee :: Monad m => Iteratee e a -> IterateeT e m a
liftIteratee (Done x remInput) = IterateeT . return $ DoneM x remInput
liftIteratee (Error msg)       = IterateeT . return $ ErrorM msg
liftIteratee (Cont k)          = IterateeT . return $ ContM (liftIteratee . k)

_cont :: Monad m => (Input e -> IterateeT e m a) -> IterateeT e m a
_cont = IterateeT . return . ContM

_done :: Monad m => a -> Input e -> IterateeT e m a
_done x = IterateeT . return . DoneM x

_error :: Monad m => String -> IterateeT e m a
_error = IterateeT . return . ErrorM

fold :: Monad m => (b -> a -> b) -> b -> IterateeT a m b
fold f zero = _cont step
  where
    step Empty    = fold f zero
    step EOF      = _done zero EOF
    step (Elem x) = fold f (f zero x)

count :: Monad m => IterateeT e m Int
count = fold (\acc _ -> acc + 1) 0

head :: Monad m => IterateeT e m e
head = _cont step
  where
    step Empty    = head
    step EOF      = _error "Unexpected end of stream"
    step (Elem x) = return x

maybeHead :: Monad m => IterateeT e m (Maybe e)
maybeHead = _cont step
  where
    step Empty    = maybeHead
    step EOF      = IterateeT . return $ DoneM Nothing EOF
    step (Elem x) = return . Just $ x

typeWriter :: IterateeT Char IO ()
typeWriter = do
  x <- maybeHead
  case x of
    Just ch -> hack ch >> flush >> typeWriter
    _       -> return ()
  where
    hack ch = liftIO $ threadDelay 75000 >> putChar ch
    flush = liftIO $ hFlush stdout
