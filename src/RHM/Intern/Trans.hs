module RHM.Intern.Trans
       ( IterM (..)
       , IterateeT(..)
       , evalIterateeT )
       where

import RHM.Input

import Prelude hiding (drop, head, repeat, or)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

data IterM e m a = DoneM a (Input e)
                 | ContM (Input e -> IterateeT e m a)
                 | ErrorM String

newtype IterateeT e m a = IterateeT { runIterateeT :: m (IterM e m a) }

instance Monad m => Functor (IterateeT e m) where
  fmap f m = IterateeT $ do
       iter <- runIterateeT m
       case iter of
         ErrorM msg       -> return $ ErrorM msg
         ContM k          -> return $ ContM (fmap f . k)
         DoneM x remInput -> return $ DoneM (f x) remInput

instance Monad m => Applicative (IterateeT e m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (IterateeT e m) where
  return x = IterateeT . return $ DoneM x Empty
  m >>= f = IterateeT $ do
    iter <- runIterateeT m
    case iter of
      ErrorM msg -> return $ ErrorM msg
      ContM k    -> return . ContM $ k >=> f       -- same as: \el -> k el >>= f
      DoneM x rst -> do
        iter' <- runIterateeT $ f x
        case iter' of
          DoneM x' _  -> return $ DoneM x' rst
          ErrorM msg  -> return $ ErrorM msg
          ContM k     -> runIterateeT . k $ rst

instance MonadTrans (IterateeT e) where
  lift m = IterateeT $ m >>= \x -> return (DoneM x Empty)

instance MonadIO m => MonadIO (IterateeT e m) where
  liftIO = lift . liftIO

evalIterateeT :: Monad m => IterateeT e m a -> m a
evalIterateeT iterT = do
  iter <- runIterateeT iterT
  case iter of
    DoneM r _  -> return r
    ErrorM msg -> error msg
    ContM k    -> do
      iter' <- runIterateeT $ k EOF
      case iter' of
        DoneM r' _  -> return r'
        ErrorM msg' -> error msg'
        ContM _     -> error "Divergent iteratee"
