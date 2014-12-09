{-# LANGUAGE OverloadedStrings #-}
module RHM.Intern.Pure
       ( Input(..)
       , Iteratee(..)
       , evalIteratee )
       where

import RHM.Input

import Control.Applicative
import Control.Monad
import Data.Text.Lazy (unpack)
import Formatting

data Iteratee e a = Cont (Input e -> Iteratee e a)
                  | Done a (Input e)
                  | Error String

instance (Show e, Show a) => Show (Iteratee e a) where
  show (Cont _)          = "Cont _"
  show (Error msg)       = unpack $ format ("Error \"" % string % "\"") msg
  show (Done x remInput) = unpack $ format ( "Done (" % string % "," % string % ")" )
                                           (show x) (show remInput)

instance Functor (Iteratee e) where
  fmap f (Done x remInput) = Done (f x) remInput
  fmap f (Cont k)          = Cont $ fmap f . k     -- same as: Cont $ \el -> fmap f (k el)
  fmap _ (Error msg)       = Error msg

instance Applicative (Iteratee e) where
  pure = return
  (<*>) = ap

instance Monad (Iteratee e) where
  return x = Done x Empty
  Cont k          >>= f = Cont $ k >=> f
  Error msg       >>= _ = Error msg
  Done x remInput >>= f = case f x of
                            Cont k    -> k remInput
                            Error msg -> Error msg
                            Done x' _ -> Done x' remInput

instance Alternative (Iteratee e) where
  empty = Error "empty"
  d@(Done _ _) <|> _    = d
  Cont k       <|> othr = Cont $ \el -> k el <|> othr
  e@(Error _)  <|> othr = case othr of
                            Error _ -> e
                            _       -> othr

evalIteratee :: Iteratee e a -> a
evalIteratee (Done r _)  = r
evalIteratee (Error msg) = error msg
evalIteratee (Cont k)    = case k EOF of
                             Done r _  -> r
                             Error msg -> error msg
                             Cont _    -> error "Divergent iteratee"
