module RHM.Input where

data Input a = Elem a
             | Empty
             | EOF
             deriving (Eq, Show)
