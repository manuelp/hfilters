{-# OPTIONS_GHC -Wall #-}
module Filters where

data Filter = IntF String Int 
            | StrF String String
            | BoolB String Bool
              deriving Show

class Filterable d where
    matches :: d -> Filter -> Bool

data Person = Person {
      pName :: String,
      pSurname :: String,
      pAge :: Int
    } deriving Show

instance Filterable Person where
    matches p (IntF "age" v) = (pAge p) == v
    matches p (StrF "name" v) = (pName p) == v
    matches p (StrF "surname" v) = (pSurname p) == v
    matches _ _ = False

filterData :: Filterable d => [d] -> Filter -> [d]
filterData xs f = filter ((flip matches) f) xs

filterData' :: Filterable d => [d] -> [Filter] -> [d]
filterData' xs fs = foldl filterData xs fs

