{-# OPTIONS_GHC -Wall #-}
module Filters where

-- A filter is a key associated with a value of a certain type.
data Filter = IntF String Int 
            | StrF String String
            | BoolF String Bool
              deriving Show

-- Values that are filterable can be matched against a Filter. We
-- define a default implementation of the matching function that
-- always returns False. This way when we write our instances we only
-- need to specify the real tests.
class Filterable d where
    matches :: d -> Filter -> Bool
    matches _ _ = False

-- As an example, we define a record type to use for our experiments.
data Person = Person {
      pName :: String,
      pSurname :: String,
      pAge :: Int,
      isASuperHero :: Bool
    } deriving Show

-- We make Person values Filterable by defining the available filters
-- and how they are computed.
instance Filterable Person where
    matches p (IntF "age" v) = (pAge p) == v
    matches p (StrF "name" v) = (pName p) == v
    matches p (StrF "surname" v) = (pSurname p) == v
    matches p (BoolF "is-a-superhero" v) = (isASuperHero p) == v
    matches _ _ = False

-- Filtering data by using a filter can be defined as a generic
-- function.
filterData :: Filterable d => [d] -> Filter -> [d]
filterData xs f = filter ((flip matches) f) xs

-- This way, filtering data using a list of filters can be defined
-- generically as a fold.
filterData' :: Filterable d => [d] -> [Filter] -> [d]
filterData' xs fs = foldl filterData xs fs

