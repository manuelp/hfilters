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
    matches :: Filter -> d -> Bool
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
    matches (IntF "age" v) p = (pAge p) == v
    matches (StrF "name" v) p = (pName p) == v
    matches (StrF "surname" v) p = (pSurname p) == v
    matches (BoolF "is-a-superhero" v) p = (isASuperHero p) == v
    matches _ _ = False

-- Filtering data by using a filter can be defined as a generic
-- function.
filterData :: Filterable d => Filter -> [d] -> [d]
filterData f = filter (matches f) 

-- This way, filtering data using a list of filters can be defined
-- generically as a fold.
filterData' :: Filterable d => [d] -> [Filter] -> [d]
filterData' = foldr filterData

