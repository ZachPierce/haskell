{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Main (main) where

import           Data.Text
import           Text.HTML.Scalpel


data City = City
  { name       :: Text
  , country    :: Text
  , population :: Text
  , definition :: Text
  , altPop :: Text
} deriving (Show, Eq)

--create our function to scrape a url
allCities :: IO (Maybe ([City]))
allCities = scrapeURL "https://en.wikipedia.org/wiki/List_of_largest_cities" table

  where
    --table is a scratper that uses the data structure city
    table :: Scraper Text [City]
    --we are loking for the table with the following class name
    table = chroot ("table" @: [hasClass "static-row-numbers"]) cities

    --citites is another scraper which is scraping the table we just identified?
    cities :: Scraper Text [City]
    cities = chroots "tr" city

    --city is yet another scraper that will be scraping each row "tr" from the above scraper grabbing the first, second, third and fourth elements
    city :: Scraper Text City
    city = do
      name <- text "th"
      rows <- texts "td"
      defs <- text ("td" // "a")  
      let country = getCountry(rows)
      let population = getPopulation(rows)
      let definition = defs
      let altPop = getAltPopulation(rows)
      return $ City (strip $ name) country population definition altPop

    --im not entirely sure how the following code is working, the arguments seem to be linked to
    --how many columns we are working with in the table, ie get country is only looking at the first and
    --second columns, getPopulation is looking at the first 3, and so on. 
    getCountry(x: xs) = strip x
    getCountry(_) = "Not available"

    getPopulation(x: y: xs) = strip y
    getPopulation(_) = "Not available"

    getAltPopulation(x: y: xs: xy: xx) = strip xy
    getAltPopulation(_) = "Not available"

    --would be cool to have a single funciton that identifies which column we are after and returns that value instead of having a function
    --for every column value like above
   
    -- getByColumn = (country: population: definition: altPop: xx) = if 
    -- getByColumn (_) = "Not available"



main :: IO ()
main = do
  result <- allCities
  case result of
    Just x  -> print x
    Nothing -> print "Didn't find the necessary items."