{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Csv.Embed
import Data.Proxy

import Types

main :: IO ()
main = do
  print $(embedNamedRecords (Proxy :: Proxy Rec) "example/data.csv")
  print $(embedRecords (Proxy :: Proxy (String, Int)) "example/data.csv")
