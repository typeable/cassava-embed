{-|
Module      : Data.Csv.Embed
Description : CSV-file embedding library
License     : BSD-3
Stability   : experimental
Portability : POSIX

@cassava-embed@ helps to embed CSV-file using TemplateHaskell.

Typical usage:

> print $(embedRecord (Proxy :: Proxy (String, Int)) "data.csv")

-}

module Data.Csv.Embed
  ( embedRecords
  , embedNamedRecords
  ) where

import Data.ByteString.Lazy as BSL
import Data.Csv
import Data.Proxy
import Data.Vector
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Embeds CSV-file as list of values with type
-- that implements 'FromRecord'.
embedRecords
  :: (Lift a, FromRecord a)
  => Proxy a   -- ^ type of row (proxied)
  -> FilePath  -- ^ path to CSV-file
  -> ExpQ
embedRecords proxy path = do
  content <- runIO $ BSL.readFile path
  let
    Right rows = decode HasHeader content
    Right xs = traverse (fmap cast . runParser . parseRecord) $ toList rows
  ListE <$> Prelude.mapM lift xs
  where
    cast x = x `asProxyTypeOf` proxy

-- | Embeds CSV-file as list of values with type
-- that implements 'FromNamedRecord'.
embedNamedRecords
  :: (Lift a, FromNamedRecord a)
  => Proxy a   -- ^ type of row (proxied)
  -> FilePath  -- ^ path to CSV-file
  -> ExpQ
embedNamedRecords proxy path = do
  content <- runIO $ BSL.readFile path
  let
    Right (_, rows) = decodeByName content
    Right xs = traverse (fmap cast . runParser . parseNamedRecord) $ toList rows
  ListE <$> Prelude.mapM lift xs
  where
    cast x = x `asProxyTypeOf` proxy
