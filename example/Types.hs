{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Csv
#if __GLASGOW_HASKELL__ < 800
import Language.Haskell.TH.Lift
#else
import Language.Haskell.TH.Syntax
#endif

data Rec = Rec
  { _rKey :: !String
  , _rVal :: !Int
  } deriving (Show)

#if __GLASGOW_HASKELL__ < 800
$(deriveLift ''Rec)
#else
deriving instance Lift Rec
#endif

instance FromNamedRecord Rec where
  parseNamedRecord r = Rec <$> r .: "key" <*> r .: "val"
