{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
{-# LANGUAGE DeriveGeneric #-}
module Command.Quote.Quote where

import Data.Text (Text)
import GHC.Generics
import Data.Aeson 
import Data.Vector (Vector, fromList)
import GHC.Word

newtype Quotes = Quotes { quotes::(Vector Quote) }
    deriving (Generic, Show)

instance ToJSON Quotes where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Quotes

instance ToJSON Quote where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Quote

data Quote = Quote {
    content::Text
  , user::Maybe Text
  , userId::Maybe Word64
  } deriving (Generic, Show)

testQuote = Quote "asd" (Just "asd") (Just 1234)