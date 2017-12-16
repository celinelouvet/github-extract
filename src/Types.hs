{-# LANGUAGE OverloadedStrings #-}

module Types
    (User
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)


data User = User
  { firstName :: Text
  , lastName :: Text
  } deriving Show

instance FromJSON User where 
  parseJSON (Object o) = 
    User <$> o .: "firstName" 
         <*> o .: "lastName"
  parseJSON invalid    = typeMismatch "User" invalid

