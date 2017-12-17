{-# LANGUAGE OverloadedStrings #-}

module Github.Types
    ( Authorization(..)
    , UserAgent
    , Owner
    , Repository
    , Organization
    , Issue
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)
import Web.Internal.HttpApiData

type UserAgent = String
type Owner = String
type Repository = String

newtype Authorization = Authorization String

instance ToHttpApiData Authorization where
  toQueryParam (Authorization a) = toQueryParam a

data User = User
  { login :: Text
  } deriving Show

instance FromJSON User where 
  parseJSON (Object o) = 
    User <$> o .: "login" 
  parseJSON invalid    = typeMismatch "User" invalid


data Issue = Issue
  { number :: Int
  , state :: String
  , user :: User
  , assignee :: Maybe User
  } deriving Show

instance FromJSON Issue where
  parseJSON (Object o) =
    Issue <$> o .: "number"
          <*> o .: "state"
          <*> o .: "user"
          <*> o .:? "assignee"
  parseJSON invalid    = typeMismatch "Issue" invalid

data Organization = Organization
  { id :: Int
  } deriving Show

instance FromJSON Organization where 
  parseJSON (Object o) = 
    Organization <$> o .: "id"
  parseJSON invalid    = typeMismatch "Organization" invalid
