{-# LANGUAGE OverloadedStrings #-}

module Types
    ( Authorization
    , UserAgent
    , Owner
    , Repository
    , Organization
    , Issue
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text)

type Authorization = String
type UserAgent = String
type Owner = String
type Repository = String

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
  , assignee :: User
  } deriving Show

instance FromJSON Issue where
  parseJSON (Object o) =
    Issue <$> o .: "number"
          <*> o .: "state"
          <*> o .: "user"
          <*> o .: "assignee"
  parseJSON invalid    = typeMismatch "Issue" invalid

data Organization = Organization
  { id :: Int
  } deriving Show

instance FromJSON Organization where 
  parseJSON (Object o) = 
    Organization <$> o .: "id"
  parseJSON invalid    = typeMismatch "Organization" invalid

