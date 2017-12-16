{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( getOrgs
    ) where

import           Data.Proxy
import           Data.Text
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           Types

type Orgs = Header "User-Agent" UserAgent
          :> Header "Authorization" Authorization
          :> "user" :> "repos" :> Get '[JSON] [Organization]

getOrgsClient :: Maybe UserAgent -> Maybe Authorization -> ClientM [Organization]
getOrgsClient = client (Proxy :: Proxy Orgs)

getOrgs' :: IO (Either ServantError [Organization])
getOrgs' = getClientEnv >>= runClientM (getOrgsClient userAgent authorization)

getOrgs :: IO ()
getOrgs = getOrgs' >>= either print print

getClientEnv :: IO ClientEnv
getClientEnv = do
  manager <- newManager tlsManagerSettings
  let baseUrl = BaseUrl { baseUrlScheme = Https
                        , baseUrlHost   = "api.github.com"
                        , baseUrlPort   = 443
                        , baseUrlPath   = ""
                        }
  pure $ ClientEnv manager baseUrl

userAgent :: Maybe UserAgent
userAgent = Just "Servant-Client"

authorization :: Maybe Authorization
authorization = Just "Basic whateverToken"

owner :: Owner
owner = "Sfeir"

repository :: Repository
repository = "bouffe-front"

