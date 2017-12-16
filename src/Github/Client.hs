{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Github.Client
    ( getIssues
    ) where

import           Data.Proxy
import           Data.Text
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           Github.Types

-- GET /repos/:owner/:repository/issues
type Issues = Header "User-Agent" UserAgent :> Header "Authorization" Authorization
            :> "repos" :> Capture "owner" Owner :> Capture "repository" Repository :> "issues"
            :> Get '[JSON] [Issue] 

getIssuesClient :: Maybe UserAgent -> Maybe Authorization -> Owner -> Repository -> ClientM [Issue]
getIssuesClient = client (Proxy :: Proxy Issues)

query :: Show a 
     => (Maybe UserAgent -> Maybe Authorization -> Owner -> Repository -> ClientM [a])
     -> Owner -> Repository
     -> IO (Either ServantError [a])
query client owner repository = getClientEnv >>= runClientM (client userAgent authorization owner repository)

getIssues ::  Owner -> Repository -> IO ()
getIssues owner repository = query getIssuesClient owner repository >>= either print print

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

