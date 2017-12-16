{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( getUser
    ) where

import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Types


type CurrentUser = "api" :> "user" :> Get '[JSON] User

getUserClient :: ClientM User
getUserClient = client (Proxy :: Proxy CurrentUser)

getUser' :: IO (Either ServantError User)
getUser' = getClientEnv >>= runClientM getUserClient

getUser :: IO ()
getUser = getUser' >>= either onError print
  where
    onError error = putStrLn $ "Error: " ++ show error

getClientEnv :: IO ClientEnv
getClientEnv = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl { baseUrlScheme = Http
                        , baseUrlHost   = "localhost"
                        , baseUrlPort   = 8080
                        , baseUrlPath   = ""
                        }
  pure $ ClientEnv manager baseUrl

