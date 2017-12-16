module Main where

import Github

main :: IO ()
main = do
  putStrLn "Get current User"
  getIssues owner repository

owner :: Owner
owner = "Sfeir"

repository :: Repository
repository = "bouffe-front"

