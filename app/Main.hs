{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app _ respond = respond $ responseLBS
  status200
  [("Content-Type", "text/plain")]
  "Hello, web!"

main :: IO ()
main = do
  putStrLn "Running on http://localhost:8080"
  run 8080 app
