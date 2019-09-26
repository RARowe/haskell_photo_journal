{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai.Handler.Warp (run)
import qualified Migration as M (runMigrations)
import qualified Router as R (router)
import qualified DataSource as DS (withConnection)

main :: IO ()
main = do
  putStrLn "Running migrations..."
  DS.withConnection M.runMigrations
  putStrLn "Running on http://localhost:8080"
  run 8080 $ R.router
