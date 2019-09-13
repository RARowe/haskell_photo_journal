{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Lib
import qualified Network.Wai as W
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as A
import GHC.Generics
import qualified Migration as M (runMigrations)
import qualified Database.HDBC.Sqlite3 as S (connectSqlite3)
import qualified Database.HDBC as DB (disconnect)

headers :: ResponseHeaders
headers = [("Content-Type", "text/plain"), ("Cache-Control", "public, max-age=604800")]

index :: W.Response
index = W.responseLBS
  status200
  headers
  "Hello! This is the photo journal API!"

notFound :: W.Response
notFound = W.responseLBS
  status404
  headers
  "404: Not found."

api :: W.Application
api req res = res $ W.responseLBS
  status200
  [("Content-Type", "application/json")]
  (A.encode filmRolls)


router :: W.Application
router req res =
  case W.pathInfo req of
    [] -> res index
    "api":path -> api req res
    _ -> res notFound


main :: IO ()
main = do
  putStrLn "Running migrations..."
  runMigrations
  putStrLn "Running on http://localhost:8080"
  run 8080 router

runMigrations :: IO ()
runMigrations = do
  conn <- S.connectSqlite3 "photo_journal.db"
  M.runMigrations conn
  DB.disconnect conn


data FilmRoll = FilmRoll {
    title :: String
  , photoCount :: Int
  , dateCreated :: String
  } deriving (Generic, Show)

instance A.ToJSON FilmRoll

instance A.FromJSON FilmRoll

filmRolls :: [FilmRoll]
filmRolls = [
  FilmRoll{ title="In the park", photoCount=22, dateCreated="03/23/2019" },
  FilmRoll{ title="Lake house weekend", photoCount=24, dateCreated="07/23/2019" },
  FilmRoll{ title="Cecily and friends", photoCount=27, dateCreated="04/23/2019" }]
