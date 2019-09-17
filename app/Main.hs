{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception (bracket)
import qualified Network.Wai as W
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as A (encode)
import qualified Migration as M (runMigrations)
import qualified Models as MO (FilmRoll(..))
import qualified Database.HDBC.Sqlite3 as S (connectSqlite3)
import qualified Database.HDBC as DB (disconnect, IConnection)
import qualified DataSource as DS (get, readStringFromRow, fromSqlRow)

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
api req res =  do
  results <- withConnection getAllFilmRolls
  res $ W.responseLBS
    status200
    [("Content-Type", "application/json")]
    (A.encode results)

getAllFilmRolls :: DB.IConnection conn => conn -> IO [MO.FilmRoll]
getAllFilmRolls conn = do
  rows <- DS.get conn "select Title, DateCreated from FilmRoll;" []
  return $ map DS.fromSqlRow rows

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

withConnection operation = bracket
  (S.connectSqlite3 "photo_journal.db")
  (DB.disconnect)
  (operation)

runMigrations :: IO ()
runMigrations = withConnection M.runMigrations
