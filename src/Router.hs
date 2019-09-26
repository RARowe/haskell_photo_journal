{-# LANGUAGE OverloadedStrings #-}
module Router 
  ( router
  ) where

import qualified Database.HDBC as DB (IConnection)
import qualified Network.Wai as W
import Network.HTTP.Types
import qualified Data.Aeson as A (encode)
import qualified Models as MO (FilmRoll(..))
import qualified DataSource as DS (withConnection, get, fromSqlRow)
import qualified Router.Internal as RI (makeRoute, getUri)

router :: W.Application
router req res =
  case W.pathInfo req of
    [] -> res index
    "api":path -> apiRouter req res
    _ -> res notFound

apiRouter :: W.Application
apiRouter req res =
  case RI.getUri $ RI.makeRoute req of
    "api/filmrolls" -> getAllFilmRolls req res
    _ -> res index

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

getAllFilmRolls :: W.Application
getAllFilmRolls req res =  do
  results <- DS.withConnection getAllFilmRollsDb
  res $ W.responseLBS
    status200
    [("Content-Type", "application/json")]
    (A.encode results)

getAllFilmRollsDb :: DB.IConnection conn => conn -> IO [MO.FilmRoll]
getAllFilmRollsDb conn = do
  rows <- DS.get conn "select Title, DateCreated from FilmRoll;" []
  return $ map DS.fromSqlRow rows