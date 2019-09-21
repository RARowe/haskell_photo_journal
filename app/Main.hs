{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Exception (bracket)
import qualified Network.Wai as W
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as A (encode)
import qualified Data.Either as E (lefts)
import qualified Data.List as L (intercalate)
import qualified Data.Text as T (unpack)
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

getAllFilmRolls :: W.Application
getAllFilmRolls req res =  do
  results <- withConnection getAllFilmRollsDb
  res $ W.responseLBS
    status200
    [("Content-Type", "application/json")]
    (A.encode results)

getAllFilmRollsDb :: DB.IConnection conn => conn -> IO [MO.FilmRoll]
getAllFilmRollsDb conn = do
  rows <- DS.get conn "select Title, DateCreated from FilmRoll;" []
  return $ map DS.fromSqlRow rows

router :: W.Application
router req res =
  case W.pathInfo req of
    [] -> res index
    "api":path -> apiRouter req res
    _ -> res notFound

type Route = (String, [Int])

getUri :: Route -> String
getUri (uri, _) = uri

getIds :: Route -> [Int]
getIds (_, ids) = ids

makeRoute :: W.Request -> Route
makeRoute req = (makeRouteUri segments, getRouteIds segments)
  where segments = map (makeRouteSegment . T.unpack) $ W.pathInfo req

makeRouteUri :: [Either Int String] -> String
makeRouteUri segs = L.intercalate "/" $ map foo segs
  where foo seg = case seg of
                    (Left _) -> ":id"
                    (Right val) -> val

getRouteIds :: [Either Int String] -> [Int]
getRouteIds segs = E.lefts segs

makeRouteSegment :: String -> Either Int String
makeRouteSegment seg =
  case readInt seg of
    (Just id) -> Left id
    Nothing -> Right seg

readInt :: String -> Maybe Int
readInt s =
  case reads s of
    [(val, _)] -> Just val
    _ -> Nothing

apiRouter :: W.Application
apiRouter req res =
  case getUri $ makeRoute req of
    "api/filmrolls" -> getAllFilmRolls req res
    _ -> res index

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
