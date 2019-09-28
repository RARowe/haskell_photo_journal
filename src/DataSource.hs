module DataSource
  ( getAllFilmRolls
  , getFilmRoll) where
import qualified Database.HDBC as DB (toSql)
import qualified DataSource.Internal as DSI (withConnection, get, fromSqlRow)
import qualified Models as MO (FilmRoll)

getAllFilmRolls :: IO [MO.FilmRoll]
getAllFilmRolls = DSI.withConnection $ \conn -> do
  rows <- DSI.get conn "select Title, DateCreated from FilmRoll;" []
  return $ map DSI.fromSqlRow rows

getFilmRoll :: Int -> IO MO.FilmRoll
getFilmRoll filmRollId = DSI.withConnection $ \conn -> do
  rows <- DSI.get conn "select Title, DateCreated from FilmRoll where RowId = ?;" [DB.toSql filmRollId]
  return $ DSI.fromSqlRow $ head rows
