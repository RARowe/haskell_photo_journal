module DataSource
    ( SqlPair
    , SqlRow
    , readStringFromRow
    , get
    , retrieveSingleInt
    , retrieveSingleBool
    , runBatchAndCommit
    , FromSqlRow(..)
    , withConnection
    ) where
import qualified Control.Exception as E (bracket)
import qualified Control.Monad as C (mapM_)
import Data.Maybe (Maybe)
import qualified Database.HDBC as DB
import qualified Database.HDBC.SqlValue as SV (fromSql)
import qualified Database.HDBC.Sqlite3 as S (connectSqlite3)

type SqlPair = (String, [DB.SqlValue])
type SqlRow = [(String, DB.SqlValue)]
class FromSqlRow a where
  fromSqlRow :: SqlRow -> a

-- This needs to be changed
withConnection operation = E.bracket
  (S.connectSqlite3 "photo_journal.db")
  (DB.disconnect)
  (operation)

readStringFromRow :: SqlRow -> String -> String
readStringFromRow row columnName = SV.fromSql $ findInRow row columnName

findInRow :: SqlRow -> String -> DB.SqlValue
findInRow row columnName = snd $ unsafeFind predicate row
  where
    predicate sqlPair = (fst sqlPair) == columnName

unsafeFind :: (a -> Bool) -> [a] -> a
unsafeFind pred [] = error "The thing you were looking for does not exist."
unsafeFind pred (row:rows)
  | pred row = row
  | otherwise = unsafeFind pred rows

get :: (DB.IConnection conn) => conn -> String -> [DB.SqlValue] -> IO [SqlRow]
get conn sql params = do
  statement <- DB.prepare conn sql
  DB.execute statement params
  DB.fetchAllRowsAL' statement

runBatchAndCommit :: DB.IConnection conn => conn -> [SqlPair] -> IO ()
runBatchAndCommit conn sqlPairs = runBatch conn sqlPairs >> DB.commit conn

runBatch :: DB.IConnection conn => conn -> [SqlPair] -> IO ()
runBatch conn sqlPairs = C.mapM_ (run conn) sqlPairs

run :: DB.IConnection conn => conn -> SqlPair -> IO Integer
run conn (sql, sqlParams) = DB.run conn sql sqlParams

executeAndCommitSql :: DB.IConnection conn => conn -> String -> IO ()
executeAndCommitSql conn sql = DB.runRaw conn sql >> DB.commit conn

retrieveSingleInt :: DB.IConnection conn => conn -> String -> IO (Maybe Int)
retrieveSingleInt conn sql = retrieveSingleValue conn sql >>= \i -> return $ fmap DB.fromSql i

retrieveSingleBool :: DB.IConnection conn => conn -> String -> IO (Maybe Bool)
retrieveSingleBool conn sql = retrieveSingleValue conn sql >>= \b -> return $ fmap DB.fromSql b

retrieveSingleValue :: DB.IConnection conn => conn -> String -> IO (Maybe DB.SqlValue)
retrieveSingleValue conn sql = fmap firstValue $ DB.quickQuery conn sql []
  where firstValue vs
          | vs == [] = Nothing
          | head vs == [] = Nothing
          | otherwise = Just $ (head . head) vs
