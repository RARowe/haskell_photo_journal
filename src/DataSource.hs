module DataSource
    ( SqlPair
    , retrieveSingleInt
    , retrieveSingleBool
    , runBatchAndCommit
    ) where
import qualified Control.Monad as C (mapM_)
import Data.Maybe (Maybe)
import qualified Database.HDBC as DB

type SqlPair = (String, [DB.SqlValue])

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
