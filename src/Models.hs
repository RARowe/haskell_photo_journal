{-# LANGUAGE DeriveGeneric #-}
module Models
  ( FilmRoll(..)
  ) where
import qualified Data.Aeson as A (ToJSON)
import GHC.Generics (Generic)
import DataSource.Internal (FromSqlRow(..), readStringFromRow)

data FilmRoll = FilmRoll {
    title :: String
  , dateCreated :: String
  } deriving (Generic, Show)

instance A.ToJSON FilmRoll

instance FromSqlRow FilmRoll where
  fromSqlRow row = FilmRoll { title = readStringFromRow row "Title", dateCreated = readStringFromRow row "DateCreated" }
