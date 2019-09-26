module Router.Internal
  ( makeRoute
  , getUri
  ) where
import qualified Data.Either as E (lefts)
import qualified Data.List as L (intercalate)
import qualified Data.Text as T (unpack)
import qualified Network.Wai as W (Request, pathInfo)

type Route = (String, [Int])

getUri :: Route -> String
getUri (uri, _) = uri

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
