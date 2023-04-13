module Weights
( fetchStats
)
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Aeson
import Data.Ord
import Data.List(sortOn, groupBy)
import Control.Arrow((&&&))
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as Map
import GHC.Generics
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import Servant.Server
import Text.Show.Pretty

fetchStats :: Text -> Handler [((UTCTime, UTCTime), Map.Map DayOfWeek Double)]
fetchStats accessToken = do
    fetchLimit <- liftIO $ addUTCTime ((-90) * nominalDay) <$> getCurrentTime
    let statsRequestUrl = "https://wbsapi.withings.net/measure"
        statsRequestBody =
          [ "action" := ("getmeas" :: Text),
            "meastype" := ("1" :: Text),
            "category" := ("1" :: Text),
            "startdate" := round @_ @Integer (nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds fetchLimit)
          ]
        statsRequestOptions =
          Wreq.defaults
            & Wreq.header "Accept" .~ ["application/json"]
            & Wreq.header "Authorization" .~ ["Bearer " <> T.encodeUtf8 accessToken]
    response <- liftIO $ Wreq.postWith statsRequestOptions statsRequestUrl statsRequestBody
    liftIO $ pPrint response
    let orderedWeights :: [(UTCTime, Double)] -> [((UTCTime, UTCTime), Map.Map DayOfWeek Double)]
        orderedWeights =
          map (\xs -> ((minimum &&& maximum) $ map (fst . snd) xs, Map.fromList $ reverse $ map (\(wd, (_, m)) -> (wd, m)) xs))
          . groupBy (\_ x -> fst x /= Sunday)
          . map (dayOfWeek . utctDay . fst &&& id)
          . sortOn (Down . fst)
    case eitherDecode @StatsResponse (response ^. Wreq.responseBody) of
      Left e -> do
        liftIO $ print @(Text, _) ("Cannot parse stats response", e)
        throwError $ err500 { errBody = "Cannot parse stats response" }
      Right r -> do
        unless (r.srStatus == 0) $
          throwError $ err500 { errBody = "Stats error" }
        liftIO $ pPrint r
        return $ orderedWeights $ concatMap zippedWeights r.srBody.srbMeasureGroups

data StatsResponse = StatsResponse
  { srStatus :: Integer,
    srBody :: StatsResponseBody
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON StatsResponse where
  parseJSON =
    withObject "StatsResponse" $ \o ->
      StatsResponse
        <$> o .: "status"
        <*> o .: "body"

data StatsResponseBody = StatsResponseBody
  { srbUpdateTime :: Integer,
    srbTimeZone :: Text,
    srbMeasureGroups :: [MeasureGroup],
    srbMore :: Maybe Integer,
    srbOffset :: Maybe Integer
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON StatsResponseBody where
  parseJSON =
    withObject "StatsResponseBody" $ \o ->
      StatsResponseBody
        <$> o .: "updatetime"
        <*> o .: "timezone"
        <*> o .: "measuregrps"
        <*> optional (o .: "offset")
        <*> optional (o .: "more")

data MeasureGroup = MeasureGroup
  { mgGroupId :: Integer,
    mgAttrib :: Integer, -- filter '0'
    mgMeasureTime :: Integer,
    mgCategory :: Integer, -- filter '1' (input 'category')
    -- mgTimeZone :: Text, -- only in the docs
    mgMeasures :: [Measure]
  }
  deriving stock (Eq, Show, Generic)

zippedWeights :: MeasureGroup -> [(UTCTime, Double)]
zippedWeights mg =
  if mg.mgAttrib == 0 && mg.mgCategory == 1
    then
      zip (repeat $ posixSecondsToUTCTime $ fromInteger mg.mgMeasureTime) $
        map computeMesureValue $ filterWeightMeasures mg.mgMeasures
    else []

instance FromJSON MeasureGroup where
  parseJSON =
    withObject "MeasureGroup" $ \o ->
      MeasureGroup
        <$> o .: "grpid"
        <*> o .: "attrib"
        <*> o .: "date"
        <*> o .: "category"
        -- <*> o .: "timezone"
        <*> o .: "measures"

data Measure = Measure
  { mBaseValue :: Integer,
    mType :: Integer, -- filter '1' (input 'meastype')
    mUnitPow :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Measure where
  parseJSON =
    withObject "Measure" $ \o ->
      Measure
        <$> o .: "value"
        <*> o .: "type"
        <*> o .: "unit"

filterWeightMeasures :: [Measure] -> [Measure]
filterWeightMeasures = filter $ (== 1) . (.mType)

computeMesureValue :: Measure -> Double
computeMesureValue m = fromInteger m.mBaseValue * (10 ** fromInteger m.mUnitPow)
