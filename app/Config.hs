{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (groupBy, intersperse, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX
import qualified Env as Env
import qualified Env.Generic as Env
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import qualified Network.URI.Encode as URI
import Network.Wai.Handler.Warp
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import Servant.API
import Servant.Server
import Text.Printf
import Text.Show.Pretty

main :: IO ()
main = Env.parse (Env.header "Withings Average Weeks Weight") Env.record >>= run 5555 . app

type API = NamedRoutes Routes

data Routes mode = Routes
  { home ::
      mode :- Get '[HTML] Text,
    oauth ::
      mode
        :- "oauth"
        :> QueryParam' '[Required] "code" Text
        :> QueryParam' '[Required] "state" Text
        :> Get '[HTML] Text,
    stats ::
      mode
        :- "stats"
        :> QueryParam' '[Required] "accessToken" Text
        :> Get '[HTML] Text
  }
  deriving (Generic)

app :: WithingsEnv -> Application
app env =
  serve (Proxy @API) $
    Routes
      { home = homeHandler env,
        oauth = oauthHandler env,
        stats = statsHandler env
      }

homeHandler :: WithingsEnv -> Handler Text
homeHandler env =
  return $
    mconcat
      [ "<html>",
        "  <head>",
        "    <title>Home page</title>",
        "  </head>",
        "  <body>",
        "    <p><a href=\"" <> withingsOauthUrl env <> "\">Ask for Withings connection</a></p>",
        "  </body>",
        "</html>"
      ]

oauthHandler :: WithingsEnv -> Text -> Text -> Handler Text
oauthHandler env code state = do
  when (state /= oauthState) $
    throwError $
      err400 {errBody = "Wrong state"}
  let oauthRequestUrl = "https://wbsapi.withings.net/v2/oauth2"
      oauthRequestBody =
        [ "action" := ("requesttoken" :: Text),
          "grant_type" := ("authorization_code" :: Text),
          "client_id" := env . oauthClientId,
          "client_secret" := env . oauthClientSecret,
          "code" := code,
          "redirect_uri" := env . oauthCallbackUrl
        ]
      oauthRequestOptions = Wreq.defaults & Wreq.header "Accept" .~ ["application/json"]
  response <- liftIO $ Wreq.postWith oauthRequestOptions oauthRequestUrl oauthRequestBody
  liftIO $ pPrint response
  (accessToken, refreshToken) <-
    case eitherDecode @RequestTokenResponse (response ^. Wreq.responseBody) of
      Left e -> do
        liftIO $ print @(Text, _) ("Cannot parse OAuth response", e)
        throwError $ err500 {errBody = "Cannot parse OAuth response"}
      Right r -> do
        unless (r . rtrStatus == 0 && isNothing r . rtrError) $
          throwError $
            err500 {errBody = "OAuth response error: '" <> maybe ".." (B.fromStrict . T.encodeUtf8) r . rtrError <> "'"}
        body <- maybe (throwError $ err500 {errBody = "No body in oAuth response"}) return r . rtrBody
        return (body . rtrbAccessToken, body . rtrbRefreshToken)

  return $
    mconcat
      [ "<html>",
        "  <head>",
        "    <title>Loged-in</title>",
        "  </head>",
        "  <body>",
        "    <p><a href=\"/stats?accessToken=" <> accessToken <> "&refreshToken=" <> refreshToken <> "\">Check your stats!</a></p>",
        "  </body>",
        "</html>"
      ]

statsHandler :: WithingsEnv -> Text -> Handler Text
statsHandler _env accessToken = do
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
  groupedWeights <-
    case eitherDecode @StatsResponse (response ^. Wreq.responseBody) of
      Left e -> do
        liftIO $ print @(Text, _) ("Cannot parse stats response", e)
        throwError $ err500 {errBody = "Cannot parse stats response"}
      Right r -> do
        unless (r . srStatus == 0) $
          throwError $
            err500 {errBody = "Stats error"}
        liftIO $ pPrint r
        return $ orderedWeights $ concatMap zippedWeights r . srBody . srbMeasureGroups
  let renderWeight :: ((UTCTime, UTCTime), Map.Map DayOfWeek Double) -> String
      renderWeight ((start, end), measures) =
        let measures' = Map.elems measures
            avg :: Double
            avg = sum measures' / fromIntegral (length measures')
            fmtDay :: UTCTime -> String
            fmtDay = formatTime defaultTimeLocale "%m/%d"
            fmtWeight :: Double -> String
            fmtWeight = printf "%.2f"
            dailyWeights :: String
            dailyWeights = concat $ intersperse " " $ map (\(d, w) -> fmtWeight w <> "kg [" <> take 1 (show d) <> "]") $ Map.toList measures
         in "    <li><b>" <> fmtDay start <> " - " <> fmtDay end <> " (" <> fmtWeight avg <> ")</b>: " <> dailyWeights <> "</li>"
  return $
    mconcat
      [ "<html>",
        "  <head>",
        "    <title>Your stats</title>",
        "  </head>",
        "  <body>",
        "    <h1>Your weights:</h1>",
        "    <ul>",
        T.pack $ concatMap renderWeight groupedWeights,
        "    </ul>",
        "  </body>",
        "</html>"
      ]

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ = B.fromStrict . T.encodeUtf8

data WithingsEnv = WithingsEnv
  { oauthClientId :: Text,
    oauthClientSecret :: Text,
    oauthCallbackUrl :: Text
  }
  deriving stock (Eq, Show, Generic)

instance Env.Record Env.Error WithingsEnv

instance Env.Field Env.Error Text

oauthState :: Text
oauthState = "dontevencare"

withingsOauthUrl :: WithingsEnv -> Text
withingsOauthUrl env = "https://account.withings.com/oauth2_user/authorize2?response_type=code&client_id=" <> env . oauthClientId <> "&state=" <> oauthState <> "&scope=user.metrics&redirect_uri=" <> URI.encodeText env . oauthCallbackUrl

data RequestTokenResponse = RequestTokenResponse
  { rtrStatus :: Integer,
    rtrBody :: Maybe RequestTokenResponseBody,
    rtrError :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON RequestTokenResponse where
  parseJSON =
    withObject "RequestTokenResponse" $ \o ->
      RequestTokenResponse
        <$> o
        .: "status"
        <*> optional (o .: "body")
        <*> optional (o .: "error")

data RequestTokenResponseBody = RequestTokenResponseBody
  { rtrbUserId :: Text,
    rtrbAccessToken :: Text,
    rtrbRefreshToken :: Text,
    rtrbScope :: Text,
    rtrbExpiresIn :: Integer,
    rtrbTokenType :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON RequestTokenResponseBody where
  parseJSON =
    withObject "RequestTokenResponseBody" $ \o ->
      RequestTokenResponseBody
        <$> o
        .: "userid"
        <*> o
        .: "access_token"
        <*> o
        .: "refresh_token"
        <*> o
        .: "scope"
        <*> o
        .: "expires_in"
        <*> o
        .: "token_type"

data StatsResponse = StatsResponse
  { srStatus :: Integer,
    srBody :: StatsResponseBody
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON StatsResponse where
  parseJSON =
    withObject "StatsResponse" $ \o ->
      StatsResponse
        <$> o
        .: "status"
        <*> o
        .: "body"

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
        <$> o
        .: "updatetime"
        <*> o
        .: "timezone"
        <*> o
        .: "measuregrps"
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
  if mg . mgAttrib == 0 && mg . mgCategory == 1
    then
      zip (repeat $ posixSecondsToUTCTime $ fromInteger mg . mgMeasureTime) $
        map computeMesureValue $
          filterWeightMeasures mg . mgMeasures
    else []

instance FromJSON MeasureGroup where
  parseJSON =
    withObject "MeasureGroup" $ \o ->
      MeasureGroup
        <$> o
        .: "grpid"
        <*> o
        .: "attrib"
        <*> o
        .: "date"
        <*> o
        .: "category"
        -- <*> o .: "timezone"
        <*> o
        .: "measures"

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
        <$> o
        .: "value"
        <*> o
        .: "type"
        <*> o
        .: "unit"

filterWeightMeasures :: [Measure] -> [Measure]
filterWeightMeasures = filter $ (== 1) . (. mType)

computeMesureValue :: Measure -> Double
computeMesureValue m = fromInteger m . mBaseValue * (10 ** fromInteger m . mUnitPow)
