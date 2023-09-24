{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import Data.Fixed
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Env
import qualified Env.Generic as Env
import GHC.Exts
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Prometheus
import Oauth
import Prometheus
import Prometheus.Metric.GHC
import Prometheus.Servant
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import System.IO
import Text.Printf
import Weights

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  (serverEnv, oauthEnv) <- Env.parse (Env.header "Withings Average Weeks Weight") $ (,) <$> Env.record <*> Env.record
  info <- mkUsersInfo oauthEnv
  putStrLn $ "Listening on " <> show serverEnv.serverPort
  -- Metrics
  register ghcMetrics
  metrics <-
    WithingsMetrics
      <$> register (vector "username" $ gauge (Info "withings_last_checked" "Last time a User checked his/her stats"))
      <*> register (vector "username" $ gauge (Info "withings_last_weight" "Last User weight"))
      <*> register (counter (Info "withings_users" "Users count"))

  addCounter metrics.users . fromIntegral . length =<< runHandler (listUsers info)

  let servantPMW = prometheusMiddleware defaultMetrics $ Proxy @API
  run serverEnv.serverPort $ prometheus def $ servantPMW $ app serverEnv oauthEnv info metrics

type API = NamedRoutes Routes

data Routes mode = Routes
  { home ::
      mode :- Get '[HTML] Text,
    newUser ::
      mode
        :- "newUser"
        :> QueryParam' '[Required] "user" UserName
        :> Get '[HTML] Text,
    oauth ::
      mode
        :- "oauth"
        :> QueryParam' '[Required] "code" Text
        :> QueryParam' '[Required] "state" Text
        :> Get '[HTML] Text,
    stats ::
      mode
        :- "stats"
        :> QueryParam' '[Required] "user" UserName
        :> Get '[HTML] Text,
    assets ::
      mode
        :- "assets"
        :> Raw
  }
  deriving (Generic)

data ServerEnv = ServerEnv
  { serverAssetsPath :: FilePath,
    serverPort :: Int
  }
  deriving stock (Eq, Show, Generic)

instance Env.Record Env.Error ServerEnv

app :: ServerEnv -> OauthEnv -> UsersInfo -> WithingsMetrics -> Application
app serverEnv oauthEnv info metrics =
  serve (Proxy @API) $
    Routes
      { home = homeHandler info,
        newUser = prepareOauthHandler oauthEnv,
        oauth = retrieveOauthHandler oauthEnv info metrics,
        stats = statsHandler oauthEnv info metrics,
        assets = serveDirectoryWebApp serverEnv.serverAssetsPath
      }

homeHandler :: UsersInfo -> Handler Text
homeHandler info = do
  users <- listUsers info
  return $
    T.unlines
      [ "<html>",
        "  <head>",
        "    <title>Users</title>",
        cssHeaders,
        "  </head>",
        "  <body>",
        "    <h1>Users</h1>",
        "    <p>",
        "      <ul>",
        foldMap
          (\u -> "    <li><a class=\"pure-menu-link\" href=\"stats?user=" <> u.getUserName <> "\">" <> u.getUserName <> "</a></li>")
          users,
        "      </ul>",
        "    </p>",
        "    <p>",
        "      <form action=\"/newUser\" method=\"GET\" class=\"pure-form\">",
        "        <label for=\"user\">User name</label>",
        "        <input name=\"user\" id=\"user\" />",
        "        <button class=\"pure-button\">Add a user</button>",
        "      </form>",
        "    </p>",
        "  </body>",
        "</html>"
      ]

prepareOauthHandler :: OauthEnv -> UserName -> Handler Text
prepareOauthHandler env user = do
  when (user.getUserName == "") $
    throwError $
      err400 {errBody = "Username should not be empty"}

  return $
    T.unlines
      [ "<html>",
        "  <head>",
        "    <title>Sign-in</title>",
        cssHeaders,
        "  </head>",
        "  <body>",
        "    <h1>Sign-in</h1>",
        "    <p><a  " <> cssClassLinkButton <> "href=\"" <> withingsOauthUrl env user <> "\">Ask for Withings connection</a></p>",
        "  </body>",
        "</html>"
      ]

retrieveOauthHandler :: OauthEnv -> UsersInfo -> WithingsMetrics -> Text -> Text -> Handler Text
retrieveOauthHandler env info metrics code state = do
  user <- fetchOauthTokens env info code state
  liftIO $ incCounter metrics.users

  return $
    T.unlines
      [ "<html>",
        "  <head>",
        "    <title>Signed-in</title>",
        cssHeaders,
        "  </head>",
        "  <body>",
        "    <h1>Signed-in</h1>",
        "    <p><a  " <> cssClassLinkButton <> "href=\"/stats?user=" <> user.getUserName <> "\">Check your stats!</a></p>",
        "  </body>",
        "</html>"
      ]

statsHandler :: OauthEnv -> UsersInfo -> WithingsMetrics -> UserName -> Handler Text
statsHandler env info metrics user = do
  groupedWeights <- fetchStats $ withOauthBearer env info user
  let fromFixed :: (Fractional a, HasResolution b) => Fixed b -> a
      fromFixed fv@(MkFixed v) = (fromIntegral v) / (fromIntegral $ resolution fv)
  nowGaugeValue <- fromFixed . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
  liftIO $
    withLabel metrics.lastChecked user.getUserName $ \metric ->
      setGauge metric nowGaugeValue
  let withWeekDay :: (Monoid a, IsString a) => (DayOfWeek -> a) -> a
      withWeekDay f = foldMap (\d -> "<td>" <> f d <> "</td>") [Monday .. Sunday]
      renderWeight :: ((UTCTime, UTCTime), Map.Map DayOfWeek Double) -> String
      renderWeight ((start, end), measures) =
        let measures' = Map.elems measures
            avg :: Double
            avg = sum measures' / fromIntegral (length measures')
            fmtDay :: UTCTime -> String
            fmtDay = formatTime defaultTimeLocale "%m/%d"
            fmtWeight :: Double -> String
            fmtWeight = printf "%.2f"
            dailyWeights :: String
            dailyWeights = withWeekDay (\d -> maybe "" (\w -> fmtWeight w <> "kg") $ Map.lookup d measures)
         in mconcat
              [ "          <tr>",
                "<td>" <> fmtDay start <> "</td>",
                "<td>" <> fmtDay end <> "</td>",
                "<td><b>" <> fmtWeight avg <> "kg</b></td>",
                dailyWeights,
                "</tr>"
              ]
  return $
    T.unlines
      [ "<html>",
        "  <head>",
        "    <title>Your stats</title>",
        cssHeaders,
        "  </head>",
        "  <body>",
        "    <h1>Your weights:</h1>",
        "    <p>",
        "      <table class=\"mq-table pure-table-horizontal pure-table\">",
        "        <thead><tr><th>Start</th><th>End</th><th>Average</th>" <> withWeekDay (T.pack . show) <> "</tr></thead>",
        "        <tbody>",
        T.pack $ concatMap renderWeight groupedWeights,
        "        </tbody>",
        "      </table>",
        "    </p>",
        "    <p><a  " <> cssClassLinkButton <> "href=\"/\">Go back home</a></p>",
        "  </body>",
        "</html>"
      ]

cssHeaders :: Text
cssHeaders =
  T.unlines
    [ "    <link rel=\"stylesheet\" href=\"/assets/purecss-3.0.0/pure-min.css\">",
      "    <link rel=\"stylesheet\" href=\"/assets/purecss-3.0.0/grids-responsive-min.css\">",
      "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
      "    <link rel=\"stylesheet\" href=\"/assets/style.css\">"
    ]

cssClassLinkButton :: Text
cssClassLinkButton = "class=\"pure-button pure-button-primary\""

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
  mimeRender _ = B.fromStrict . T.encodeUtf8

data WithingsMetrics = WithingsMetrics
  { lastChecked :: Vector Text Gauge,
    lastWeight :: Vector Text Gauge,
    users :: Counter
  }
