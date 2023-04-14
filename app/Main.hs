{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.List (intersperse)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Env as Env
import qualified Env.Generic as Env
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp
import Oauth
import Servant.API
import Servant.Server
import Text.Printf
import Weights

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

app :: OauthEnv -> Application
app env =
  serve (Proxy @API) $
    Routes
      { home = homeHandler env,
        oauth = oauthHandler env,
        stats = statsHandler env
      }

homeHandler :: OauthEnv -> Handler Text
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

oauthHandler :: OauthEnv -> Text -> Text -> Handler Text
oauthHandler env code state = do
  (accessToken, refreshToken) <- fetchOauthTokens env code state

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

statsHandler :: OauthEnv -> Text -> Handler Text
statsHandler _env accessToken = do
  groupedWeights <- fetchStats accessToken
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
