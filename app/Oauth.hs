{-# LANGUAGE MultiParamTypeClasses #-}

module Oauth
  ( withingsOauthUrl,
    OauthEnv,
    fetchOauthTokens,
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Env
import qualified Env.Generic as Env
import GHC.Generics
import qualified Network.URI.Encode as URI
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import Servant.Server
import Text.Show.Pretty

checkOauthState :: Text -> Bool
checkOauthState = (== oauthState)

fetchOauthTokens :: OauthEnv -> Text -> Text -> Handler (Text, Text)
fetchOauthTokens env code state = do
  unless (checkOauthState state) $
    throwError $
      err400 {errBody = "Wrong state"}
  let oauthRequestUrl = "https://wbsapi.withings.net/v2/oauth2"
      oauthRequestBody =
        [ "action" := ("requesttoken" :: Text),
          "grant_type" := ("authorization_code" :: Text),
          "client_id" := env.oauthClientId,
          "client_secret" := env.oauthClientSecret,
          "code" := code,
          "redirect_uri" := env.oauthCallbackUrl
        ]
      oauthRequestOptions = Wreq.defaults & Wreq.header "Accept" .~ ["application/json"]
  response <- liftIO $ Wreq.postWith oauthRequestOptions oauthRequestUrl oauthRequestBody
  liftIO $ pPrint response
  case eitherDecode @RequestTokenResponse (response ^. Wreq.responseBody) of
    Left e -> do
      liftIO $ print @(Text, _) ("Cannot parse OAuth response", e)
      throwError $ err500 {errBody = "Cannot parse OAuth response"}
    Right r -> do
      unless (r.rtrStatus == 0 && isNothing r.rtrError) $
        throwError $
          err500 {errBody = "OAuth response error: '" <> maybe ".." (B.fromStrict . T.encodeUtf8) r.rtrError <> "'"}
      body <- maybe (throwError $ err500 {errBody = "No body in oAuth response"}) return r.rtrBody
      return (body.rtrbAccessToken, body.rtrbRefreshToken)

data OauthEnv = OauthEnv
  { oauthClientId :: Text,
    oauthClientSecret :: Text,
    oauthCallbackUrl :: Text
  }
  deriving stock (Eq, Show, Generic)

instance Env.Record Env.Error OauthEnv

instance Env.Field Env.Error Text

oauthState :: Text
oauthState = "dontevencare"

withingsOauthUrl :: OauthEnv -> Text
withingsOauthUrl env = "https://account.withings.com/oauth2_user/authorize2?response_type=code&client_id=" <> env.oauthClientId <> "&state=" <> oauthState <> "&scope=user.metrics&redirect_uri=" <> URI.encodeText env.oauthCallbackUrl

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
