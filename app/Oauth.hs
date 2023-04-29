{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Oauth
  ( withingsOauthUrl,
    OauthEnv,
    fetchOauthTokens,
    withOauthBearer,
    UsersInfo,
    mkUsersInfo,
    UserName (..),
    listUsers,
    FetchTokenResponse (..),
    FetchTokenResponseBody (..),
  )
where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import qualified Env
import qualified Env.Generic as Env
import GHC.Generics
import qualified Network.URI.Encode as URI
import Network.Wreq (FormParam ((:=)))
import qualified Network.Wreq as Wreq
import Servant (FromHttpApiData)
import Servant.Server
import System.Directory (doesFileExist)
import Text.Show.Pretty

fetchOauthTokens :: OauthEnv -> UsersInfo -> Text -> Text -> Handler UserName
fetchOauthTokens env info code state = do
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
  case eitherDecode @FetchTokenResponse (response ^. Wreq.responseBody) of
    Left e -> do
      liftIO $ print @(Text, _) ("Cannot parse OAuth response", e)
      throwError $ err500 {errBody = "Cannot parse OAuth response"}
    Right r -> do
      unless (r.ftrStatus == 0 && isNothing r.ftrError) $
        throwError $
          err500 {errBody = "OAuth response error: '" <> maybe ".." (LBS.fromStrict . T.encodeUtf8) r.ftrError <> "'"}
      body <- maybe (throwError $ err500 {errBody = "No body in oAuth response"}) return r.ftrBody
      let user = UserName state
      upsertUserInfo info user body
      return user

data OauthEnv = OauthEnv
  { oauthClientId :: Text,
    oauthClientSecret :: Text,
    oauthCallbackUrl :: Text,
    oauthStorePath :: FilePath
  }
  deriving stock (Eq, Show, Generic)

instance Env.Record Env.Error OauthEnv

instance Env.Field Env.Error Text where
  field name field = T.pack <$> Env.field name field

withingsOauthUrl :: OauthEnv -> UserName -> Text
withingsOauthUrl env (UserName state) = "https://account.withings.com/oauth2_user/authorize2?response_type=code&client_id=" <> env.oauthClientId <> "&state=" <> state <> "&scope=user.metrics&redirect_uri=" <> URI.encodeText env.oauthCallbackUrl

data FetchTokenResponse = FetchTokenResponse
  { ftrStatus :: Integer,
    ftrBody :: Maybe FetchTokenResponseBody,
    ftrError :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON FetchTokenResponse where
  parseJSON =
    withObject "FetchTokenResponse" $ \o ->
      FetchTokenResponse
        <$> o
        .: "status"
        <*> optional (o .: "body")
        <*> optional (o .: "error")

data FetchTokenResponseBody = FetchTokenResponseBody
  { ftrbAccessToken :: Text,
    ftrbRefreshToken :: Text,
    ftrbExpiresIn :: Integer
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON FetchTokenResponseBody where
  parseJSON =
    withObject "FetchTokenResponseBody" $ \o ->
      FetchTokenResponseBody
        <$> o
        .: "access_token"
        <*> o
        .: "refresh_token"
        <*> o
        .: "expires_in"

newtype UserName = UserName {getUserName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (FromJSONKey, ToJSONKey, FromHttpApiData)

data UsersInfo = UsersInfo
  { storedPath :: FilePath,
    lock :: MVar ()
  }

mkUsersInfo :: OauthEnv -> IO UsersInfo
mkUsersInfo env = UsersInfo env.oauthStorePath <$> newMVar ()

withOauthBearer ::
  OauthEnv ->
  UsersInfo ->
  UserName ->
  (ByteString -> IO (Wreq.Response LBS.ByteString)) ->
  Handler (Wreq.Response LBS.ByteString)
withOauthBearer env info user f = do
  fetchedCredentials <-
    withUsersInfo info $ \users ->
      return (users, Map.lookup user users)

  credentials <-
    case fetchedCredentials of
      Nothing -> throwError $ err404 {errBody = "Unknown user"}
      Just x -> return x

  now <- liftIO getCurrentTime
  accessToken <-
    if credentials.expiresAt > now
      then return credentials.accessToken
      else oauthRefreshToken env info user

  response <- liftIO $ f $ T.encodeUtf8 accessToken
  case decode @FailibleResponse (response ^. Wreq.responseBody) of
    Nothing -> return response
    Just failure ->
      if T.isInfixOf "invalid_token" failure.frError || failure.frStatus == 401
        then oauthRefreshToken env info user >>= liftIO . f . T.encodeUtf8
        else return response

data FailibleResponse = FailibleResponse
  { frStatus :: Int,
    frError :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON FailibleResponse where
  parseJSON =
    withObject "FailibleResponse" $ \o ->
      FailibleResponse
        <$> o
        .: "status"
        <*> o
        .: "error"

type UsersInfoState = Map.Map UserName UserInfo

data UserInfo = UserInfo
  { refreshToken :: Text,
    accessToken :: Text,
    expiresAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

withUsersInfo ::
  UsersInfo ->
  (UsersInfoState -> IO (UsersInfoState, a)) ->
  Handler a
withUsersInfo info f = do
  fetchedUserInfoState <-
    liftIO $
      withMVar info.lock $ \() -> do
        exists <- doesFileExist info.storedPath
        if exists
          then first ("decoding error: " <>) <$> eitherDecodeFileStrict @UsersInfoState info.storedPath
          else return $ Right mempty

  case fetchedUserInfoState of
    Left e -> throwError $ err500 {errBody = "State store: " <> LBS.fromStrict (T.encodeUtf8 $ T.pack $ show e)}
    Right state -> liftIO $ do
      (newState, result) <- f state
      encodeFile info.storedPath newState
      return result

oauthRefreshToken ::
  OauthEnv ->
  UsersInfo ->
  UserName ->
  Handler Text
oauthRefreshToken env info user = do
  fetchedCredentials <-
    withUsersInfo info $ \users ->
      return (users, Map.lookup user users)

  credentials <-
    case fetchedCredentials of
      Nothing -> throwError $ err404 {errBody = "Unknown user"}
      Just x -> return x

  let oauthRequestUrl = "https://wbsapi.withings.net/v2/oauth2"
      oauthRequestBody =
        [ "action" := ("requesttoken" :: Text),
          "grant_type" := ("refresh_token" :: Text),
          "client_id" := env.oauthClientId,
          "client_secret" := env.oauthClientSecret,
          "refresh_token" := credentials.refreshToken
        ]
      oauthRequestOptions = Wreq.defaults & Wreq.header "Accept" .~ ["application/json"]
  response <- liftIO $ Wreq.postWith oauthRequestOptions oauthRequestUrl oauthRequestBody
  liftIO $ pPrint response
  case eitherDecode @FetchTokenResponse (response ^. Wreq.responseBody) of
    Left e -> do
      liftIO $ print @(Text, _) ("Cannot parse OAuth response", e)
      throwError $ err500 {errBody = "Cannot parse OAuth response"}
    Right r -> do
      unless (r.ftrStatus == 0 && isNothing r.ftrError) $
        throwError $
          err500 {errBody = "OAuth response error: '" <> maybe ".." (LBS.fromStrict . T.encodeUtf8) r.ftrError <> "'"}
      body <- maybe (throwError $ err500 {errBody = "No body in oAuth response"}) return r.ftrBody
      upsertUserInfo info user body
      return body.ftrbAccessToken

upsertUserInfo ::
  UsersInfo ->
  UserName ->
  FetchTokenResponseBody ->
  Handler ()
upsertUserInfo info user r = do
  now <- liftIO getCurrentTime
  let userInfo = UserInfo r.ftrbRefreshToken r.ftrbAccessToken $ addUTCTime (secondsToNominalDiffTime $ fromInteger r.ftrbExpiresIn) now
  withUsersInfo info $ \users ->
    return (Map.insert user userInfo users, ())

listUsers ::
  UsersInfo ->
  Handler [UserName]
listUsers info =
  withUsersInfo info $ \users ->
    return (users, Map.keys users)
