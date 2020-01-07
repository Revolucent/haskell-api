{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Network.HTTP.Api (
    Api,
    Connection,
    MonadApi(..),
    call,
    get_,
    getJ,
    getS,
    postJ,
    postJ_,
    putJ,
    req,
    req_,
    reqJ,
    reqS,
    withApi,
    withApiConfig,
    withApiConnection,
    withApiHttp,
    withApiHttps,
    withAuthorization,
    withBearerAuthorization,
    withEndpoint,
    withHeader,
    withOption,
    withPath,
    withPaths,
    withUUID,
    (/->),
    (<-/)
)

where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Proxy (Proxy)
import Data.Semigroup hiding (Option)
import Data.String.Conversions
import Data.Text hiding (pack)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Data.Typeable
import GHC.Generics
import Network.HTTP.Client.MultipartFormData (Part)
import Network.HTTP.Req hiding (req)
import qualified Network.HTTP.Req as Req
import Text.Printf

type Connection scheme = (Url scheme, Option scheme)

data InvalidUrlException = InvalidUrlException ByteString deriving (Show, Typeable)
instance Exception InvalidUrlException

newtype Api scheme a = Api (ReaderT (Connection scheme) (ReaderT HttpConfig IO) a) deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Connection scheme), MonadCatch, MonadThrow, MonadPlus, Alternative)

instance MonadHttp (Api scheme) where
    handleHttpException = throwM
    getHttpConfig = Api $ lift ask 

class (MonadReader (Connection scheme) m, MonadHttp m) => MonadApi scheme m where
    localHttpConfig :: MonadApi scheme m => (HttpConfig -> HttpConfig) -> m a -> m a

instance MonadApi scheme (Api scheme) where
    localHttpConfig t call = Api $ do 
        connection <- ask
        config <- t <$> lift ask
        withApiConfig config connection call

withApiConfig :: MonadIO m => HttpConfig -> Connection scheme -> Api scheme a -> m a
withApiConfig config connection (Api call) = liftIO $ runReaderT (runReaderT call connection) config

withApiConnection :: MonadIO m => Connection scheme -> Api scheme a -> m a
withApiConnection = withApiConfig defaultHttpConfig

withApi :: MonadIO m => Url scheme -> Api scheme a -> m a
withApi url = withApiConnection (url, mempty)

normalize :: ByteString -> ByteString -> ByteString
normalize prefix url = if ByteString.isInfixOf "://" url then url else prefix <> url

withApiHttp :: (MonadIO m, MonadThrow m) => ByteString -> Api Http a -> m a
withApiHttp url call = case parseUrlHttp $ normalize "http://" url of
    Nothing -> throwM $ InvalidUrlException url
    Just connection -> withApiConnection connection call

withApiHttps :: (MonadIO m, MonadThrow m) => ByteString -> Api Https a -> m a
withApiHttps url call = case parseUrlHttps $ normalize "https://" url of
    Nothing -> throwM $ InvalidUrlException url
    Just connection -> withApiConnection connection call

withOption :: MonadReader (Connection scheme) m => Option scheme -> m a -> m a 
withOption option call = local modify call 
    where
        modify (url, options) = (url, options <> option)

withPath :: MonadReader (Connection scheme) m => Text -> m a -> m a
withPath path call = local modify call
    where
        modify (url, options) = (url /: path, options) 

withPaths :: MonadReader (Connection scheme) m => [Text] -> m a -> m a
withPaths [] call = call
withPaths (p:ps) call = withPath p $ withPaths ps call

withUUID :: MonadReader (Connection scheme) m => UUID -> m a -> m a
withUUID = withPath . convertString . UUID.toString 

withHeader :: MonadReader (Connection scheme) m => ByteString -> ByteString -> m a -> m a
withHeader name value = withOption $ header name value

withAuthorization :: MonadReader (Connection scheme) m => ByteString -> m a -> m a
withAuthorization auth = withHeader "Authorization" auth 

withBearerAuthorization :: MonadReader (Connection scheme) m => String -> m a -> m a
withBearerAuthorization = withAuthorization . convertString . ("Bearer " <>) 

withEndpoint :: MonadReader (Connection scheme) m => Text -> m a -> m a
withEndpoint endpoint = withPaths $ splitOn "/" endpoint 

infixl 5 /->

(/->) :: MonadReader (Connection scheme) m => Text -> m a -> m a
(/->) = withEndpoint

infixl 5 <-/

(<-/) :: MonadReader (Connection scheme) m => m a -> Text -> m a
(<-/) = flip withEndpoint

call :: (MonadApi scheme m, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body, HttpResponse response) => method -> body -> Proxy response -> m response 
call method body response = do
    (url, options) <- ask
    Req.req method url body response options

req :: (MonadApi scheme m, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body, HttpResponse response) => method -> body -> Proxy response -> m (HttpResponseBody response) 
req method body response = responseBody <$> call method body response

reqJ :: (MonadApi scheme m, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body, FromJSON a) => method -> body -> m a 
reqJ method body = req method body jsonResponse

reqS :: (MonadApi scheme m, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) => method -> body -> m ByteString 
reqS method body = req method body bsResponse 

req_ :: (MonadApi scheme m, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpMethod method, HttpBody body) => method -> body -> m () 
req_ method body = req method body ignoreResponse 

get_ :: (MonadApi scheme m) => m () 
get_ = req_ GET NoReqBody 

getJ :: (MonadApi scheme m, FromJSON a) => m a 
getJ = reqJ GET NoReqBody

getS :: MonadApi scheme m => m ByteString
getS = reqS GET NoReqBody

postJ :: (MonadApi scheme m, ToJSON up, FromJSON down) => up -> m down
postJ = reqJ POST . ReqBodyJson

postJ_ :: (MonadApi scheme m, ToJSON up) => up -> m ()
postJ_ = req_ POST . ReqBodyJson

putJ :: (MonadApi scheme m, ToJSON up, FromJSON down) => up -> m down
putJ = reqJ PUT . ReqBodyJson 

delete_ :: MonadApi scheme m => m ()
delete_ = req_ DELETE NoReqBody

deleteJ :: (MonadApi scheme m, FromJSON a) => m a 
deleteJ = req DELETE NoReqBody jsonResponse 