{-# LANGUAGE OverloadedStrings #-}

module Arachnid.Response
( ResponseData
, emptyResponse
, responseHeaders
, HeaderValue
, addHeaders
, addHeader
, getHeader
, setBody
, hasBody
, body
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as M
import Data.Time (UTCTime)
import Arachnid.Internal.Date (formatHttpDate)

class HeaderValue a where
  toHeader :: a -> BS.ByteString

instance (HeaderValue a) => HeaderValue [a] where
  toHeader values = BS.concat $ intersperse ", " $ map toHeader values

instance HeaderValue BS.ByteString where
  toHeader m = m

instance HeaderValue UTCTime where
  toHeader = formatHttpDate

data ResponseData = ResponseData { headers :: Header.ResponseHeaders
                                 , body    :: Maybe LBS.ByteString
                                 } deriving (Show)

emptyResponse = ResponseData { headers = [], body = Nothing }

responseHeaders :: ResponseData -> Header.ResponseHeaders
responseHeaders = headers

addHeaders :: Header.ResponseHeaders -> ResponseData -> ResponseData
addHeaders hs d = d { headers = hs ++ headers d }

addHeader :: (HeaderValue v) => Header.HeaderName -> v -> ResponseData -> ResponseData
addHeader n h d = d { headers = (n, toHeader h) : headers d }

getHeader :: Header.HeaderName -> ResponseData -> Maybe BS.ByteString
getHeader h d = snd `fmap` find ((==h) . fst) (headers d)

setBody :: LBS.ByteString -> ResponseData -> ResponseData
setBody b d = d { body = Just b }

hasBody ::  ResponseData -> Bool
hasBody d = isJust $ body d
