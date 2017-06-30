{-# LANGUAGE OverloadedStrings #-}

module Arachnid.Response
( ResponseData
, emptyResponse
, responseHeaders
, addHeaders
, addHeader
, getHeader
, setBody
, hasBody
, body
, setChosenContentType
, chosenContentType
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Media as MT
import Data.Time (UTCTime)
import Arachnid.Internal.Date (formatHttpDate)

instance MT.RenderHeader UTCTime where
  renderHeader = formatHttpDate

data ResponseData = ResponseData { headers :: Header.ResponseHeaders
                                 , body    :: Maybe LBS.ByteString
                                 , chosenContentType :: Maybe MT.MediaType
                                 } deriving (Show)

emptyResponse = ResponseData { headers = [], body = Nothing, chosenContentType = Nothing }

responseHeaders :: ResponseData -> Header.ResponseHeaders
responseHeaders = headers

addHeaders :: Header.ResponseHeaders -> ResponseData -> ResponseData
addHeaders hs d = d { headers = hs ++ headers d }

addHeader :: (MT.RenderHeader v) => Header.HeaderName -> v -> ResponseData -> ResponseData
addHeader n h d = d { headers = (n, MT.renderHeader h) : headers d }

getHeader :: Header.HeaderName -> ResponseData -> Maybe BS.ByteString
getHeader h d = snd `fmap` find ((==h) . fst) (headers d)

setBody :: LBS.ByteString -> ResponseData -> ResponseData
setBody b d = d { body = Just b }

hasBody ::  ResponseData -> Bool
hasBody d = isJust $ body d

setChosenContentType :: Maybe MT.MediaType -> ResponseData -> ResponseData
setChosenContentType ct d = d { chosenContentType = ct }
