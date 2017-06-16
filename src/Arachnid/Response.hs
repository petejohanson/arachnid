
module Arachnid.Response
( ResponseData
, emptyResponse
, responseHeaders
, addHeader
, getHeader
, hasBody
, body
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe
import qualified Network.HTTP.Types.Header as Header

data ResponseData = ResponseData { headers :: Header.ResponseHeaders
                                 , body    :: Maybe LBS.ByteString
                                 } deriving (Show)

emptyResponse = ResponseData { headers = [], body = Nothing }

responseHeaders :: ResponseData -> (Header.ResponseHeaders, ResponseData)
responseHeaders d = (headers d, d)

addHeader :: Header.Header -> ResponseData -> ((), ResponseData)
addHeader h d = ((), d { headers = (h:headers d) })

getHeader :: Header.HeaderName -> ResponseData -> (Maybe BS.ByteString, ResponseData)
getHeader h d = (snd `fmap` find ((==h) . fst) (headers d), d)

setBody :: LBS.ByteString -> ResponseData -> ((), ResponseData)
setBody b d = ((), d { body = Just b })

hasBody ::  ResponseData -> (Bool, ResponseData)
hasBody d = (isJust $ body d, d)
