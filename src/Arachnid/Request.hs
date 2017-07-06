{-# LANGUAGE OverloadedStrings #-}

module Arachnid.Request
( Request (Request, headers, method, schema, host, target)
, RequestHost (RequestHost, source, hostname, port)
, RequestTarget (RequestTarget, path, query)
, defaultRequest
, fromWai
) where

import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Version as V
import qualified Network.HTTP.Types.URI as U
import qualified Network.Wai as Wai
import           Data.Char
import           Data.Maybe
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8

fromWai :: Wai.Request -> Request
fromWai r = Request { version = Wai.httpVersion r
                    , schema  = schemaOf $ Wai.isSecure r
                    , method  = Wai.requestMethod r
                    , headers = Wai.requestHeaders r
                    , host = parseHost $ Wai.requestHeaderHost r
                    , target  = RequestTarget { path = Wai.pathInfo r, query = Wai.queryString r }
                    }
  where parseHost (Just s) = fromHostPieces s (BS.breakEnd (== fromIntegral (ord ':')) s)
        parseHost Nothing = RequestHost { source = "", hostname = "", port = 80 }
        -- TODO: Different default port based on scheme!
        fromHostPieces s (host, port) = RequestHost { source = s, hostname = host, port = fromMaybe 80 $ fst <$> C8.readInteger port }
        schemaOf True  = "https"
        schemaOf False = "http"


defaultRequest :: Request
defaultRequest = Request { version = V.http11
                         , headers = []
                         , method = M.methodGet
                         , schema = "http"
                         , host = RequestHost { source = "localhost:80", hostname = "localhost", port = 80 }
                         , target = RequestTarget { path = ["/"], query = [] }
                         }

data RequestHost = RequestHost { source   :: BS.ByteString
                               , hostname :: BS.ByteString
                               , port     :: Integer
                               }

data RequestTarget = RequestTarget { path  :: [Text.Text]
                                   , query :: U.Query
                                   }

data Request = Request { version :: V.HttpVersion
                       , method  :: M.Method
                       , schema  :: String
                       , host    :: RequestHost
                       , target  :: RequestTarget
                       , headers :: Header.RequestHeaders
                       }
