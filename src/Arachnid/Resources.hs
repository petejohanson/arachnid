{-# LANGUAGE OverloadedStrings #-}

module Arachnid.Resources
( ResourceMonad
, Resource
, serviceAvailable
, knownMethods
, requestURITooLong
, allowedMethods
, malformedRequest
, authorized
, forbidden
, validContentHeaders
, knownContentType
, requestEntityTooLarge
, options
, contentTypesProvided
, languageAvailable
, charsetsProvided
, Responsible
, toResponse
) where

import Data.Word
import Data.Text
import Data.ByteString
import Control.Monad.Reader
import Control.Monad.Trans.Resource

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as MT

type ResourceMonad = ReaderT Wai.Request (ResourceT IO)

http_1_1_Methods :: [HTTP.Method]
http_1_1_Methods = [ "GET"
                   , "HEAD"
                   , "POST"
                   , "PUT"
                   , "DELETE"
                   , "CONNECT"
                   , "OPTIONS"
                   , "TRACE"
                   ]

class (Show a) => Resource a where
  serviceAvailable :: a -> ResourceMonad Bool
  serviceAvailable _ = return True

  knownMethods :: a -> ResourceMonad [HTTP.Method]
  knownMethods = const $ return http_1_1_Methods

  requestURITooLong :: a -> ResourceMonad Bool
  requestURITooLong = const $ return False

  allowedMethods :: a -> ResourceMonad [HTTP.Method]
  allowedMethods = const $ return [ "GET", "HEAD" ]

  malformedRequest :: a -> ResourceMonad Bool
  malformedRequest = const $ return False

  authorized :: a -> ResourceMonad Bool
  authorized = const $ return True

  forbidden :: a -> ResourceMonad Bool
  forbidden = const $ return False

  validContentHeaders :: a -> ResourceMonad Bool
  validContentHeaders = const $ return True

  knownContentType :: a -> ResourceMonad Bool
  knownContentType = const $ return True

  requestEntityTooLarge :: a -> ResourceMonad Bool
  requestEntityTooLarge = const $ return False

  options :: a -> ResourceMonad HTTP.ResponseHeaders
  options = const $ return []

  contentTypesProvided :: a -> ResourceMonad [(MT.MediaType, ResourceMonad (IO ByteString))]
  contentTypesProvided = const $ return []

  languageAvailable :: ByteString -> a -> ResourceMonad Bool
  languageAvailable = const $ const $ return True

  charsetsProvided :: a -> ResourceMonad (Maybe [(ByteString, Word8 -> Word8)])
  charsetsProvided = const $ return Nothing

class Responsible a where
  toResponse :: a -> ResourceMonad Wai.Response

instance Responsible HTTP.Status where
  toResponse s =
    return $ Wai.responseLBS s [("Content-Type", "text/plain")] "Hello World"

