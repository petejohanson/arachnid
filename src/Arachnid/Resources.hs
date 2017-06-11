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
, encodingsProvided
, resourceExists
, generateETag
, lastModified
, deleteResource
, deleteCompleted
, Responsible
, toResponse
) where

import Data.Word
import Data.Text
import Data.Time
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

  encodingsProvided :: a -> ResourceMonad (Maybe [(ByteString, Word8 -> Word8)])
  encodingsProvided = const $ return Nothing

  resourceExists :: a -> ResourceMonad Bool
  resourceExists = const $ return True

  generateETag :: a -> ResourceMonad (Maybe ByteString)
  generateETag = const $ return Nothing

  lastModified :: a -> ResourceMonad (Maybe UTCTime)
  lastModified = const $ return Nothing

  deleteResource :: a -> ResourceMonad Bool
  deleteResource = const $ return True

  deleteCompleted :: a -> ResourceMonad Bool
  deleteCompleted = const $ return True

class Responsible a where
  toResponse :: a -> ResourceMonad Wai.Response

instance Responsible HTTP.Status where
  toResponse s =
    return $ Wai.responseLBS s [("Content-Type", "text/plain")] "Hello World"

