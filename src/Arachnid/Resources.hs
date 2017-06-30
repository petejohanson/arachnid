{-# LANGUAGE OverloadedStrings #-}

module Arachnid.Resources
( ResourceMonad
, Resource
, ResourceResult
, ResourceMonadResult
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
, exists
, generateETag
, lastModified
, expires
, deleteResource
, deleteCompleted
, previouslyExisted
, movedPermanently
, movedTemporarily
, isConflict
, contentTypesAccepted
, hasResponseBody
, multipleChoices
, allowMissingPost
, Responsible
, toResponse
) where

import Arachnid.Response

import Data.Word
import Data.Text
import Data.Time
import Data.ByteString
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media as MT

type ResourceMonad = ReaderT Wai.Request (StateT ResponseData (ResourceT IO))

type ResourceResult v = Either HTTP.Status v

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

type ResourceMonadResult v = ResourceMonad (ResourceResult v)

class (Show a) => Resource a where
  serviceAvailable :: a -> ResourceMonadResult Bool
  serviceAvailable _ = return (return True)

  knownMethods :: a -> ResourceMonadResult [HTTP.Method]
  knownMethods = const $ return (return http_1_1_Methods)

  requestURITooLong :: a -> ResourceMonadResult Bool
  requestURITooLong = const $ return (return False)

  allowedMethods :: a -> ResourceMonadResult [HTTP.Method]
  allowedMethods = const $ return (return ["GET", "HEAD"])

  malformedRequest :: a -> ResourceMonadResult Bool
  malformedRequest = const $ return (return False)

  authorized :: a -> ResourceMonadResult Bool
  authorized = const $ return (return True)

  forbidden :: a -> ResourceMonadResult Bool
  forbidden = const $ return (return False)

  validContentHeaders :: a -> ResourceMonadResult Bool
  validContentHeaders = const $ return (return True)

  knownContentType :: a -> ResourceMonadResult Bool
  knownContentType = const $ return (return True)

  requestEntityTooLarge :: a -> ResourceMonadResult Bool
  requestEntityTooLarge = const $ return (return False)

  options :: a -> ResourceMonadResult HTTP.ResponseHeaders
  options = const $ return (return [])

  contentTypesProvided :: a -> ResourceMonadResult [(MT.MediaType, IO LBS.ByteString)]
  contentTypesProvided = const $ return (return [])

  languageAvailable :: ByteString -> a -> ResourceMonadResult Bool
  languageAvailable = const $ const $ return (return True)

  charsetsProvided :: a -> ResourceMonadResult (Maybe [(ByteString, Word8 -> Word8)])
  charsetsProvided = const $ return (return Nothing)

  encodingsProvided :: a -> ResourceMonadResult (Maybe [(ByteString, Word8 -> Word8)])
  encodingsProvided = const $ return (return Nothing)

  exists :: a -> ResourceMonadResult Bool
  exists = const $ return (return True)

  generateETag :: a -> ResourceMonadResult (Maybe ByteString)
  generateETag = const $ return (return Nothing)

  lastModified :: a -> ResourceMonadResult (Maybe UTCTime)
  lastModified = const $ return (return Nothing)

  expires :: a -> ResourceMonadResult (Maybe UTCTime)
  expires = const $ return (return Nothing)

  deleteResource :: a -> ResourceMonadResult Bool
  deleteResource = const $ return (return True)

  deleteCompleted :: a -> ResourceMonadResult Bool
  deleteCompleted = const $ return (return True)

  previouslyExisted :: a -> ResourceMonadResult Bool
  previouslyExisted = const $ return (return False)

  movedPermanently :: a -> ResourceMonadResult (Maybe ByteString)
  movedPermanently = const $ return (return Nothing)

  movedTemporarily :: a -> ResourceMonadResult (Maybe ByteString)
  movedTemporarily = const $ return (return Nothing)

  isConflict :: a -> ResourceMonadResult Bool
  isConflict = const $ return (return False)

  contentTypesAccepted :: a -> ResourceMonad [(MT.MediaType, ResourceMonadResult Bool)]
  contentTypesAccepted = const $ return []

  hasResponseBody :: a -> ResourceMonadResult Bool
  hasResponseBody = const $ return (return False)

  multipleChoices :: a -> ResourceMonadResult Bool
  multipleChoices = const $ return (return False)

  allowMissingPost :: a -> ResourceMonadResult Bool
  allowMissingPost = const $ return (return False)

class Responsible a where
  toResponse :: a -> ResourceMonad Wai.Response

instance Responsible HTTP.Status where
  toResponse s =
    return $ Wai.responseLBS s [("Content-Type", "text/plain")] "Hello World"

