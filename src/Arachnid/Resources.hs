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
, Responsible
, toResponse
) where

import Control.Monad.Reader
import Control.Monad.Trans.Resource

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as HTTP

type ResourceMonad = ReaderT Wai.Request (ResourceT IO)

class Resource a where
  serviceAvailable :: a -> ResourceMonad Bool
  serviceAvailable _ = return True

  knownMethods :: a -> ResourceMonad [HTTP.Method]
  knownMethods = const $ return
    [ "GET"
    , "HEAD"
    , "POST"
    , "PUT"
    , "DELETE"
    , "PATCH"
    , "OPTIONS"
    , "TRACE"
    ]

  requestURITooLong :: a -> ResourceMonad Bool
  requestURITooLong = const $ return False

  allowedMethods :: a -> ResourceMonad [HTTP.Method]
  allowedMethods = const $ return
    [ "GET"
    , "HEAD"
    , "OPTIONS"
    ]

  malformedRequest :: a -> ResourceMonad Bool
  malformedRequest = const $ return False

  authorized :: a -> ResourceMonad Bool
  authorized = const $ return True

class Responsible a where
  toResponse :: a -> ResourceMonad Wai.Response

instance Responsible HTTP.Status where
  toResponse s =
    return $ Wai.responseLBS s [("Content-Type", "text/plain")] "Hello World"
