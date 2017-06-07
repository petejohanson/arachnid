{-# LANGUAGE OverloadedStrings #-}

module Arachnid.Types
( ResourceMonad
, Resource
, serviceAvailable
, knownMethods
, requestURITooLong
, allowedMethods
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
  requestURITooLong _ = return False

  allowedMethods :: a -> ResourceMonad [HTTP.Method]
  allowedMethods = const $ return
    [ "GET"
    , "HEAD"
    , "OPTIONS"
    ]

class Responsible a where
  toResponse :: a -> ResourceMonad Wai.Response

instance Responsible HTTP.Status where
  toResponse s =
    return $ Wai.responseLBS s [("Content-Type", "text/plain")] "Hello World"

