{-# LANGUAGE OverloadedStrings #-}

module Arachnid
( makeApp
) where

import Arachnid.Decisions
import Arachnid.Routing
import Arachnid.Types

import Control.Monad.Trans.Resource

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as W

makeApp :: (Resource a) => [(Route, a)] -> W.Application
makeApp [] _ respond = respond $ W.responseLBS HTTP.notFound404 [] "Not Found"
makeApp (r:rs) req respond =
  case match (fst r) path of
    Just _  -> runHandler (snd r) req respond
    Nothing -> makeApp rs req respond
  where path = W.pathInfo req

runHandler :: (Resource a) => a -> W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
runHandler res req respond = do
  resp <- runResourceT $ handle res req
  respond resp

