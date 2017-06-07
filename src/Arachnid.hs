module Arachnid
( makeApp
) where

import Arachnid.Decisions
import Arachnid.Routing
import Arachnid.Types

import Control.Monad.Trans.Resource

import qualified Network.Wai as W

makeApp :: (Resource a) => [(Route, a)] -> W.Application
makeApp (r:rs) req =
  case match (fst r) path of
    Just _  -> runHandler (snd r) req
    Nothing -> makeApp rs req
  where path = W.pathInfo req

runHandler :: (Resource a) => a -> W.Request -> (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
runHandler res req respond = do
  resp <- runResourceT $ handle res req
  respond resp

