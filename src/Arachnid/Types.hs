
module Types
( ResourceMonad
, Resource
) where

import Control.Monad.Reader
import Control.Monad.Trans.Resource

import qualified Network.Wai as Wai

type ResourceMonad = ReaderT Wai.Request (ResourceT IO)

class Resource a where
  resourceExists :: a -> ResourceMonad Bool
  resourceExists _ = return True
