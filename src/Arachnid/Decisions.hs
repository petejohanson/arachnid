{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Arachnid.Decisions
( handle
) where


import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Arachnid.Types
import Data.List
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

type Decision = forall a. (Resource a) => a -> ResourceMonad Wai.Response

decisionStart :: Decision
decisionStart = v3b13

decisionBranch :: (Resource a) => ResourceMonad Bool -> Decision -> Decision -> a -> ResourceMonad Wai.Response

decisionBranch check t f res =
  check >>= (\p -> (if p then t else f) res)

v3b13 :: Decision
v3b13 res = decisionBranch (serviceAvailable res) v3b12 (\_ -> toResponse HTTP.serviceUnavailable503) res

v3b12 :: Decision
v3b12 res = decisionBranch (elem <$> asks Wai.requestMethod <*> knownMethods res) v3b11 (\_ -> toResponse HTTP.notImplemented501) res

v3b11 :: Decision
v3b11 res = decisionBranch (requestURITooLong res) (\_ -> toResponse HTTP.requestURITooLong414) v3b10 res

data AllowedMethods = AllowedMethods [HTTP.Method]
instance Responsible AllowedMethods where
  toResponse (AllowedMethods allowedMethods) =
    return $ Wai.responseLBS HTTP.methodNotAllowed405 [("Allow", BS.concat $ intersperse "," allowedMethods)] ""

v3b10 :: Decision
v3b10 res = do
  m <- asks Wai.requestMethod
  a <- allowedMethods res

  if m `elem` a
    then toResponse HTTP.ok200
    else toResponse (AllowedMethods a)

handle :: forall a. (Resource a) => a -> Wai.Request -> ResourceT IO Wai.Response
handle res =
  runReaderT (decisionStart res)