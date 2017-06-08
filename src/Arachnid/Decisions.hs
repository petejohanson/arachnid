{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Arachnid.Decisions
( handle
) where


import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Arachnid.Resources
import Data.List
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

type Decision = forall a. (Resource a) => a -> ResourceMonad Wai.Response

decisionStart :: Decision
decisionStart = v3b13

decisionBranch :: (Resource a) => (a -> ResourceMonad Bool) -> Decision -> Decision -> a -> ResourceMonad Wai.Response

decisionBranch check t f res =
  check res >>= (\p -> (if p then t else f) res)

v3b13 :: Decision
v3b13 = decisionBranch serviceAvailable v3b12 (const $ toResponse HTTP.serviceUnavailable503)

v3b12 :: Decision
v3b12 = decisionBranch (\res -> elem <$> asks Wai.requestMethod <*> knownMethods res) v3b11 (const $ toResponse HTTP.notImplemented501)

v3b11 :: Decision
v3b11 = decisionBranch requestURITooLong (const $ toResponse HTTP.requestURITooLong414) v3b10

data AllowedMethods = AllowedMethods [HTTP.Method]
instance Responsible AllowedMethods where
  toResponse (AllowedMethods allowed) =
    return $ Wai.responseLBS HTTP.methodNotAllowed405 [("Allow", BS.concat $ intersperse "," allowed)] ""

v3b10 :: Decision
v3b10 res = do
  m <- asks Wai.requestMethod
  a <- allowedMethods res

  if m `elem` a
    then v3b9 res
    else toResponse (AllowedMethods a)

v3b9 :: Decision
v3b9 = decisionBranch malformedRequest (const $ toResponse HTTP.badRequest400) v3b8

v3b8 :: Decision
v3b8 = decisionBranch authorized v3b7 (const $ toResponse HTTP.unauthorized401)

v3b7 :: Decision
v3b7 = decisionBranch forbidden (const $ toResponse HTTP.forbidden403) v3b6

v3b6 :: Decision
v3b6 = decisionBranch validContentHeaders v3b5 (const $ toResponse HTTP.notImplemented501)

v3b5 :: Decision
v3b5 = decisionBranch knownContentType v3b4 (const $ toResponse HTTP.unsupportedMediaType415)

v3b4 :: Decision
v3b4 = decisionBranch requestEntityTooLarge (const $ toResponse HTTP.requestEntityTooLarge413) v3b3

v3b3 :: Decision
v3b3 res = do
  m <- asks Wai.requestMethod

  case m of
    "OPTIONS" -> do
      opts <- options res
      return $ Wai.responseLBS HTTP.ok200 opts ""
    _ -> toResponse HTTP.ok200

handle :: forall a. (Resource a) => a -> Wai.Request -> ResourceT IO Wai.Response
handle res =
  runReaderT (decisionStart res)