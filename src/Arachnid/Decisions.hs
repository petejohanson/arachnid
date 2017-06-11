{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Arachnid.Decisions
( handle
) where

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Arachnid.Resources
import Arachnid.Internal.Date (parseHttpDate)
import Data.Char (ord)
import Data.List
import Data.Maybe
import Data.Time

import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Media as MT
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Header
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
    _ -> v3c3 res

decideIfHeader :: (Resource a) => Header.HeaderName -> (BS.ByteString -> Decision) -> Decision -> a -> ResourceMonad Wai.Response
decideIfHeader header found missing res = do
  h <- asks Wai.requestHeaders

  case find ((==header) . fst) h of
    Just a -> found (snd a) res
    Nothing -> missing res

decideIfDateHeader :: (Resource a) => Header.HeaderName -> (UTCTime -> Decision) -> Decision -> a -> ResourceMonad Wai.Response
decideIfDateHeader header found missing = decideIfHeader header (decideIfDate found missing) missing

decideIfDate :: (UTCTime -> Decision) -> Decision -> BS.ByteString -> Decision
decideIfDate found missing s =
  case parseHttpDate s of
    Nothing -> missing
    Just date -> found date

v3c3 :: Decision
v3c3 = decideIfHeader Header.hAccept
                      v3c4
                      (const $ toResponse HTTP.ok200)

v3c4 :: BS.ByteString -> Decision
v3c4 a = decisionBranch (\res -> (\types -> isJust $ MT.mapAccept types a) `fmap` contentTypesProvided res)
                        v3d4
                        (const $ toResponse HTTP.unsupportedMediaType415)

v3d4 :: Decision
v3d4 = decideIfHeader Header.hAcceptLanguage
                      v3d5
                      v3e5

v3d5 :: BS.ByteString -> Decision
v3d5 l = decisionBranch (languageAvailable l)
                        v3e5
                        (const $ toResponse HTTP.unsupportedMediaType415)

v3e5 :: Decision
v3e5 = decideIfHeader Header.hAcceptCharset
                      v3e6
                      v3f6

v3e6 :: BS.ByteString -> Decision
v3e6 c res = do
  cs <- charsetsProvided res

  case cs of
    Nothing -> v3f6 res
    Just charsets ->
      case MT.mapAccept charsets c of
        Nothing -> toResponse HTTP.unsupportedMediaType415
        Just _ -> v3f6 res


v3f6 :: Decision
v3f6 = decideIfHeader Header.hAcceptEncoding
                      v3f7
                      v3g7

v3f7 :: BS.ByteString -> Decision
v3f7 c res = do
  es <- encodingsProvided res

  case es of
    Nothing -> v3g7 res
    Just encodings ->
      case MT.mapAccept encodings c of
        Nothing -> toResponse HTTP.unsupportedMediaType415
        Just _ -> v3g7 res

v3g7 :: Decision
v3g7 = decisionBranch resourceExists
                      v3g8
                      v3h7

v3g8 :: Decision
v3g8 = decideIfHeader Header.hIfMatch
                      v3g9
                      v3h10

v3g9 :: BS.ByteString -> Decision
v3g9 "*" = v3h10
v3g9 im  = v3g11 im

decideETagMatch :: (Resource a) => Decision -> Decision -> BS.ByteString -> a -> ResourceMonad Wai.Response
decideETagMatch pass fail h res = do
  -- TODO: Need to strip whitespace too!
  let matchTags = BS.split (fromIntegral $ ord ',') h

  etag <- generateETag res
  case etag of
    Nothing -> fail res
    Just e ->
      if e `elem` matchTags
        then pass res
        else fail res

v3g11 :: BS.ByteString -> Decision
v3g11 = decideETagMatch v3h10 (const $ toResponse HTTP.preconditionFailed412)

v3h10 :: Decision
v3h10 = decideIfDateHeader Header.hIfUnmodifiedSince
                       v3h12
                       v3i12

v3h7 :: Decision
v3h7 res = do
  h <- asks Wai.requestHeaders

  case find (==(Header.hIfMatch, "*")) h of
    Just _ -> toResponse HTTP.preconditionFailed412
    Nothing -> v3i7 res

v3i7 :: Decision
v3i7 = const $ toResponse HTTP.ok200

v3h12 :: UTCTime -> Decision
v3h12 ius res = do
  lm <- lastModified res

  if Just ius < lm
    then v3i12 res
    else toResponse HTTP.preconditionFailed412

v3i12 :: Decision
v3i12 = decideIfHeader Header.hIfNoneMatch
                       v3i13
                       v3l13

v3i13 :: BS.ByteString -> Decision
v3i13 "*" = v3j18
v3i13 inm = v3k13 inm

v3l13 :: Decision
v3l13 = decideIfDateHeader Header.hIfModifiedSince
                           v3l15
                           v3m16

v3j18 :: Decision
v3j18 _ = do
  m <- asks Wai.requestMethod

  if m `elem` ["GET", "HEAD"]
    then toResponse HTTP.notModified304
    else toResponse HTTP.preconditionFailed412

v3k13 :: BS.ByteString -> Decision
v3k13 = decideETagMatch v3j18 v3l13

v3l15 :: UTCTime -> Decision
v3l15 ims res = do
  now <- lift $ lift getCurrentTime
  if ims > now
    then v3m16 res
    else v3l17 ims res

v3l17 :: UTCTime -> Decision
v3l17 ims res = do
  lm <- lastModified res

  if Just ims < lm
    then toResponse HTTP.notModified304
    else v3m16 res

v3m16 :: Decision
v3m16 res = do
  m <- asks Wai.requestMethod

  if m == "DELETE"
    then v3m20 res
    else v3n16 res
-- v3m16 res = decisionBranch (asks Wai.requestMethod >>= (=="DELETE"))
--                        v3m20 res
--                        v3n16 res

v3m20 :: Decision
v3m20 = decisionBranch deleteResource
                       v3m20b
                       (const $ toResponse HTTP.status500)

v3m20b :: Decision
v3m20b = decisionBranch deleteCompleted
                       v3o20
                       (const $ toResponse HTTP.accepted202)

v3o20 :: Decision
v3o20 = const $ toResponse HTTP.ok200

v3n16 :: Decision
v3n16 = const $ toResponse HTTP.ok200

handle :: forall a. (Resource a) => a -> Wai.Request -> ResourceT IO Wai.Response
handle res =
  runReaderT (decisionStart res)
