{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Arachnid.Decisions
( handle
) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Arachnid.Resources
import qualified Arachnid.Response as Resp
import Arachnid.Internal.Date (parseHttpDate)
import Data.Char (ord)
import Data.List
import Data.Maybe
import Data.Time

import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8
import qualified Network.HTTP.Media as MT
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Network.Wai as Wai

type DecisionResult = Either HTTP.Status DecisionNode

data DecisionNode
  = B13
  | B12
  | B11
  | B10
  | B9
  | B8
  | B7
  | B6
  | B5
  | B4
  | B3
  | C3
  | C4
  | D4
  | D5
  | E5
  | E6
  | F6
  | F7
  | G7
  | G8
  | G9
  | G11
  | H7
  | H10
  | H11
  | H12
  | I4
  | I7
  | I12
  | I13
  | J18
  | K5
  | K7
  | K13
  | L5
  | L7
  | L13
  | L14
  | L15
  | L17
  | M5
  | M7
  | M16
  | M20
  | N5
  | N11
  | N16
  | O14
  | O16
  | O18
  | O20
  | P3
  | P11
  deriving (Show)

decisionStart = B13

type Decision = forall a. (Resource a) => DecisionNode -> a -> ResourceMonad DecisionResult

findHeader :: Header.HeaderName -> [Header.Header] -> Maybe BS.ByteString
findHeader hn hs = snd `fmap` find ((==hn) . fst) hs

getHeader :: Header.HeaderName -> ResourceMonad (Maybe BS.ByteString)
getHeader header = asks Wai.requestHeaders >>= (return . findHeader header)

decisionBranch :: (Resource a) => (a -> ResourceMonad Bool) -> DecisionResult -> DecisionResult -> a -> ResourceMonad DecisionResult
decisionBranch check t f res =
  check res >>= (\p -> (return (if p then t else f)))

decideIfHeader :: (Resource a) => Header.HeaderName -> (BS.ByteString -> DecisionResult) -> DecisionResult -> a -> ResourceMonad DecisionResult
decideIfHeader header found missing res = do
  h <- asks Wai.requestHeaders

  case find ((==header) . fst) h of
    Just a -> return $ found (snd a)
    Nothing -> return missing

decideIfAcceptHeader :: (Resource r, MT.Accept a) => Header.HeaderName -> (r -> ResourceMonad (Maybe [(a, b)])) -> DecisionResult -> DecisionResult -> r -> ResourceMonad DecisionResult
decideIfAcceptHeader hn hFunc pass fail res =
  ((&&) <$> liftM isJust (getHeader hn) <*> liftM isJust (hFunc res)) >>= (\p -> if p then return pass else return fail)

decideIfDateHeader :: (Resource a) => Header.HeaderName -> (UTCTime -> DecisionResult) -> DecisionResult -> a -> ResourceMonad DecisionResult
decideIfDateHeader header found missing = decideIfHeader header (decideIfDate found missing) missing

decideIfDate :: (UTCTime -> DecisionResult) -> DecisionResult -> BS.ByteString -> DecisionResult
decideIfDate found missing s =
  case parseHttpDate s of
    Nothing -> missing
    Just date -> found date

decideIfMethod :: HTTP.Method -> DecisionResult -> DecisionResult -> ResourceMonad DecisionResult
decideIfMethod method pass fail = ((==) <$> asks Wai.requestMethod <*> (pure method)) >>= (\p -> if p then return pass else return fail)

parseETag :: BS.ByteString -> [BS.ByteString]
parseETag = BS.split (fromIntegral $ ord ',')

decideETagMatch :: (Resource a) => Header.HeaderName -> DecisionResult -> DecisionResult -> a -> ResourceMonad DecisionResult
decideETagMatch header pass fail = decisionBranch (\res -> (fromMaybe False) `fmap` ((pure $ pure elem) <**> generateETag res <**> ((pure $ pure parseETag) <**> (getHeader header)))) pass fail
  where (<**>) = liftA2 (<*>)

decideUnlessMovedPermanently :: (Resource a) => DecisionResult -> a -> ResourceMonad DecisionResult
decideUnlessMovedPermanently = decideUnlessMoved movedPermanently HTTP.movedPermanently301

decideUnlessMovedTemporarily :: (Resource a) => DecisionResult -> a -> ResourceMonad DecisionResult
decideUnlessMovedTemporarily = decideUnlessMoved movedTemporarily HTTP.status307

decideUnlessMoved :: (Resource a) => (a -> ResourceMonad (Maybe BS.ByteString)) -> HTTP.Status -> DecisionResult -> a -> ResourceMonad DecisionResult
decideUnlessMoved check movedStatus notMoved = decisionBranch isMoved (Left movedStatus) notMoved
  where isMoved res = (check res) >>= mapUri
        mapUri :: Maybe BS.ByteString -> ResourceMonad Bool
        mapUri Nothing = return False
        mapUri (Just uri) = (modify $ Resp.addHeader Header.hLocation uri) >>= (const $ return True)

decision :: Decision
decision B13 = decisionBranch serviceAvailable (Right B12) (Left HTTP.serviceUnavailable503)
decision B12 = decisionBranch (\res -> elem <$> asks Wai.requestMethod <*> knownMethods res) (Right B11) (Left HTTP.notImplemented501)
decision B11 = decisionBranch requestURITooLong (Left HTTP.requestURITooLong414) (Right B10)
decision B10 = decisionBranch checkAllowed (Right B9) (Left HTTP.methodNotAllowed405)
  where checkAllowed res = do
          m <- asks Wai.requestMethod
          allowed <- allowedMethods res

          if m `elem` allowed
            then return True
            else do
              modify $ Resp.addHeader Header.hAllow allowed
              return False

decision B9 = decisionBranch malformedRequest (Left HTTP.badRequest400) (Right B8)
-- TODO Include WWW-Authenticate header in response!
decision B8 = decisionBranch authorized (Right B7) (Left HTTP.unauthorized401)
decision B7 = decisionBranch forbidden (Left HTTP.forbidden403) (Right B6)
decision B6 = decisionBranch validContentHeaders (Right B5) (Left HTTP.notImplemented501)
decision B5 = decisionBranch knownContentType (Right B4) (Left HTTP.unsupportedMediaType415)
decision B4 = decisionBranch requestEntityTooLarge (Left HTTP.requestEntityTooLarge413) (Right B3)
decision B3 = decisionBranch isOptions (Left HTTP.ok200) (Right C3)
  where handleOptions _ False = return False
        handleOptions res True = options res >>= (modify . Resp.addHeaders) >>= (const $ return True)
        isOptions res = (=="OPTIONS") <$> asks Wai.requestMethod >>= handleOptions res

decision C3 = decideIfHeader Header.hAccept (const $ Right C4) (Right D5)

decision C4 = decisionBranch (\res -> (isJust `fmap` (MT.mapAccept <$> contentTypesProvided res <*> (fromJust `fmap` (getHeader Header.hAccept)))))
                              (Right D4)
                              (Left HTTP.notAcceptable406)
decision D4 = decideIfHeader Header.hAcceptLanguage (const $ Right D5) (Right E5)

decision D5 = decisionBranch (\res -> (fromJust `fmap` (getHeader Header.hAcceptLanguage) >>= (\l -> languageAvailable l res))) (Right E5) (Left HTTP.unsupportedMediaType415)

decision E5 = decideIfAcceptHeader Header.hAcceptCharset
                                   charsetsProvided
                                   (Right E6)
                                   (Right F6)

-- Can we do this in one pass without the do? liftM somehow?
decision E6 = (\res -> do
  cs <- charsetsProvided res
  c <- getHeader Header.hAcceptCharset

  case MT.mapAccept <$> cs <*> c of
    Nothing -> return $ Left HTTP.notAcceptable406
    Just _ -> return $ Right F6
  )

decision F6 = decideIfAcceptHeader Header.hAcceptEncoding
                                   encodingsProvided
                                   (Right F7)
                                   (Right G7)

-- Can we do this in one pass without the do? liftM somehow?
decision F7 = (\res -> do
  es <- encodingsProvided res
  e <- getHeader Header.hAcceptEncoding

  case MT.mapAccept <$> es <*> e of
    Nothing -> return $ Left HTTP.notAcceptable406
    Just _ -> return $ Right G7
  )

decision G7 = decisionBranch exists (Right G8) (Right H7)

decision G8 = decideIfHeader Header.hIfMatch (const $ Right G9) (Right H10)

decision G9 = const $ (getHeader Header.hIfMatch) >>= (return . Right . ifMatchNodeMap . fromJust)
  where ifMatchNodeMap "*" = H10
        ifMatchNodeMap _   = G11

decision G11 = decideETagMatch Header.hIfMatch (Right H10) (Left HTTP.preconditionFailed412)

decision H7 = const $ do
  h <- asks Wai.requestHeaders

  case find (==(Header.hIfMatch, "*")) h of
    Nothing -> return $ Right I7
    (Just _) -> return $ Left HTTP.preconditionFailed412

decision H10 = decideIfDateHeader Header.hIfUnmodifiedSince (const $ Right H12) (Right I12)

-- Tad duplicitive, but allows tracing to not miss decision point. Worth it?
-- decision H10 = decideIfHeader Header.hIfUnmodifiedSince (const $ Right H11) (Right I12)
-- decision H11 = decideIfDateHeader Header.hIfUnmodifiedSince (const $ Right H12) (Right I12)

decision H12  = decisionBranch (\res -> (<) <$> ((fmap $ fromJust . parseHttpDate) `fmap` (getHeader Header.hIfUnmodifiedSince)) <*> lastModified res)
                               (Right I12)
                               (Left HTTP.preconditionFailed412)


decision I4 = decideUnlessMovedPermanently (Right P3)

decision I7 = const $ decideIfMethod "PUT" (Right I4) (Right K7)

decision I12 = decideIfHeader Header.hIfNoneMatch (const $ Right I13) (Right L13)

decision I13 = const $ (getHeader Header.hIfNoneMatch) >>= (return . Right . ifNoneMatchNodeMap . fromJust)
  where ifNoneMatchNodeMap "*" = J18
        ifNoneMatchNodeMap _   = K13

decision J18 = const $ asks Wai.requestMethod >>= (\m -> if m `elem` ["GET", "HEAD"] then (return $ Left HTTP.notModified304) else (return $ Left HTTP.preconditionFailed412))

decision K5 = decideUnlessMovedPermanently (Right L5)

decision K7 = decisionBranch previouslyExisted (Right K5) (Right L7)

decision K13 = decideETagMatch Header.hIfNoneMatch (Right J18) (Right L13)

decision L5 = decideUnlessMovedTemporarily (Right M5)
decision L7 = const $ decideIfMethod "POST" (Right M7) (Left HTTP.notFound404)

decision L13 = decideIfDateHeader Header.hIfModifiedSince (const $ Right L15) (Right M16)

-- Tad duplicitive, but allows tracing to not miss decision point. Worth it?
-- decision L13 = decideIfHeader Header.hIfModifiedSince (const $ Right L14) (Right M16)
-- decision L14 = decideIfDateHeader Header.hIfModifiedSince (const $ Right L15) (Right M16)

decision L15  = decisionBranch (\res -> (>) <$> ((fmap $ fromJust . parseHttpDate) `fmap` (getHeader Header.hIfModifiedSince)) <*> (liftIO (getCurrentTime >>= ((\t -> return $ Just t)))))
                               (Right M16)
                               (Right L17)

decision L17  = decisionBranch (\res -> (>) <$> lastModified res <*> ((fmap $ fromJust . parseHttpDate) `fmap` (getHeader Header.hIfModifiedSince)))
                               (Right M16)
                               (Left HTTP.notModified304)

decision P3 = decisionBranch isConflict (Left HTTP.conflict409) (Right P11)

-- 
-- v3p3 :: Decision
-- v3p3 = decisionBranch isConflict
--                       (return $ toResponse HTTP.conflict409)
--                       (\res -> acceptContent res >>= v3p11 res)
-- 
-- acceptContent :: (Resource a) => a -> ResourceMonad ProcessingResult
-- acceptContent res = do
--   headers <- asks Wai.requestHeaders
--   accepted <- contentTypesAccepted res
-- 
--   case fmap snd  (find ((==Header.hContentType) . fst) headers) >>= (MT.mapContent accepted) of
--     Nothing -> return $ Halt HTTP.unsupportedMediaType415
--     Just t -> t
-- 
-- v3p11 :: (Resource a) => a -> ProcessingResult -> ResourceMonad Wai.Response
-- v3p11 _ (Halt status) = toResponse status
-- v3p11 _ (Created uri) = return $ Wai.responseLBS HTTP.created201 [(Header.hLocation, uri)] ""
-- v3p11 _ Error = toResponse HTTP.status500
-- v3p11 r Success = v3o20 r
-- 
-- v3h12 :: UTCTime -> Decision
-- v3h12 ius res = do
--   lm <- lastModified res
-- 
--   if Just ius < lm
--     then v3i12 res
--     else toResponse HTTP.preconditionFailed412
-- 
-- 
-- v3l13 :: Decision
-- v3l13 = decideIfDateHeader Header.hIfModifiedSince
--                            v3l15
--                            v3m16
-- 
-- v3j18 :: Decision
-- v3j18 _ = do
--   m <- asks Wai.requestMethod
-- 
--   if m `elem` ["GET", "HEAD"]
--     then toResponse HTTP.notModified304
--     else toResponse HTTP.preconditionFailed412
-- 
-- v3k13 :: BS.ByteString -> Decision
-- v3k13 = decideETagMatch v3j18 v3l13
-- 
-- v3l15 :: UTCTime -> Decision
-- v3l15 ims res = do
--   now <- lift $ lift getCurrentTime
--   if ims > now
--     then v3m16 res
--     else v3l17 ims res
-- 
-- v3l17 :: UTCTime -> Decision
-- v3l17 ims res = do
--   lm <- lastModified res
-- 
--   if Just ims < lm
--     then toResponse HTTP.notModified304
--     else v3m16 res
-- 
-- v3m16 :: Decision
-- v3m16 = decideIfMethod "DELETE" v3m20 v3n16
-- 
-- v3m20 :: Decision
-- v3m20 = decisionBranch deleteResource
--                        v3m20b
--                        (const $ toResponse HTTP.status500)
-- 
-- v3m20b :: Decision
-- v3m20b = decisionBranch deleteCompleted
--                        v3o20
--                        (const $ toResponse HTTP.accepted202)
-- 
-- v3o20 :: Decision
-- v3o20 = decisionBranch hasResponseBody
--                        v3o18
--                        (const $ toResponse HTTP.noContent204)
-- 
-- v3o18 :: Decision
-- v3o18 = decisionBranch multipleChoices
--                        (const $ toResponse HTTP.status300)
--                        (const $ toResponse HTTP.ok200)
-- 
-- v3n16 :: Decision
-- v3n16 = const $ toResponse HTTP.ok200

handle :: forall a. (Resource a) => a -> Wai.Request -> ResourceT IO Wai.Response
handle res req = runStateT (runReaderT (decide decisionStart res) req) Resp.emptyResponse >>= return . createResponse
   where decide node res = decision node res >>= processResult
         processResult result = case result of -- I really want to use Either short circuiting via fmap/>>= here!
                                  Right next -> decide next res
                                  Left s     -> return $ s
         createResponse (status, respData) = Wai.responseLBS status (fst $ Resp.responseHeaders respData) (fromMaybe LBS.empty (Resp.body respData))
