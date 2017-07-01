{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Arachnid.Decisions
( handle
) where

import Control.Applicative
import Control.Monad ((>=>))
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
  | C3a
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
  | M20B
  | N5
  | N11
  | N16
  | O14
  | O16
  | O18
  | O18a
  | O20
  | P3
  | P11
  deriving (Show)

mapRes :: (a -> ResourceMonad b) -> ResourceResult a -> ResourceMonadResult b
mapRes f (Left s) = return $ Left s
mapRes f (Right r) = fmap Right (f r)

getBody :: (Resource a) => a -> ResourceMonadResult (Maybe (IO LBS.ByteString))
getBody res = contentTypesProvided res >>= mapRes getContent
  where getContent :: [(MT.MediaType, IO LBS.ByteString)] -> ResourceMonad (Maybe (IO LBS.ByteString))
        getContent opts = gets Resp.chosenContentType >>= (\h -> return $ join $ MT.mapAcceptMedia <$> return opts <*> fmap MT.renderHeader h)

decisionStart = B13

type Decision = forall a. (Resource a) => DecisionNode -> a -> ResourceMonad DecisionResult

findHeader :: Header.HeaderName -> [Header.Header] -> Maybe BS.ByteString
findHeader hn hs = snd `fmap` find ((==hn) . fst) hs

getHeader :: Header.HeaderName -> ResourceMonad (Maybe BS.ByteString)
getHeader header = fmap (findHeader header) (asks Wai.requestHeaders)

getDateHeader :: Header.HeaderName -> ResourceMonad (Maybe UTCTime)
getDateHeader header = fmap (findHeader header >=> parseHttpDate) (asks Wai.requestHeaders)

mapToDecision :: DecisionResult -> DecisionResult -> ResourceResult Bool -> DecisionResult
mapToDecision pass fail check = check >>= (\p -> if p then pass else fail)

decisionBranch :: (Resource a) => (a -> ResourceMonadResult Bool) -> DecisionResult -> DecisionResult -> a -> ResourceMonad DecisionResult
decisionBranch check t f res =
  fmap (mapToDecision t f) (check res)

decideIfHeader :: (Resource a) => Header.HeaderName -> (BS.ByteString -> DecisionResult) -> DecisionResult -> a -> ResourceMonad DecisionResult
decideIfHeader header found missing res = do
  h <- asks Wai.requestHeaders

  case find ((==header) . fst) h of
    Just a -> return $ found (snd a)
    Nothing -> return missing

decideIfAcceptHeader :: (Resource r, MT.Accept a) => Header.HeaderName -> (r -> ResourceMonadResult (Maybe [(a, b)])) -> DecisionResult -> DecisionResult -> r -> ResourceMonad DecisionResult
decideIfAcceptHeader hn hFunc = decisionBranch (hasHeaderAndResourceResult hn hFunc)
  where hasHeaderAndResourceResult :: (Resource a) => Header.HeaderName -> (a -> ResourceMonadResult (Maybe b)) -> a -> ResourceMonadResult Bool
        hasHeaderAndResourceResult hn f res = fmap . (&&) <$> fmap isJust (getHeader hn) <*> fmap (fmap isJust) (f res)

decideIfDateHeader :: (Resource a) => Header.HeaderName -> (UTCTime -> DecisionResult) -> DecisionResult -> a -> ResourceMonad DecisionResult
decideIfDateHeader header found missing = decideIfHeader header (decideIfDate found missing) missing

decideIfDate :: (UTCTime -> DecisionResult) -> DecisionResult -> BS.ByteString -> DecisionResult
decideIfDate found missing s =
  case parseHttpDate s of
    Nothing -> missing
    Just date -> found date

decideIfMethod :: HTTP.Method -> DecisionResult -> DecisionResult -> ResourceMonad DecisionResult
decideIfMethod method pass fail = ((==) <$> asks Wai.requestMethod <*> pure method) >>= (\p -> if p then return pass else return fail)

parseETag :: BS.ByteString -> [BS.ByteString]
parseETag = BS.split (fromIntegral $ ord ',')

decideETagMatch :: (Resource a) => Header.HeaderName -> DecisionResult -> DecisionResult -> a -> ResourceMonad DecisionResult
decideETagMatch header = decisionBranch (\res -> fmap (fromMaybe False) `fmap` (generateETag res >>= includeETag))
  where includeETag :: ResourceResult (Maybe BS.ByteString) -> ResourceMonadResult (Maybe Bool)
        includeETag etag = fmap (\h -> fmap (compareETags h) etag ) (getHeader header)
        compareETags :: Maybe BS.ByteString -> Maybe BS.ByteString -> Maybe Bool
        compareETags header val = elem <$> val <*> fmap parseETag header

decideUnlessMovedPermanently :: (Resource a) => DecisionResult -> a -> ResourceMonad DecisionResult
decideUnlessMovedPermanently = decideUnlessMoved movedPermanently HTTP.movedPermanently301

decideUnlessMovedTemporarily :: (Resource a) => DecisionResult -> a -> ResourceMonad DecisionResult
decideUnlessMovedTemporarily = decideUnlessMoved movedTemporarily HTTP.status307

decideUnlessMoved :: (Resource a) => (a -> ResourceMonadResult (Maybe BS.ByteString)) -> HTTP.Status -> DecisionResult -> a -> ResourceMonad DecisionResult
decideUnlessMoved check movedStatus = decisionBranch isMoved (Left movedStatus)
  where isMoved res = check res >>= mapResultUri
        mapResultUri (Left s) = return (Left s)
        mapResultUri (Right u) = mapUri u
        mapUri :: Maybe BS.ByteString -> ResourceMonadResult Bool
        mapUri Nothing = return (return False)
        mapUri (Just uri) = modify (Resp.addHeader Header.hLocation uri) >>= const (return (return True))

decision :: Decision
decision B13 = decisionBranch serviceAvailable (Right B12) (Left HTTP.serviceUnavailable503)

decision B12 = decisionBranch isKnownMethod (Right B11) (Left HTTP.notImplemented501)
  where isKnownMethod :: (Resource a) => a -> ResourceMonadResult Bool
        isKnownMethod res = (fmap . elem) <$> asks Wai.requestMethod <*> knownMethods res

decision B11 = decisionBranch requestURITooLong (Left HTTP.requestURITooLong414) (Right B10)

-- TODO: This whole thing is messy. How better to add headers if not allowed?
decision B10 = decisionBranch checkAllowed (Right B9) (Left HTTP.methodNotAllowed405)
  where reqMethod :: ResourceMonad HTTP.Method
        reqMethod = asks Wai.requestMethod
        handleAllowed :: ResourceResult [HTTP.Method] -> ResourceMonadResult Bool
        handleAllowed allowed =
          reqMethod >>= (\m -> addAllowedHeader allowed $ fmap (elem m) allowed)
        addAllowedHeader :: ResourceResult [HTTP.Method] -> ResourceResult Bool -> ResourceMonadResult Bool
        addAllowedHeader (Right methods) (Right False) = fmap (const $ Right False) (modify $ Resp.addHeader Header.hAllow methods)
        addAllowedHeader _ def = return def
        checkAllowed res = allowedMethods res >>= handleAllowed


decision B9 = decisionBranch malformedRequest (Left HTTP.badRequest400) (Right B8)
-- TODO Include WWW-Authenticate header in response!
decision B8 = decisionBranch authorized (Right B7) (Left HTTP.unauthorized401)
decision B7 = decisionBranch forbidden (Left HTTP.forbidden403) (Right B6)
decision B6 = decisionBranch validContentHeaders (Right B5) (Left HTTP.notImplemented501)
decision B5 = decisionBranch knownContentType (Right B4) (Left HTTP.unsupportedMediaType415)
decision B4 = decisionBranch requestEntityTooLarge (Left HTTP.requestEntityTooLarge413) (Right B3)

decision B3 = decisionBranch isOptions (Left HTTP.ok200) (Right C3)
  where handleOptions :: (Resource a) => a -> Bool -> ResourceMonadResult Bool
        handleOptions _ False = return (Right False)
        handleOptions res True = options res >>= addHeaders >>= const (return $ return True)
        addHeaders :: ResourceResult Header.ResponseHeaders -> ResourceMonadResult ()
        addHeaders (Right hs) = fmap Right (modify $ Resp.addHeaders hs)
        addHeaders (Left l) = return $ Left l
        isOptions :: (Resource a) => a -> ResourceMonadResult Bool
        isOptions res = (=="OPTIONS") <$> asks Wai.requestMethod >>= handleOptions res

decision C3 = decideIfHeader Header.hAccept (const $ Right C4) (Right C3a)
decision C3a = \r -> contentTypesProvided r >>= mapRes defaultCT >> return (Right D4)
  where defaultCT :: [(MT.MediaType, IO LBS.ByteString)] -> ResourceMonad ()
        defaultCT = modify . Resp.setChosenContentType . Just . fst . head

decision C4 = decisionBranch isAcceptable
                             (Right D4)
                             (Left HTTP.notAcceptable406)
  where isAcceptable :: (Resource a) => a -> ResourceMonadResult Bool
        isAcceptable res = checkAccept <$> fmap fromJust (getHeader Header.hAccept) <*> contentTypesProvided res >>= mapRes recordSelected
        recordSelected :: Maybe MT.MediaType -> ResourceMonad Bool
        recordSelected (Just ct) = lift (modify $ Resp.setChosenContentType (Just ct)) >> return True
        recordSelected Nothing = return False
        checkAccept :: BS.ByteString -> ResourceResult [(MT.MediaType, IO LBS.ByteString)] -> ResourceResult (Maybe MT.MediaType)
        checkAccept h = fmap (\a -> MT.matchAccept (fmap fst a) h)

decision D4 = decideIfHeader Header.hAcceptLanguage (const $ Right D5) (Right E5)

decision D5 = decisionBranch (\res -> fromJust `fmap` getHeader Header.hAcceptLanguage >>= (`languageAvailable` res))
                             (Right E5)
                             (Left HTTP.unsupportedMediaType415)

decision E5 = decideIfAcceptHeader Header.hAcceptCharset
                                   charsetsProvided
                                   (Right E6)
                                   (Right F6)

decision E6 = decisionBranch hasAcceptedCS (Right F6) (Left HTTP.notAcceptable406)
  where hasAcceptedCS :: (Resource a) => a -> ResourceMonadResult Bool
        hasAcceptedCS res = compareCS <$> getHeader Header.hAcceptCharset <*> charsetsProvided res
        compareCS :: Maybe BS.ByteString -> ResourceResult (Maybe [(BS.ByteString, b)]) -> ResourceResult Bool
        compareCS h = fmap (\r -> isJust $ MT.mapAccept <$> r <*> h)

decision F6 = decideIfAcceptHeader Header.hAcceptEncoding
                                   encodingsProvided
                                   (Right F7)
                                   (Right G7)

-- TODO: DO this like E6 above!
-- Can we do this in one pass without the do? liftM somehow?
decision F7 = \res -> do
  es <- encodingsProvided res
  e <- getHeader Header.hAcceptEncoding

  return $ es >>= findEncoding e
  where findEncoding e es = case MT.mapAccept <$> es <*> e of
                               Nothing -> Left HTTP.notAcceptable406
                               Just _ -> Right G7

decision G7 = decisionBranch exists (Right G8) (Right H7)

decision G8 = decideIfHeader Header.hIfMatch (const $ Right G9) (Right H10)

decision G9 = const $ fmap (Right . ifMatchNodeMap . fromJust) (getHeader Header.hIfMatch)
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

decision H12  = decisionBranch lastModGTIfUnmodSince
                               (Right I12)
                               (Left HTTP.preconditionFailed412)
  where lastModGTIfUnmodSince :: (Resource a) => a -> ResourceMonadResult Bool
        lastModGTIfUnmodSince res = compareMod <$> getIUS <*> lastModified res
        getIUS = fmap (fromJust . parseHttpDate) `fmap` getHeader Header.hIfUnmodifiedSince
        compareMod :: Maybe UTCTime -> ResourceResult (Maybe UTCTime) -> ResourceResult Bool
        compareMod ius = fmap (\m -> fromMaybe False $ (<) <$> ius <*> m)


decision I4 = decideUnlessMovedPermanently (Right P3)

decision I7 = const $ decideIfMethod "PUT" (Right I4) (Right K7)

decision I12 = decideIfHeader Header.hIfNoneMatch (const $ Right I13) (Right L13)

decision I13 = const $ fmap (Right . ifNoneMatchNodeMap . fromJust) (getHeader Header.hIfNoneMatch)
  where ifNoneMatchNodeMap "*" = J18
        ifNoneMatchNodeMap _   = K13

decision J18 = const $ fmap (\m -> if m `elem` ["GET", "HEAD"] then Left HTTP.notModified304 else Left HTTP.preconditionFailed412) (asks Wai.requestMethod)

decision K5 = decideUnlessMovedPermanently (Right L5)

decision K7 = decisionBranch previouslyExisted (Right K5) (Right L7)

decision K13 = decideETagMatch Header.hIfNoneMatch (Right J18) (Right L13)

decision L5 = decideUnlessMovedTemporarily (Right M5)
decision L7 = const $ decideIfMethod "POST" (Right M7) (Left HTTP.notFound404)

decision L13 = decideIfDateHeader Header.hIfModifiedSince (const $ Right L15) (Right M16)

-- Tad duplicitive, but allows tracing to not miss decision point. Worth it?
-- decision L13 = decideIfHeader Header.hIfModifiedSince (const $ Right L14) (Right M16)
-- decision L14 = decideIfDateHeader Header.hIfModifiedSince (const $ Right L15) (Right M16)

decision L15  = decisionBranch ifModSinceFuture
                               (Right M16)
                               (Right L17)
  where ifModSinceFuture :: (Resource a) => a -> ResourceMonadResult Bool
        ifModSinceFuture res = fmap fromJust (getDateHeader Header.hIfModifiedSince) >>= compareVal
        compareVal :: UTCTime -> ResourceMonadResult Bool
        compareVal ims = fmap (return . (>)ims) (liftIO getCurrentTime)

decision L17  = decisionBranch lastModGTIfModSince
                               (Right M16)
                               (Left HTTP.notModified304)
  where lastModGTIfModSince :: (Resource a) => a -> ResourceMonadResult Bool
        lastModGTIfModSince res = compareMod <$> getIMS <*> lastModified res
        getIMS = fmap (fromJust . parseHttpDate) `fmap` getHeader Header.hIfModifiedSince
        compareMod :: Maybe UTCTime -> ResourceResult (Maybe UTCTime) -> ResourceResult Bool
        compareMod ims = fmap (\m -> fromMaybe False $ (<) <$> ims <*> m)


decision M5 = const $ decideIfMethod "POST" (Right N5) (Left HTTP.gone410)

decision M7 = decisionBranch allowMissingPost (Right N11) (Left HTTP.gone410)

decision M16 = const $ decideIfMethod "DELETE" (Right M20) (Right N16)
decision M20 = decisionBranch deleteResource (Right M20B) (Left HTTP.status500)
decision M20B = decisionBranch deleteCompleted (Right O20) (Left HTTP.accepted202)

decision N5 = decisionBranch allowMissingPost (Right N11) (Left HTTP.notFound404)

-- Actually accept the content of the POST request
-- decision N11 = decisionBranch redirect (Right N11) (Left HTTP.notFound404)

decision N16 = const $ decideIfMethod "POST" (Right N11) (Right O16)

-- TODO Actually accept the content of the PUT request
decision O14 = decisionBranch isConflict (Left HTTP.conflict409) (Right P11)

decision O16 = const $ decideIfMethod "PUT" (Right O14) (Right O18)

decision O18 = decisionBranch multipleChoices (Left HTTP.status300) (Right O18a)

-- TODO Generate body for GET/HEAD only
decision O18a = \res -> addETag res >> addLastModified res >> addExpires res >> addContentType >> encodeBody res >> success
  where addETag res = generateETag res >>= addHeader Header.hETag
        addLastModified res = lastModified res >>= addHeader Header.hLastModified
        addExpires res = expires res >>= addHeader Header.hExpires
        addContentType :: ResourceMonadResult ()
        addContentType = gets Resp.chosenContentType >>= fmap Right . addContentHeader
        addContentHeader :: Maybe MT.MediaType -> ResourceMonad ()
        addContentHeader (Just v) = modify $ Resp.addHeader Header.hContentType v
        addContentHeader _ = return ()
        encodeBody :: (Resource a) => a -> ResourceMonadResult ()
        encodeBody res = getBody res >>= mapRes setBody
        setBody :: Maybe (IO LBS.ByteString) -> ResourceMonad ()
        setBody (Just body) = liftIO body >>= lift . modify . Resp.setBody
        setBody _ = return ()
        addHeader :: (MT.RenderHeader v) => Header.HeaderName -> ResourceResult (Maybe v) -> ResourceMonadResult ()
        addHeader h = mapRes $ realAddHeader h
        realAddHeader :: (MT.RenderHeader v) => Header.HeaderName -> Maybe v -> ResourceMonad ()
        realAddHeader h (Just v) = lift $ modify $ Resp.addHeader h v
        realAddHeader _ _ = return ()
        success = return $ Left HTTP.ok200

decision P3 = \res -> isConflict res >>= mapConflict res
  where mapConflict :: (Resource a) => a -> ResourceResult Bool -> ResourceMonad DecisionResult
        mapConflict res (Left e) = return $ Left e
        mapConflict res (Right v) = handleConflict res v
        handleConflict :: (Resource a) => a -> Bool -> ResourceMonad DecisionResult
        handleConflict _ True = return $ Left HTTP.conflict409
        handleConflict res False = acceptContent (Right P11) res

decision P11 = const $ fmap hasLocationHeader (gets (Resp.getHeader Header.hLocation))
  where hasLocationHeader Nothing  = Right O20
        hasLocationHeader (Just _) = Left HTTP.seeOther303


decision O20 = decisionBranch hasBody (Right O18) (Left HTTP.noContent204)
  where hasBody :: (Resource a) => a -> ResourceMonadResult Bool
        hasBody = const $ fmap Right (gets Resp.hasBody)
-- 
-- v3n16 :: Decision
-- v3n16 = const $ toResponse HTTP.ok200

acceptContent :: (Resource a) => DecisionResult -> a -> ResourceMonad DecisionResult
acceptContent success res = do
  headers <- asks Wai.requestHeaders
  accepted <- contentTypesAccepted res

  case fmap snd  (find ((==Header.hContentType) . fst) headers) >>= MT.mapContent accepted of
    Nothing -> return $ Left HTTP.unsupportedMediaType415
    Just t -> fmap (mapToDecision success (Left HTTP.status500)) t

handle :: forall a. (Resource a) => a -> Wai.Request -> ResourceT IO Wai.Response
handle res req = fmap createResponse (runStateT (runReaderT (decide decisionStart res) req) Resp.emptyResponse)
   where decide node res = decision node res >>= processResult
         processResult result = case result of -- I really want to use Either short circuiting via fmap/>>= here!
                                  Right next -> decide next res
                                  Left s     -> return s
         createResponse (status, respData) = Wai.responseLBS status (Resp.responseHeaders respData) (fromMaybe LBS.empty (Resp.body respData))
