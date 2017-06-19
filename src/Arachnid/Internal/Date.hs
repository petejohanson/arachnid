module Arachnid.Internal.Date
( parseHttpDate
) where

import qualified Data.ByteString as B

import Data.Time

import qualified Network.HTTP.Date as D

-- | Parse an HTTP RFC formatted date
--
-- $setup
-- >>> import qualified Data.ByteString.Char8 as BS
--
-- Examples:
--
-- >>> parseHttpDate $ BS.pack "Tue, 15 Nov 1994 08:12:31 GMT"
-- Just 1994-11-15 08:12:31 UTC
--
-- >>> parseHttpDate $ BS.pack "Tue, 15 Blue 1994 08:12:31 GMT"
-- Nothing
--
parseHttpDate :: B.ByteString -> Maybe UTCTime
parseHttpDate s = D.parseHTTPDate s >>= convertHTTPDateToUTCTime

convertHTTPDateToUTCTime :: D.HTTPDate -> Maybe UTCTime
convertHTTPDateToUTCTime hd = (\d -> UTCTime d (httpDateToDiffTime hd)) `fmap` httpDateToDay hd

httpDateToDay :: D.HTTPDate -> Maybe Day
httpDateToDay hd = fromGregorianValid (toInteger $ D.hdYear hd) (D.hdMonth hd) (D.hdDay hd)

httpDateToDiffTime :: D.HTTPDate -> DiffTime
httpDateToDiffTime hd = secondsToDiffTime $ toInteger (((D.hdHour hd * 60) + D.hdMinute hd) * 60 + D.hdSecond hd)

