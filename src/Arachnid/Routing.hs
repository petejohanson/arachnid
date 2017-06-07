{-# LANGUAGE FlexibleInstances #-}

module Arachnid.Routing
( match
, (</>)
, Route
, RouteElement (Segment, Capture, Rest)
, RouteMatch
, RouteMatchElement (SegmentMatch, CaptureMatch, RestMatch)
, routeMatchCaptures
) where

import Data.Text (Text, pack)

data RouteElement = Segment String | Capture String | Rest deriving (Show)

type Route = [RouteElement]

data RouteMatch = RouteMatch { elements :: [RouteMatchElement] } deriving (Show)
data RouteMatchElement = SegmentMatch Text | CaptureMatch String Text | RestMatch [Text] deriving (Show)

routeMatchCaptures :: RouteMatch -> [(String, Text)]
routeMatchCaptures=
  map (\(CaptureMatch name value) -> (name, value)) . filter isPathCapture . elements
  where isPathCapture (CaptureMatch _ _) = True
        isPathCapture _ = False

match :: Route -> [Text] -> Maybe RouteMatch
match r p = (\m -> RouteMatch { elements = snd m }) `fmap` foldl match' (Just (p, [])) r

match' :: Maybe ([Text], [RouteMatchElement]) -> RouteElement -> Maybe ([Text], [RouteMatchElement])
match' Nothing _ = Nothing
match' (Just (p, m)) e = (\(r, n) ->  (r, m ++ [n])) `fmap` matchElement e p

matchElement :: RouteElement -> [Text] -> Maybe ([Text], RouteMatchElement)
matchElement (Segment s) (x:xs) = if pack s == x then Just (xs, SegmentMatch x) else Nothing
matchElement (Capture s) (x:xs) = Just (xs, CaptureMatch s x)
matchElement Rest path = Just ([], RestMatch path)
matchElement _ [] = Nothing

class RouteCapture a where
  toRouteElements :: a -> [RouteElement]
  (</>) :: (RouteCapture b) => a -> b -> [RouteElement]
  (</>) a b = toRouteElements a ++ toRouteElements b

instance RouteCapture Route where
  toRouteElements r = r

instance RouteCapture RouteElement where
  toRouteElements other = [other]

instance RouteCapture String where
  toRouteElements s = [Segment s]