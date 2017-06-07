{-# LANGUAGE FlexibleInstances #-}

module Arachnid.Routing
( match
, (</>)
, RouteElement (Segment, Capture, Rest)
, RouteMatch
, routeMatchCaptures
) where

data RouteElement = Segment String | Capture String | Rest deriving (Show)

type Route = [RouteElement]

data RouteMatch = RouteMatch { elements :: [RouteMatchElement] } deriving (Show)
data RouteMatchElement = StaticPath String | PathCapture String String | RestPath [String] deriving (Show)

routeMatchCaptures :: RouteMatch -> [(String, String)]
routeMatchCaptures=
  map (\(PathCapture name value) -> (name, value)) . filter isPathCapture . elements
  where isPathCapture (PathCapture _ _) = True
        isPathCapture _ = False

match :: Route -> [String] -> Maybe RouteMatch
match r p = (\m -> RouteMatch { elements = snd m }) `fmap` foldl match' (Just (p, [])) r

match' :: Maybe ([String], [RouteMatchElement]) -> RouteElement -> Maybe ([String], [RouteMatchElement])
match' Nothing _ = Nothing
match' (Just (p, m)) e = (\(r, n) ->  (r, m ++ [n])) `fmap` matchElement e p

matchElement :: RouteElement -> [String] -> Maybe ([String], RouteMatchElement)
matchElement (Segment s) (x:xs) = if s == x then Just (xs, StaticPath s) else Nothing
matchElement (Capture s) (x:xs) = Just (xs, PathCapture s x)
matchElement Rest path = Just ([], RestPath path)
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