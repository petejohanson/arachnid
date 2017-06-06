{-# LANGUAGE FlexibleInstances #-}

module Arachnid.Routing
( match
, (</>)
, RouteElement (Route, Segment, Capture, Rest)
, RouteMatch
, routeMatchCaptures
) where

data RouteElement = Route [RouteElement] | Segment String | Capture String | Rest deriving (Show)

data RouteMatch = RouteMatch { elements :: [RouteMatchElement] } deriving (Show)
data RouteMatchElement = StaticPath String | PathCapture String String | RestPath [String] deriving (Show)

routeMatchCaptures :: RouteMatch -> [(String, String)]
routeMatchCaptures=
  map (\(PathCapture name value) -> (name, value)) . filter isPathCapture . elements
  where isPathCapture (PathCapture _ _) = True
        isPathCapture _ = False

match :: RouteElement -> String -> Maybe RouteMatch
match = const $ const Nothing

class RouteCapture a where
  toRouteElements :: a -> [RouteElement]
  (</>) :: (RouteCapture b) => a -> b -> RouteElement
  (</>) a b = Route (toRouteElements a ++ toRouteElements b)

instance RouteCapture RouteElement where
  toRouteElements (Route routes) = routes
  toRouteElements other = [other]
  (</>) (Route routes) other = Route (routes ++ toRouteElements other)
  (</>) element other = Route (element : toRouteElements other)

instance RouteCapture String where
  toRouteElements s = [Segment s]

route :: RouteElement
route = "test" </> Capture "bar"