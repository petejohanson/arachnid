{-# LANGUAGE FlexibleInstances, ExistentialQuantification #-}

module Arachnid.Routing
( match
, (</>)
, (<:>)
, RoutableResource
, resourceRoute
, Route
, RouteElement (Root, Segment, Capture, Rest)
, RouteMatch (RouteMatch, elements)
, RouteMatchElement (SegmentMatch, CaptureMatch, RestMatch)
, routeMatchCaptures
) where

import Arachnid.Resources
import Data.Text (Text, pack)

data RoutableResource = forall a. Resource a => RoutableResource a Route

instance Show RoutableResource where
  show (RoutableResource res route) = show route ++ " => " ++ show res

instance Resource RoutableResource where
  serviceAvailable (RoutableResource a _) = serviceAvailable a
  knownMethods (RoutableResource a _) = knownMethods a
  allowedMethods (RoutableResource a _) = allowedMethods a
  requestURITooLong (RoutableResource a _) = requestURITooLong a
  authorized (RoutableResource a _) = authorized a
  malformedRequest (RoutableResource a _) = malformedRequest a
  forbidden (RoutableResource a _) = forbidden a
  requestEntityTooLarge (RoutableResource a _) = requestEntityTooLarge a
  knownContentType (RoutableResource a _) = knownContentType a
  validContentHeaders (RoutableResource a _) = validContentHeaders a
  options (RoutableResource a _) = options a
  contentTypesProvided (RoutableResource a _) = contentTypesProvided a
  languageAvailable l (RoutableResource a _) = languageAvailable l a
  charsetsProvided (RoutableResource a _) = charsetsProvided a
  encodingsProvided (RoutableResource a _) = encodingsProvided a
  exists (RoutableResource a _) = exists a
  generateETag (RoutableResource a _) = generateETag a
  lastModified (RoutableResource a _) = lastModified a
  deleteResource (RoutableResource a _) = deleteResource a
  deleteCompleted (RoutableResource a _) = deleteCompleted a
  previouslyExisted (RoutableResource a _) = previouslyExisted a
  movedPermanently (RoutableResource a _) = movedPermanently a
  movedTemporarily (RoutableResource a _) = movedTemporarily a
  isConflict (RoutableResource a _) = isConflict a
  hasResponseBody (RoutableResource a _) = hasResponseBody a
  multipleChoices (RoutableResource a _) = multipleChoices a

(<:>) :: (Resource a, RouteCapture r) => r -> a -> RoutableResource
(<:>) route res = RoutableResource res (toRouteElements route)

resourceRoute :: RoutableResource -> Route
resourceRoute (RoutableResource _ route) = route

data RouteElement = Root | Segment String | Capture String | Rest deriving (Show)

type Route = [RouteElement]

data RouteMatch = RouteMatch { elements :: [RouteMatchElement] } deriving (Show, Eq)
data RouteMatchElement = RootMatch | SegmentMatch Text | CaptureMatch String Text | RestMatch [Text] deriving (Show, Eq)

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
matchElement Root [] = Just([], RootMatch)
matchElement Root _ = Nothing
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
