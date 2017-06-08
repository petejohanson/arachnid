module Arachnid.RoutingSpec where

import Test.Hspec
import Arachnid.Routing
import Data.Text (pack)

spec :: Spec
spec = do
  describe "Routing" $ do
    describe "The root route" $ do
      it "matches the basic route" $ do
        let isJust (Just _) = True
            isJust _ = False
        match [Root] [] `shouldSatisfy` isJust

      it "does not match complex routes" $ do
        match [Root] [pack "home"] `shouldBe` Nothing

    describe "A basic string segment" $ do
      it "matches the same string route" $ do
        let isJust (Just _) = True
            isJust _ = False
        match [Segment "home"] [pack "home"] `shouldSatisfy` isJust

      it "does not match a different string" $ do
        match [Segment "home"] [pack "users"] `shouldBe` Nothing

    describe "A capture segment" $ do
      it "Captures the value" $ do
        let
          captures = routeMatchCaptures `fmap` match ("home" </> Capture "name") [pack "home", pack "John"]
        captures `shouldBe` Just [("name", pack "John")]

    describe "A rest segment" $ do
      it "captures the remaining path items" $ do
        match [Rest] [pack "users", pack "123"] `shouldBe` Just RouteMatch { elements = [RestMatch [pack "users", pack "123"]] }

    describe "a complex route" $ do
      let route = "home" </> "users" </> Capture "id"

      it "does not match other paths that differ" $ do
        match route [pack "home", pack "posts", pack "123"] `shouldBe` Nothing

      it "Does not do partial matches" $ do
        match route [pack "home", pack "users"] `shouldBe` Nothing

      it "matches the expected path" $ do
        match route [pack "home", pack "users", pack "123"] `shouldBe` Just RouteMatch { elements = [SegmentMatch $ pack "home", SegmentMatch $ pack "users", CaptureMatch "id" (pack "123")]}
