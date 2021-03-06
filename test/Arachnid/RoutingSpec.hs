module Arachnid.RoutingSpec where

import Test.Hspec
import Arachnid.Routing
import Data.Maybe
import Data.Text (pack)

spec :: Spec
spec =
  describe "Routing" $ do
    describe "The root route" $ do
      it "matches the basic route" $
        match [Root] [] `shouldSatisfy` isJust

      it "does not match complex routes" $
        match [Root] [pack "home"] `shouldBe` Nothing

    describe "A basic string segment" $ do
      it "matches the same string route" $
        match [Segment "home"] [pack "home"] `shouldSatisfy` isJust

      it "does not match a different string" $
        match [Segment "home"] [pack "users"] `shouldBe` Nothing

    describe "A capture segment" $
      it "Captures the value" $ do
        let
          captures = routeMatchCaptures `fmap` match ("home" </> Capture "name") [pack "home", pack "John"]
        captures `shouldBe` Just [("name", pack "John")]

    describe "A rest segment" $
      it "captures the remaining path items" $
        match [Rest] [pack "users", pack "123"] `shouldBe` Just RouteMatch { elements = [RestMatch [pack "users", pack "123"]] }

    describe "a complex route" $ do
      let route = "home" </> "users" </> Capture "id"

      it "does not match other paths that differ" $
        match route [pack "home", pack "posts", pack "123"] `shouldBe` Nothing

      it "Does not do partial matches" $
        match route [pack "home", pack "users"] `shouldBe` Nothing

      it "matches the expected path" $
        match route [pack "home", pack "users", pack "123"] `shouldBe` Just RouteMatch { elements = [SegmentMatch $ pack "home", SegmentMatch $ pack "users", CaptureMatch "id" (pack "123")]}
