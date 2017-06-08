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
        let isNothing Nothing = True
            isNothing _ = False
        match [Root] [pack "home"] `shouldSatisfy` isNothing

    describe "A basic string segment" $ do
      it "matches the same string route" $ do
        let isJust (Just _) = True
            isJust _ = False
        match [Segment "home"] [pack "home"] `shouldSatisfy` isJust

      it "does not match a different string" $ do
        let isNothing Nothing = True
            isNothing _ = False
        match [Segment "home"] [pack "users"] `shouldSatisfy` isNothing

    describe "A capture segment" $ do
      it "Captures the value" $ do
        captures `shouldBe` Just [("name", pack "John")]
        where
          captures = routeMatchCaptures `fmap` match ("home" </> Capture "name") [pack "home", pack "John"]
