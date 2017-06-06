module Arachnid.RoutingSpec where

import Test.Hspec
import Arachnid.Routing

spec :: Spec
spec =
  describe "Routing" $ do
    describe "A basic string segment" $
      it "matches the same string route" $ do
        let isJust (Just _) = True
            isJust _ = False
        match (Segment "home") "home" `shouldSatisfy` isJust

    describe "A capture segment" $
      it "Captures the value" $
        captures `shouldBe` Just [("name", "John")]
        where
          captures = routeMatchCaptures `fmap` match ("home" </> Capture "name") "home/John"
