{-# LANGUAGE FlexibleContexts #-}
module Arachnid.ResourceDefaultsSpec where

import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Test.Hspec
import Arachnid.Resources
import Network.Wai

data TestResource = TestResource
instance Resource TestResource

run res = runResourceT $ runReaderT res defaultRequest

spec :: Spec
spec = do
  describe "Resource defaults" $ do
    it "has serviceAvailable = True" $ do
      res <- run $ serviceAvailable TestResource
      res `shouldBe` True

    it "has authorized = True" $ do
      res <- run $ authorized TestResource
      res `shouldBe` True

    it "has forbidden = False" $ do
      res <- run $ forbidden TestResource
      res `shouldBe` False

