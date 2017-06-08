{-# LANGUAGE FlexibleContexts, RankNTypes, KindSignatures, OverloadedStrings #-}
module Arachnid.ResourceDefaultsSpec where

import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Test.Hspec
import Arachnid.Resources
import Network.Wai

data TestResource = TestResource
instance Resource TestResource

run :: forall (m :: * -> *) a.
             MonadBaseControl IO m =>
             ReaderT Request (ResourceT m) a -> m a

run res = runResourceT $ runReaderT res defaultRequest

spec :: Spec
spec =
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

    it "has malformedRequest = False" $ do
      res <- run $ malformedRequest TestResource
      res `shouldBe` False

    it "has requestURITooLong = False" $ do
      res <- run $ requestURITooLong TestResource
      res `shouldBe` False

    it "has knownMethods = HTTP 1.1 Methods" $ do
          res <- run $ knownMethods TestResource
          res `shouldMatchList` ["GET", "HEAD", "POST", "PUT", "DELETE", "TRACE", "CONNECT", "OPTIONS"]
