{-# LANGUAGE FlexibleContexts, RankNTypes, KindSignatures, OverloadedStrings #-}
module Arachnid.ResourceDefaultsSpec where

import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Test.Hspec
import Arachnid.Resources
import Arachnid.Routing
import Network.Wai

data TestResource = TestResource deriving (Show)
instance Resource TestResource

run :: forall (m :: * -> *) a.
             MonadBaseControl IO m =>
             ReaderT Request (ResourceT m) a -> m a

run res =
  runResourceT $ runReaderT res defaultRequest

spec :: Spec
spec = describe "Default resources" $ do
  defaultResourceSpecs TestResource
  defaultResourceSpecs (Segment "users" </> Segment "123" <:> TestResource)

defaultResourceSpecs :: (Resource a) => a -> Spec
defaultResourceSpecs a =
  describe ("Resource defaults for " ++ show a) $ do
    it "has serviceAvailable = True" $ do
      res <- run $ serviceAvailable a
      res `shouldBe` True

    it "has authorized = True" $ do
      res <- run $ authorized a
      res `shouldBe` True

    it "has forbidden = False" $ do
      res <- run $ forbidden a
      res `shouldBe` False

    it "has malformedRequest = False" $ do
      res <- run $ malformedRequest a
      res `shouldBe` False

    it "has requestURITooLong = False" $ do
      res <- run $ requestURITooLong a
      res `shouldBe` False

    it "has knownMethods = HTTP 1.1 Methods" $ do
      res <- run $ knownMethods a
      res `shouldMatchList` ["GET", "HEAD", "POST", "PUT", "DELETE", "TRACE", "CONNECT", "OPTIONS"]

    it "has allowedMethod = ['GET', 'HEAD']" $ do
      res <- run $ allowedMethods a
      res `shouldMatchList` ["GET", "HEAD"]

    it "has validContentHeaders = True" $ do
      res <- run $ validContentHeaders a
      res `shouldBe` True

    it "has knownContentType = True" $ do
      res <- run $ knownContentType a
      res `shouldBe` True

    it "has requestEntityTooLarge = False" $ do
      res <- run $ requestEntityTooLarge a
      res `shouldBe` False

    it "has options = []" $ do
      res <- run $ options a
      res `shouldBe` []

    it "has contentTypesProvided = []" $ do
      res <- run $ contentTypesProvided a
      map fst res `shouldMatchList` []  -- fst map hack to avoid lack of Show/Eq instance for second element

    it "has languageAvailable = True" $ do
       res <- run $ languageAvailable "en-US" a
       res `shouldBe` True

    it "has charsetsProvided = Nothing" $ do
      res <- run $ charsetsProvided a
      (map fst) `fmap` res `shouldBe` Nothing -- fst map hack to avoid lack of Show/Eq instance for second element

    it "has encodingsProvided = Nothing" $ do
      res <- run $ encodingsProvided a
      (map fst) `fmap` res `shouldBe` Nothing -- fst map hack to avoid lack of Show/Eq instance for second element

    it "has resourceExists = True" $ do
      res <- run $ resourceExists a
      res `shouldBe` True

    it "has generateETag = Nothing" $ do
      res <- run $ generateETag a
      res `shouldBe` Nothing

    it "has lastModified = Nothing" $ do
      res <- run $ lastModified a
      res `shouldBe` Nothing
