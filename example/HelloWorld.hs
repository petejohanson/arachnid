{-# LANGUAGE ExistentialQuantification #-}

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Media.MediaType as MT
import           Network.Wai
import           Network.Wai.Handler.Warp

import Control.Monad.Reader
import Data.ByteString.Char8 (pack)
import Arachnid
import Arachnid.Routing
import Arachnid.Resources

data RootResource = RootResource deriving (Show)

instance Resource RootResource where
  -- serviceAvailable = const $ return $ Left $ HTTP.mkStatus 451 (pack "Unavailable For Legal Reasons")
  exists = const $ return $ return False
  previouslyExisted = const $ return $ return True
  movedPermanently = const $ return $ return $ Just $ pack "https://google.com/"

data HelloWorld = HelloWorld deriving (Show)

instance Resource HelloWorld where
  allowedMethods = const $ return $ return [HTTP.methodGet, HTTP.methodHead, HTTP.methodOptions]
  options = const $ return$ return  [(H.hAge, pack "1234")]
  contentTypesProvided = const $ return $ return [(pack "text" MT.// pack "plain", return $ return $ pack "Testing")]

data NoAuth = NoAuth deriving (Show)

instance Resource NoAuth where
  authorized = const $ return $ return False

app :: Application
app = makeApp
  [ Root <:> RootResource
  , "hello" </> "world" <:> HelloWorld
  , "stay" </> "away" <:> NoAuth
  ]

main :: IO ()
main = do
  putStrLn "Try http://localhost:8080/hello/world"
  run 8080 app
