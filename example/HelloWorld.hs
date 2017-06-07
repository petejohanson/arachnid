{-# LANGUAGE ExistentialQuantification #-}

import           Network.Wai
import           Network.Wai.Handler.Warp

import Arachnid
import Arachnid.Routing
import Arachnid.Resources

data RootResource = RootResource

instance Resource RootResource where
  serviceAvailable = const $ return False

data HelloWorld = HelloWorld

instance Resource HelloWorld

data NoAuth = NoAuth

instance Resource NoAuth where
  authorized = const $ return False

app :: Application
app = makeApp
  [ (Root <:> RootResource)
  , ("hello" </> "world" <:> HelloWorld)
  , ("stay" </> "away" <:> NoAuth)
  ]

main :: IO ()
main = do
  putStrLn "Try http://localhost:8080/hello/world"
  run 8080 app