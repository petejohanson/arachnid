import           Network.Wai
import           Network.Wai.Handler.Warp

import Arachnid
import Arachnid.Routing
import Arachnid.Types

data HelloWorld = HelloWorld

instance Resource HelloWorld

app :: Application
app = makeApp
  [ ("hello" </> "world", HelloWorld)
  ]

main :: IO ()
main = run 8080 app