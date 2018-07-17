
import Apollo
import Network (PortID(..))

main :: IO ()
main = apollo (PortNumber 3000) $ do
  get "/" $ do
    return $ status200 "Howdy!"
