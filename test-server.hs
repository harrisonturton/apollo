
import Apollo

main :: IO ()
main = apollo 3000 $ do
  get "/" $ do
    return $ status200 "Howdy!"
