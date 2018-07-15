
import Control.Monad
import System.IO
import Network
import Control.Concurrent

main = withSocketsDo $ do
  sock <- listenOn $ PortNumber 3000
  putStrLn "Listening on port 3000..."
  forever $ do
    (handle, hostname, _) <- accept sock
    handleAccept handle hostname

handleAccept :: Handle -> String -> IO ()
handleAccept handle hostname = do
  putStrLn $ "Handling request from " ++ hostname
  reqLines <- hGetContents handle
  putStrLn $ show reqLines
