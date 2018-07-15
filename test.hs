import Control.Monad.Reader

main = do
  let greeting = runReader greeter "Harry"
  putStrLn greeting

greeter :: Reader String String
greeter = do
  name <- ask
  return ("hello, " ++ name ++ "!")
