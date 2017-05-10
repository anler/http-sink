{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import           Control.Monad                        (forM_)
import qualified Data.Attoparsec.Text                 as At
import           Data.Default.Class                   (def)
import           Data.Either                          (either)
import           Data.String                          (fromString)
import qualified Data.Text                            as Text
import           Data.Time                            (getZonedTime)
import qualified Network.HTTP.Types.Status            as Status
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (setHost, setPort)
import qualified Network.Wai.Middleware.RequestLogger as Loggers
import           System.Console.Docopt
import           System.Environment                   (getArgs)
import           System.Exit                          (exitFailure)
import           Web.Scotty


main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs
  port <- args `getArgOrExit` (shortOption 'p')
  host <- args `getArgOrExit` (shortOption 'H')
  loggingMiddleware <- mkLoggingMiddleware
  either
    (const (exitWith "Invalid port. It has to be a numeric string."))
    (runServer [loggingMiddleware] host)
    (parsePort port)


runServer :: [Middleware] -> String -> Int -> IO ()
runServer middlewares host port = do
  let opts = def { verbose = 1
                 , settings = setPort port $ setHost (fromString host) $ settings def }
  scottyOpts opts (app middlewares)


app :: [Middleware] -> ScottyM ()
app middlewares = do
  forM_ middlewares middleware
  matchAny (regex "^/(.*)") $ status Status.noContent204


parsePort :: String -> Either String Int
parsePort = At.parseOnly At.decimal . Text.pack


mkLoggingMiddleware :: IO Middleware
mkLoggingMiddleware = return $ Loggers.logStdoutDev . printCurrentTimeMiddleware

printCurrentTimeMiddleware :: Middleware
printCurrentTimeMiddleware application req respond = do
  time <- getZonedTime
  putStrLn $ "\n" ++ show time
  application req respond


getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns


exitWith :: String -> IO ()
exitWith reason = putStrLn reason >> exitFailure


patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]
