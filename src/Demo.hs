module Main where

import Control.Concurrent.STM
import Control.Exception.Extensible
import Control.Monad
import Network.Orchid.Wiki
import Network.Protocol.Uri
import Network.Salvia.Handler.ExtendedFileSystem
import Network.Salvia.Handlers
import Network.Salvia.Httpd
import Network.Socket
import Paths_orchid_demo
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Process.Pipe

-------------------------------------------------------------------------------

main :: IO ()
main = do
  argv <- getArgs
  prog <- getProgName
  conf <- parseOptions prog argv

  -- Extract data dir archive.
  when (extract conf)
    $ extractArchive (extractFrom conf) (extractTo conf)

  -- Run web server with wiki.
  run
    (asServer  conf)
    (filestore conf)
    (bindAddr  conf)
    (bindPort  conf)
    (dataDir   conf)
    (userDB    conf)

-------- wiki server ----------------------------------------------------------

stringToFileStore :: String -> Maybe FileStoreType
stringToFileStore kind = lookup kind [("Darcs", Darcs), ("Git", Git)]

run :: Bool -> String -> String -> PortNumber -> FilePath -> FilePath -> IO ()
run serve sfs addr prt dir users = do

  -- Initialize global state.
  db       <- readUserDatabase users
  ioconfig <- defaultConfig 
  count    <- atomically $ newTVar 0
  sessions <- mkSessions :: IO (Sessions (UserPayload ()))
  addr'    <- inet_addr addr

  case stringToFileStore sfs of
    Nothing -> putStrLn $ "Error: No such filestore: " ++ sfs
    Just fs -> do

      -- Alter config and setup handler.
      let cfg       = ioconfig { listenAddr = addr', listenPort = prt }
      let myHandler = if serve
                       then const $ hExtendedFileSystem dir
                       else hWiki fs dir dir db

      -- Warn about serving user database.
      when (maybe False (const True) (jail dir users))
        $ putStrLn "Warning: serving user database to the evil outside world."

      -- Print status messages and..
      putStrLn $ concat ["Listening on ", addr, ":", show (listenPort cfg), "."]
      putStrLn $ concat ["Using ", dir, " as wiki repository."]

      -- ..off we go!
      putStrLn "Server started."
      start cfg $ hSessionEnv count sessions myHandler

extractArchive :: FilePath -> FilePath -> IO ()
extractArchive from to = do
  putStrLn $ concat ["Extracting repository from ", from, " to ", to, "."]
  s <- pipeString [("unzip", [from, "-d", to])] ""
  evaluate $ length s
  return ()

-------- command line options parser ------------------------------------------

-- Application configuration type.
data AppConfig =
  AppConfig {
    extract     :: Bool
  , extractFrom :: String
  , extractTo   :: String
  , dataDir     :: String
  , userDB      :: String
  , asServer    :: Bool
  , filestore   :: String
  , bindAddr    :: String
  , bindPort    :: PortNumber
  } deriving Show

-- Default application config.
defaultAppConfig :: IO AppConfig
defaultAppConfig = do
  dir <- getDataFileName "data.zip"
  return $
    AppConfig {
      extract     = False
    , extractFrom = dir
    , extractTo   = "/tmp"
    , dataDir     = "/tmp/data"
    , userDB      = "/tmp/data/_user.db"
    , asServer    = False
    , filestore   = "Darcs"
    , bindAddr    = "0.0.0.0"
    , bindPort    = 8080
    } 

-- Command line argument declaration.
options :: [OptDescr (AppConfig -> AppConfig)]
options =
  let
    optExtract     = NoArg            (\  c -> c { extract     = True })
    optExtractFrom = OptArg (maybe id (\a c -> c { extractFrom = a    })) "<from-cabal>"
    optExtractTo   = OptArg (maybe id (\a c -> c { extractTo   = a    })) "/tmp"
    optDataDir     = OptArg (maybe id (\a c -> c { dataDir     = a    })) "/tmp/data"
    optUserDB      = OptArg (maybe id (\a c -> c { userDB      = a    })) "/tmp/data/_user.db"
    optAsServer    = NoArg            (\  c -> c { asServer    = True }) 
    optFileStore   = OptArg (maybe id (\a c -> c { filestore   = a    })) "Darcs"
    optBindAddr    = OptArg (maybe id (\a c -> c { bindAddr    = a    })) "0.0.0.0"
    optBindPort    = OptArg (maybe id (\a c -> c { bindPort    = fromIntegral (read a :: Int) })) "8080"
  in [
    Option [] ["extract"]    optExtract     "extract a demo repository from archive"
  , Option [] ["source-zip"] optExtractFrom "location of repository archive"
  , Option [] ["extract-to"] optExtractTo   "location to extract demo archive to"
  , Option [] ["data-dir"]   optDataDir     "run demo with this repository"
  , Option [] ["user-db"]    optUserDB      "location of user database file"
  , Option [] ["as-server"]  optAsServer    "do not start wiki but serve directory"
  , Option [] ["filestore"]  optFileStore   "filestore type: Darcs or Git"
  , Option [] ["address"]    optBindAddr    "address to listen on"
  , Option [] ["port"]       optBindPort    "port to bind to"
  ]

-- Parser for the command line options.
parseOptions :: String -> [String] -> IO AppConfig
parseOptions prog argv = do
   def <- defaultAppConfig
   case getOpt Permute options argv of
     (o, _, [])   -> return $ foldl (flip($)) def o
     (_, _, errs) -> putStrLn (concat errs ++ usageInfo header options) >> exitFailure
   where header = "Usage: " ++ prog ++ " [OPTION...]"

