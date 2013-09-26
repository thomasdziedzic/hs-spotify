{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever)
import Data.Binary (decode)
import Network (listenOn, PortID(..), accept, Socket)
import Network.Socket (close)
import System.IO (hClose)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import GHC.IO.Handle (Handle)
import Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast

import qualified Actions as A
import qualified NetworkActions as NA
import Callbacks

import Spotify.Struct
import Spotify.Session
import Spotify.Link

main :: IO ()
main = do
  action_queue <- newChan
  scheduleProcess <- Broadcast.new
  processComplete <- Broadcast.new

  let sessionCallbacks = SessionCallbacks {
      loggedIn                  = loggedInCb
    , loggedOut                 = loggedOutCb
    , metadataUpdated           = metadataUpdatedCb
    , connectionError           = connectionErrorCb
    , messageToUser             = messageToUserCb
    , notifyMainThread          = notifyMainThreadCb scheduleProcess
    , musicDelivery             = musicDeliveryCb
    , playTokenLost             = playTokenLostCb
    , logMessage                = logMessageCb
    , endOfTrack                = endOfTrackCb
    , streamingError            = streamingErrorCb
    , userinfoUpdated           = userinfoUpdatedCb
    , startPlayback             = startPlaybackCb
    , stopPlayback              = stopPlaybackCb
    , getAudioBufferStats       = getAudioBufferStatsCb
    , offlineStatusUpdated      = offlineStatusUpdatedCb
    , offlineError              = offlineErrorCb
    , credentialsBlobUpdated    = credentialsBlobUpdatedCb
    , connectionstateUpdated    = connectionstateUpdatedCb
    , scrobbleError             = scrobbleErrorCb
    , privateSessionModeChanged = privateSessionModeChangedCb
    }

  key <- B.readFile $ "spotify_appkey.key"
  
  let sessionConfig = SessionConfig {
        cacheLocation                = "/tmp/spotify"
      , settingsLocation             = "/tmp/spotify"
      , applicationKey               = key
      , userAgent                    = "hs-spotify"
      , callbacks                    = sessionCallbacks
      , userdata                     = B.empty
      , compressPlaylists            = False
      , dontSaveMetadataForPlaylists = False
      , initiallyUnloadPlaylists     = False
      , deviceId                     = "hs-spotify"
      , proxy                        = ""
      , proxyUsername                = ""
      , proxyPassword                = ""
      , caCertsFilename              = ""
      , tracefile                    = "tracefile"
      }

  -- TODO handle error
  (Right session) <- sessionCreate sessionConfig

  -- TODO use simple server package
  _ <- forkIO (listen_for_actions session action_queue)

  _ <- forkIO $ processEventScheduler scheduleProcess processComplete session action_queue

  forever $ process_action_queue action_queue processComplete

listen_for_actions :: Session -> Chan A.Action -> IO ()
listen_for_actions session action_queue = do
    socket <- listenOn (PortNumber 3000)

    listen_for_actions' socket session action_queue

listen_for_actions' :: Socket -> Session -> Chan A.Action -> IO ()
listen_for_actions' socket session action_queue = do
    (handle, _, _) <- accept socket

    putStrLn "accepted socket"

    _ <- forkIO $ handleRequest handle session action_queue

    listen_for_actions' socket session action_queue

handleRequest :: Handle -> Session -> Chan A.Action -> IO ()
handleRequest handle session actionQueue = do
    request <- C.hGetContents handle

    putStrLn "received data:"
    let actionToTake = (decode request :: NA.NetworkAction)
    putStrLn . show $ actionToTake
    case actionToTake of
        NA.Login username password -> writeChan actionQueue (A.Login session (C.unpack username) (C.unpack password))
        NA.Load link -> writeChan actionQueue (A.Load session (C.unpack link))
        NA.Play -> writeChan actionQueue (A.Play session)
        NA.Stop -> writeChan actionQueue (A.Stop session)
    putStrLn "sending back"

    C.hPut handle "msg received"

    hClose handle

process_action_queue :: Chan A.Action -> Broadcast Integer -> IO ()
process_action_queue queue processDone = readChan queue >>= process_action processDone

process_action :: Broadcast Integer -> A.Action -> IO ()
process_action processDone (A.ProcessEvents session) = do
    -- TODO handle error
    (Right nextTimeoutMs) <- processEvents session

    if nextTimeoutMs < 0
        then
            return()
        else do
            Broadcast.broadcast processDone (fromIntegral nextTimeoutMs)
            return ()

process_action _ (A.Login session username password) = do
    -- TODO handle sessionLogin err
    -- TODO add rememberMe and blob to the login action
    _ <- sessionLogin session username password False Nothing
    return ()

process_action _ (A.Load session url) = do
    link <- linkCreateFromString url
    -- TODO handle case where we get back nothing signifying the link was invalid
    (Just track) <- linkAsTrack link
    -- TODO handle errors
    _ <- sessionPlayerLoad session track
    return ()

process_action _ (A.Play session) = do
    -- TODO handle errors
    _ <- sessionPlayerPlay session True
    return ()

process_action _ (A.Stop session) = do
    -- TODO handle errors
    _ <- sessionPlayerPlay session False
    return ()

processEventScheduler :: Broadcast () -> Broadcast Integer -> Session -> Chan A.Action -> IO ()
processEventScheduler scheduleProcess processDone session queue = do
    Broadcast.listen scheduleProcess
    queueProcessEvent scheduleProcess processDone session queue

processEventScheduler' :: Broadcast () -> Broadcast Integer -> Integer -> Session -> Chan A.Action -> IO ()
processEventScheduler' scheduleProcess processDone timeoutMs session queue = do
    Broadcast.listenTimeout scheduleProcess timeoutUs
    queueProcessEvent scheduleProcess processDone session queue
  where
    timeoutUs = timeoutMs * 1000

queueProcessEvent :: Broadcast () -> Broadcast Integer -> Session -> Chan A.Action -> IO ()
queueProcessEvent scheduleProcess processDone session queue = do
    writeChan queue (A.ProcessEvents session)
    timeoutMs <- Broadcast.listen processDone
    Broadcast.silence processDone
    Broadcast.silence scheduleProcess

    processEventScheduler' scheduleProcess processDone timeoutMs session queue
