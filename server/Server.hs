{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Chan
import Control.Monad (forever)
import Data.Binary (decode)
import Network (listenOn, PortID(..), accept)
import Network.Socket (close)
import System.IO (hClose)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B

import qualified Actions as A
import qualified NetworkActions as NA
import Callbacks

import Spotify.Struct
import Spotify.Session

main :: IO ()
main = do
  action_queue <- newChan

  let sessionCallbacks = SessionCallbacks {
      loggedIn                  = loggedInCb
    , loggedOut                 = loggedOutCb
    , metadataUpdated           = metadataUpdatedCb
    , connectionError           = connectionErrorCb
    , messageToUser             = messageToUserCb
    , notifyMainThread          = notifyMainThreadCb action_queue
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

  _ <- forkIO (listen_for_actions session action_queue)

  forever $ process_action_queue action_queue

-- TODO make listen_for_actions handle multiple handles
listen_for_actions :: Session -> Chan A.Action -> IO ()
listen_for_actions session action_queue = do
    socket <- listenOn (PortNumber 3000)
    (handle, _, _) <- accept socket
    request <- C.hGetContents handle

    putStrLn "received data:"
    let actionToTake = (decode request :: NA.NetworkAction)
    putStrLn . show $ actionToTake
    case actionToTake of
        NA.Login username password -> writeChan action_queue (A.Login session (C.unpack username) (C.unpack password))
    putStrLn "sending back"

    C.hPut handle "msg received"

    hClose handle
    close socket



process_action_queue :: Chan A.Action -> IO ()
process_action_queue queue = readChan queue >>= process_action queue

-- TODO processing ProcessEvents should reset the process_event_scheduler delay
process_action :: Chan A.Action -> A.Action -> IO ()
process_action action_queue (A.ProcessEvents session) = do
    -- TODO handle error
    (Right nextTimeoutMs) <- processEvents session

    if nextTimeoutMs < 0
        then
            return()
        else do
            _ <- forkIO $ process_event_scheduler nextTimeoutMs session action_queue
            return ()

process_action _ (A.Login session username password) = do
    -- TODO handle sessionLogin err
    -- TODO add rememberMe and blob to the login action
    _ <- sessionLogin session username password False Nothing
    return ()

-- TODO process_event_scheduler should be constantly running in the background
process_event_scheduler :: Int -> Session -> Chan A.Action -> IO ()
process_event_scheduler next_delay_ms session queue = do
    threadDelay next_delay_us
    writeChan queue (A.ProcessEvents session)
  where
    next_delay_us = next_delay_ms * 1000
