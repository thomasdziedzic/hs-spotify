import Control.Concurrent (threadDelay)
import Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast
import qualified Data.ByteString.Lazy as B

import Spotify.Session
import Spotify.Struct
import Spotify.Error

loggedInCb :: Session -> Error -> IO ()
loggedInCb session err = do
  putStrLn $ "in logged_in_cb with err: " ++ show (sp_error_message err)

loggedOutCb :: Session -> IO ()
loggedOutCb session = do
  putStrLn "in logged_out_cb"

metadataUpdatedCb :: Session -> IO ()
metadataUpdatedCb session = do
  putStrLn "in metadata_updated_cb"

connectionErrorCb :: Session -> Error -> IO ()
connectionErrorCb session err = do
  putStrLn "in connection_error callback"

messageToUserCb :: Session -> String -> IO ()
messageToUserCb session msg = do
  putStrLn "in message_to_user callback"

notifyMainThreadCb :: Broadcast () -> Session -> IO ()
notifyMainThreadCb process_events_broadcast session = do
  Broadcast.broadcast process_events_broadcast ()
  putStrLn "in notify_main_thread callback"

musicDeliveryCb :: Session -> AudioFormat -> B.ByteString -> IO Int
musicDeliveryCb session audioformat frames = do
  putStrLn "in music_delivery_cb callback"
  return 1

playTokenLostCb :: Session -> IO ()
playTokenLostCb session = do
  putStrLn "in play_token_lost_cb callback"

logMessageCb :: Session -> String -> IO ()
logMessageCb session logString = do
  putStrLn "in log_message_cb"
  putStrLn $ "log data: " ++ logString

endOfTrackCb :: Session -> IO ()
endOfTrackCb session = do
  putStrLn "in end_of_track_cb callback"

streamingErrorCb :: Session -> Error -> IO ()
streamingErrorCb session err = do
  putStrLn "in streaming_error_cb callback"

userinfoUpdatedCb :: Session -> IO ()
userinfoUpdatedCb session = do
  putStrLn "in userinfo_updated_cb callback"

startPlaybackCb :: Session -> IO ()
startPlaybackCb session = do
  putStrLn "in start_playback_cb callback"

stopPlaybackCb :: Session -> IO ()
stopPlaybackCb session = do
  putStrLn "in stop_playback_cb callback"

getAudioBufferStatsCb :: Session -> AudioBufferStats -> IO ()
getAudioBufferStatsCb session audio_buffer_stats= do
  putStrLn "in get_audio_buffer_stats_cb callback"

offlineStatusUpdatedCb :: Session -> IO ()
offlineStatusUpdatedCb session = do
  putStrLn "in offline_status_updated_cb callback"

offlineErrorCb :: Session -> Error -> IO ()
offlineErrorCb session err = do
  putStrLn "in offline_error_cb callback"

credentialsBlobUpdatedCb :: Session -> String -> IO ()
credentialsBlobUpdatedCb session log_data = do
  putStrLn "in credentials_blob_updated_cb callback"

connectionstateUpdatedCb :: Session -> IO ()
connectionstateUpdatedCb session = do
  putStrLn "in connectionstate_updated_cb callback"

scrobbleErrorCb :: Session -> Error -> IO ()
scrobbleErrorCb session err = do
  putStrLn "in scrobble_error_cb callback"

privateSessionModeChangedCb :: Session -> Bool -> IO ()
privateSessionModeChangedCb session is_private  = do
  putStrLn "in private_session_mode_changed_cb callback"

main :: IO ()
main = do
  key <- B.readFile $ "spotify_appkey.key"

  process_events_broadcast <- Broadcast.new

  let sessionCallbacks = SessionCallbacks {
      loggedIn                  = loggedInCb
    , loggedOut                 = loggedOutCb
    , metadataUpdated           = metadataUpdatedCb
    , connectionError           = connectionErrorCb
    , messageToUser             = messageToUserCb
    , notifyMainThread          = notifyMainThreadCb process_events_broadcast
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
      , deviceId                     = "deviceId"
      , proxy                        = ""
      , proxyUsername                = ""
      , proxyPassword                = ""
      , caCertsFilename              = ""
      , tracefile                    = "tracefile"
      }

  -- TODO error handling
  (Right session) <- sessionCreate sessionConfig

  putStrLn "Enter your username"
  username <- getLine
  putStrLn "Enter your password"
  password <- getLine

  -- TODO error handling
  loginResult <- sessionLogin session username password False Nothing
  case loginResult of
      Nothing -> return ()
      Just err -> putStrLn $ "login result: " ++ sp_error_message err

  delay_process_events process_events_broadcast session

delay_process_events :: Broadcast () -> Session -> IO ()
delay_process_events process_events_broadcast session = do
  putStrLn "delay_process_events"
  Broadcast.listen process_events_broadcast

  process_events process_events_broadcast session

delay_process_events':: Broadcast () -> Integer -> Session -> IO ()
delay_process_events' process_events_broadcast timeout session = do
  putStrLn "delay_process_events'"
  Broadcast.listenTimeout process_events_broadcast timeout

  process_events process_events_broadcast session

process_events :: Broadcast () -> Session -> IO ()
process_events process_events_broadcast session = do
  -- TODO error handling
  (Right nextTimeout) <- processEvents session

  let delay_us = (1000 * fromIntegral nextTimeout)
  putStrLn $ "delaying " ++ show delay_us ++ "us"

  Broadcast.silence process_events_broadcast

  delay_process_events' process_events_broadcast delay_us session
