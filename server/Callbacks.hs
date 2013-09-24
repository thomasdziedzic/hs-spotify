module Callbacks (
      loggedInCb
    , loggedOutCb
    , metadataUpdatedCb
    , connectionErrorCb
    , messageToUserCb
    , notifyMainThreadCb
    , musicDeliveryCb
    , playTokenLostCb
    , logMessageCb
    , endOfTrackCb
    , streamingErrorCb
    , userinfoUpdatedCb
    , startPlaybackCb
    , stopPlaybackCb
    , getAudioBufferStatsCb
    , offlineStatusUpdatedCb
    , offlineErrorCb
    , credentialsBlobUpdatedCb
    , connectionstateUpdatedCb
    , scrobbleErrorCb
    , privateSessionModeChangedCb
) where

import Spotify.Error
import Spotify.Struct
import Spotify.Session

import Control.Concurrent.Chan (Chan, writeChan)
import qualified Data.ByteString.Lazy as B

import Actions

loggedInCb :: Session -> Error -> IO ()
loggedInCb session err = do
  putStrLn $ "in logged_in_cb with err: " ++ sp_error_message err

loggedOutCb :: Session -> IO ()
loggedOutCb session = do
  putStrLn "in logged_out_cb"

metadataUpdatedCb :: Session -> IO ()
metadataUpdatedCb session = do
  putStrLn "in metadata_updated_cb"

connectionErrorCb :: Session -> Error -> IO ()
connectionErrorCb session err = do
  putStrLn $ "in connection_error callback with err: " ++ sp_error_message err

messageToUserCb :: Session -> String -> IO ()
messageToUserCb session msg = do
  putStrLn $ "in message_to_user callback with message: " ++ msg

notifyMainThreadCb :: Chan Action -> Session -> IO ()
notifyMainThreadCb queue session = do
  writeChan queue (ProcessEvents session)

  putStrLn "in notify_main_thread callback"

musicDeliveryCb :: Session -> AudioFormat -> B.ByteString -> IO Int
musicDeliveryCb session audioformat frames = do
  putStrLn "in music_delivery_cb callback"
  return 1

playTokenLostCb :: Session -> IO ()
playTokenLostCb session = do
  putStrLn "in play_token_lost_cb callback"

logMessageCb :: Session -> String -> IO ()
logMessageCb session logMsg = do
  putStrLn $ "in log_message_cb with log: " ++ logMsg

endOfTrackCb :: Session -> IO ()
endOfTrackCb session = do
  putStrLn "in end_of_track_cb callback"

streamingErrorCb :: Session -> Error -> IO ()
streamingErrorCb session err = do
  putStrLn $ "in streaming_error_cb callback with error: " ++ sp_error_message err

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
getAudioBufferStatsCb session audioBufferStats = do
  putStrLn "in get_audio_buffer_stats_cb callback"

offlineStatusUpdatedCb :: Session -> IO ()
offlineStatusUpdatedCb session = do
  putStrLn "in offline_status_updated_cb callback"

offlineErrorCb :: Session -> Error -> IO ()
offlineErrorCb session err = do
  putStrLn $ "in offline_error_cb callback with error: " ++ sp_error_message err

credentialsBlobUpdatedCb :: Session -> String -> IO ()
credentialsBlobUpdatedCb session credentialsBlob = do
  putStrLn $ "in credentials_blob_updated_cb callback with blob: " ++ credentialsBlob

connectionstateUpdatedCb :: Session -> IO ()
connectionstateUpdatedCb session = do
  putStrLn "in connectionstate_updated_cb callback"

scrobbleErrorCb :: Session -> Error -> IO ()
scrobbleErrorCb session err = do
  putStrLn $ "in scrobble_error_cb callback with err: " ++ sp_error_message err

privateSessionModeChangedCb :: Session -> Bool -> IO ()
privateSessionModeChangedCb session isPrivate  = do
  putStrLn $ "in private_session_mode_changed_cb callback with isPrivate: " ++ show isPrivate
