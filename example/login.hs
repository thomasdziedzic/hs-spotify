import Control.Concurrent (threadDelay)
import Bindings.Spotify.Session
import Bindings.Spotify.Error
import Bindings.Spotify.Struct
import Bindings.Spotify.CommonTypes
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Utils
import qualified Data.ByteString as BS
import Control.Monad ((<=<))
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.IO
import Foreign.Marshal.Array
import Control.Concurrent.Broadcast (Broadcast)
import qualified Control.Concurrent.Broadcast as Broadcast

logged_in_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
logged_in_cb session_ptr err = do
  putStrLn $ "in logged_in_cb with err: " ++ show err
  hFlush stdout

logged_out_cb :: Ptr Sp_Session -> IO ()
logged_out_cb session_ptr = do
  putStrLn "in logged_out_cb"
  hFlush stdout

metadata_updated_cb :: Ptr Sp_Session -> IO ()
metadata_updated_cb session_ptr = do
  putStrLn "in metadata_updated_cb"
  hFlush stdout

connection_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
connection_error_cb session_ptr err = do
  putStrLn "in connection_error callback"
  hFlush stdout

message_to_user_cb :: Ptr Sp_Session -> CString -> IO ()
message_to_user_cb session_ptr msg = do
  putStrLn "in message_to_user callback"
  hFlush stdout

notify_main_thread_cb :: Broadcast () -> Ptr Sp_Session -> IO ()
notify_main_thread_cb process_events_broadcast session_ptr = do
  Broadcast.broadcast process_events_broadcast ()

  putStrLn "in notify_main_thread callback"
  hFlush stdout

music_delivery_cb :: MusicDeliveryCb
music_delivery_cb session_ptr audioformat_ptr frames_ptr num_frames = do
  putStrLn "in music_delivery_cb callback"
  hFlush stdout
  return 1

play_token_lost_cb :: Ptr Sp_Session -> IO ()
play_token_lost_cb session_ptr = do
  putStrLn "in play_token_lost_cb callback"
  hFlush stdout

log_message_cb :: Ptr Sp_Session -> CString -> IO ()
log_message_cb session_ptr log_data = do
  putStrLn "in log_message_cb"
  logString <- peekCString log_data
  putStrLn $ "log data: " ++ logString
  hFlush stdout

end_of_track_cb :: Ptr Sp_Session -> IO ()
end_of_track_cb session_ptr = do
  putStrLn "in end_of_track_cb callback"
  hFlush stdout

streaming_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
streaming_error_cb session_ptr err = do
  putStrLn "in streaming_error_cb callback"
  hFlush stdout

userinfo_updated_cb :: Ptr Sp_Session -> IO ()
userinfo_updated_cb session_ptr = do
  putStrLn "in userinfo_updated_cb callback"
  hFlush stdout

start_playback_cb :: Ptr Sp_Session -> IO ()
start_playback_cb session_ptr = do
  putStrLn "in start_playback_cb callback"
  hFlush stdout

stop_playback_cb :: Ptr Sp_Session -> IO ()
stop_playback_cb session_ptr = do
  putStrLn "in stop_playback_cb callback"
  hFlush stdout

get_audio_buffer_stats_cb :: GetAudioBufferStatsCb
get_audio_buffer_stats_cb session_ptr audio_buffer_stats_ptr= do
  putStrLn "in get_audio_buffer_stats_cb callback"
  hFlush stdout

offline_status_updated_cb :: Ptr Sp_Session -> IO ()
offline_status_updated_cb session_ptr = do
  putStrLn "in offline_status_updated_cb callback"
  hFlush stdout

offline_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
offline_error_cb session_ptr err = do
  putStrLn "in offline_error_cb callback"
  hFlush stdout

credentials_blob_updated_cb :: Ptr Sp_Session -> CString -> IO ()
credentials_blob_updated_cb session_ptr log_data = do
  putStrLn "in credentials_blob_updated_cb callback"
  hFlush stdout

connectionstate_updated_cb :: Ptr Sp_Session -> IO ()
connectionstate_updated_cb session_ptr = do
  putStrLn "in connectionstate_updated_cb callback"
  hFlush stdout

scrobble_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
scrobble_error_cb session_ptr err = do
  putStrLn "in scrobble_error_cb callback"
  hFlush stdout

private_session_mode_changed_cb :: Ptr Sp_Session -> Sp_Bool -> IO ()
private_session_mode_changed_cb session_ptr is_private  = do
  putStrLn "in private_session_mode_changed_cb callback"
  hFlush stdout

main :: IO ()
main = do
  key <- BS.readFile $ "spotify_appkey.key"

  process_events_broadcast <- Broadcast.new

  logged_in_cb_ptr                    <- mkLoggedInCb logged_in_cb
  logged_out_cb_ptr                   <- mkLoggedOutCb logged_out_cb
  metadata_updated_cb_ptr             <- mkMetadataUpdatedCb metadata_updated_cb
  connection_error_cb_ptr             <- mkConnectionErrorCb connection_error_cb
  message_to_user_cb_ptr              <- mkMessageToUserCb message_to_user_cb
  notify_main_thread_cb_ptr           <- mkNotifyMainThreadCb $ notify_main_thread_cb process_events_broadcast
  music_delivery_cb_ptr               <- mkMusicDeliveryCb music_delivery_cb
  play_token_lost_cb_ptr              <- mkPlayTokenLostCb play_token_lost_cb
  log_message_cb_ptr                  <- mkLogMessageCb log_message_cb
  end_of_track_cb_ptr                 <- mkEndOfTrackCb end_of_track_cb
  streaming_error_cb_ptr              <- mkStreamingErrorCb streaming_error_cb
  userinfo_updated_cb_ptr             <- mkUserinfoUpdatedCb userinfo_updated_cb
  start_playback_cb_ptr               <- mkStartPlaybackCb start_playback_cb
  stop_playback_cb_ptr                <- mkStopPlaybackCb stop_playback_cb
  get_audio_buffer_stats_cb_ptr       <- mkGetAudioBufferStatsCb get_audio_buffer_stats_cb
  offline_status_updated_cb_ptr       <- mkOfflineStatusUpdatedCb offline_status_updated_cb
  offline_error_cb_ptr                <- mkOfflineErrorCb offline_error_cb
  credentials_blob_updated_cb_ptr     <- mkCredentialsBlobUpdatedCb credentials_blob_updated_cb
  connectionstate_updated_cb_ptr      <- mkConnectionstateUpdatedCb connectionstate_updated_cb
  scrobble_error_cb_ptr               <- mkScrobbleErrorCb scrobble_error_cb
  private_session_mode_changed_cb_ptr <- mkPrivateSessionModeChangedCb private_session_mode_changed_cb

  let session_callbacks = Sp_Session_Callbacks {
      logged_in                    = logged_in_cb_ptr
    , logged_out                   = logged_out_cb_ptr
    , metadata_updated             = metadata_updated_cb_ptr
    , connection_error             = connection_error_cb_ptr
    , message_to_user              = message_to_user_cb_ptr
    , notify_main_thread           = notify_main_thread_cb_ptr
    , music_delivery               = music_delivery_cb_ptr
    , play_token_lost              = play_token_lost_cb_ptr
    , log_message                  = log_message_cb_ptr
    , end_of_track                 = end_of_track_cb_ptr
    , streaming_error              = streaming_error_cb_ptr
    , userinfo_updated             = userinfo_updated_cb_ptr
    , start_playback               = start_playback_cb_ptr
    , stop_playback                = stop_playback_cb_ptr
    , get_audio_buffer_stats       = get_audio_buffer_stats_cb_ptr
    , offline_status_updated       = offline_status_updated_cb_ptr
    , offline_error                = offline_error_cb_ptr
    , credentials_blob_updated     = credentials_blob_updated_cb_ptr
    , connectionstate_updated      = connectionstate_updated_cb_ptr
    , scrobble_error               = scrobble_error_cb_ptr
    , private_session_mode_changed = private_session_mode_changed_cb_ptr
    }

  emptyCString <- newCString ""
  userAgentCString <- newCString "hs-spotify"
  cachelocation <- newCString "/tmp/spotify"
  tracefilepath <- newCString "tracefile"
  key_ptr <- newArray . BS.unpack $ key

  session_callbacks_ptr <- new session_callbacks

  let session_config = Sp_Session_Config {
        api_version                      = spotifyApiVersion
      , cache_location                   = cachelocation
      , settings_location                = cachelocation
      , application_key                  = castPtr key_ptr
      , application_key_size             = fromIntegral . BS.length $ key
      , user_agent                       = userAgentCString
      , callbacks                        = session_callbacks_ptr
      , userdata                         = castPtr userAgentCString
      , compress_playlists               = fromBool False
      , dont_save_metadata_for_playlists = fromBool False
      , initially_unload_playlists       = fromBool False
      , device_id                        = userAgentCString
      , proxy                            = emptyCString
      , proxy_username                   = emptyCString
      , proxy_password                   = emptyCString
      , ca_certs_filename                = emptyCString
      , tracefile                        = tracefilepath
      }

  session_config_ptr <- new session_config

  session_ptr_ptr <- (malloc :: IO (Ptr (Ptr Sp_Session)))

  err <- c_sp_session_create session_config_ptr session_ptr_ptr
  errMsg <- peekCString <=< c_sp_error_message $ err
  putStrLn . show $ errMsg

  session_ptr <- peek session_ptr_ptr

  putStrLn "Enter your username"
  username <- getLine >>= newCString 
  putStrLn "Enter your password"
  password <- getLine >>= newCString

  err <- c_sp_session_login session_ptr username password (fromBool False) nullPtr
  errMsg <- peekCString <=< c_sp_error_message $ err
  putStrLn . show $ errMsg

  delay_process_events process_events_broadcast session_ptr

delay_process_events :: Broadcast () -> Ptr Sp_Session -> IO ()
delay_process_events process_events_broadcast session_ptr = do
  putStrLn "delay_process_events"
  hFlush stdout
  Broadcast.listen process_events_broadcast
  putStrLn "passed"
  hFlush stdout

  process_events process_events_broadcast session_ptr

delay_process_events':: Broadcast () -> Integer -> Ptr Sp_Session -> IO ()
delay_process_events' process_events_broadcast timeout session_ptr = do
  putStrLn "delay_process_events'"
  hFlush stdout
  Broadcast.listenTimeout process_events_broadcast timeout
  putStrLn "passed"
  hFlush stdout

  process_events process_events_broadcast session_ptr

process_events :: Broadcast () -> Ptr Sp_Session -> IO ()
process_events process_events_broadcast session_ptr = do
  next_timeout_ptr <- (malloc :: IO (Ptr CInt))
  err <- c_sp_session_process_events session_ptr next_timeout_ptr

  errMsg <- peekCString <=< c_sp_error_message $ err
  putStrLn . show $ errMsg

  next_timeout_ms <- peek next_timeout_ptr

  let delay_us = (1000 * fromIntegral next_timeout_ms)
  putStrLn $ "delaying " ++ show delay_us ++ "ms"

  Broadcast.silence process_events_broadcast

  delay_process_events' process_events_broadcast delay_us session_ptr
