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

logged_in_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
logged_in_cb session_ptr err = error "in logged_in_cb"

foreign import ccall "wrapper"
  mkLoggedInCb :: (Ptr Sp_Session -> Sp_Error -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> Sp_Error -> IO ()))

logged_out_cb :: Ptr Sp_Session -> IO ()
logged_out_cb session_ptr = error "in logged_out_cb"

foreign import ccall "wrapper"
  mkLoggedOutCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

metadata_updated_cb :: Ptr Sp_Session -> IO ()
metadata_updated_cb session_ptr = error "in metadata_updated_cb"

foreign import ccall "wrapper"
  mkMetadataUpdatedCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

connection_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
connection_error_cb session_ptr err = do
  putStrLn "in connection_error callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkConnectionErrorCb :: (Ptr Sp_Session -> Sp_Error -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> Sp_Error -> IO ()))

message_to_user_cb :: Ptr Sp_Session -> CString -> IO ()
message_to_user_cb session_ptr msg = do
  putStrLn "in message_to_user callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkMessageToUserCb :: (Ptr Sp_Session -> CString -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> CString -> IO ()))

notify_main_thread_cb :: Ptr Sp_Session -> IO ()
notify_main_thread_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkNotifyMainThreadCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

type MusicDeliveryCb = Ptr Sp_Session -> Ptr Sp_AudioFormat -> Ptr () -> CInt -> IO CInt

music_delivery_cb :: MusicDeliveryCb
music_delivery_cb session_ptr audioformat_ptr frames_ptr num_frames = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout
  return 1

foreign import ccall "wrapper"
  mkMusicDeliveryCb :: MusicDeliveryCb -> IO (FunPtr MusicDeliveryCb)

play_token_lost_cb :: Ptr Sp_Session -> IO ()
play_token_lost_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkPlayTokenLostCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

log_message_cb :: Ptr Sp_Session -> CString -> IO ()
log_message_cb session_ptr log_data = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkLogMessageCb :: (Ptr Sp_Session -> CString -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> CString -> IO ()))

end_of_track_cb :: Ptr Sp_Session -> IO ()
end_of_track_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkEndOfTrackCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

streaming_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
streaming_error_cb session_ptr err = do
  putStrLn "in connection_error callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkStreamingErrorCb :: (Ptr Sp_Session -> Sp_Error -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> Sp_Error -> IO ()))

userinfo_updated_cb :: Ptr Sp_Session -> IO ()
userinfo_updated_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkUserinfoUpdatedCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

start_playback_cb :: Ptr Sp_Session -> IO ()
start_playback_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkStartPlaybackCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

stop_playback_cb :: Ptr Sp_Session -> IO ()
stop_playback_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkStopPlaybackCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

type GetAudioBufferStatsCb = Ptr Sp_Session -> Ptr Sp_Audio_Buffer_Stats -> IO ()

get_audio_buffer_stats_cb :: GetAudioBufferStatsCb
get_audio_buffer_stats_cb session_ptr audio_buffer_stats_ptr= do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkGetAudioBufferStatsCb :: GetAudioBufferStatsCb -> IO (FunPtr GetAudioBufferStatsCb)

offline_status_updated_cb :: Ptr Sp_Session -> IO ()
offline_status_updated_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkOfflineStatusUpdatedCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

offline_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
offline_error_cb session_ptr err = do
  putStrLn "in connection_error callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkOfflineErrorCb :: (Ptr Sp_Session -> Sp_Error -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> Sp_Error -> IO ()))

credentials_blob_updated_cb :: Ptr Sp_Session -> CString -> IO ()
credentials_blob_updated_cb session_ptr log_data = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkCredentialsBlobUpdatedCb :: (Ptr Sp_Session -> CString -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> CString -> IO ()))

connectionstate_updated_cb :: Ptr Sp_Session -> IO ()
connectionstate_updated_cb session_ptr = do
  putStrLn "in notify_main_thread callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkConnectionstateUpdatedCb :: (Ptr Sp_Session -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> IO ()))

scrobble_error_cb :: Ptr Sp_Session -> Sp_Error -> IO ()
scrobble_error_cb session_ptr err = do
  putStrLn "in connection_error callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkScrobbleErrorCb :: (Ptr Sp_Session -> Sp_Error -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> Sp_Error -> IO ()))

private_session_mode_changed_cb :: Ptr Sp_Session -> Sp_Bool -> IO ()
private_session_mode_changed_cb session_ptr is_private  = do
  putStrLn "in connection_error callback"
  hFlush stdout

foreign import ccall "wrapper"
  mkPrivateSessionModeChangedCb :: (Ptr Sp_Session -> Sp_Bool -> IO ()) -> IO (FunPtr (Ptr Sp_Session -> Sp_Bool -> IO ()))

main :: IO ()
main = do
  key <- BS.readFile $ "spotify_appkey.key"
  putStrLn "Key:"
  putStrLn . show $ key
  putStrLn "Key Length:"
  putStrLn . show . BS.length $ key

  logged_in_cb_ptr                    <- mkLoggedInCb logged_in_cb
  logged_out_cb_ptr                   <- mkLoggedOutCb logged_out_cb
  metadata_updated_cb_ptr             <- mkMetadataUpdatedCb metadata_updated_cb
  connection_error_cb_ptr             <- mkConnectionErrorCb connection_error_cb
  message_to_user_cb_ptr              <- mkMessageToUserCb message_to_user_cb
  notify_main_thread_cb_ptr           <- mkNotifyMainThreadCb notify_main_thread_cb
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
  putStrLn "session_callbacks_ptr"
  putStrLn . show $ session_callbacks_ptr

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

  putStrLn . show $ session_callbacks
  putStrLn . show $ session_config

  session_config_ptr <- new session_config
  putStrLn "session_config_ptr:"
  putStrLn . show $ session_config_ptr

  session_ptr_ptr <- (malloc :: IO (Ptr (Ptr Sp_Session)))
  putStrLn "session_ptr_ptr:"
  putStrLn . show $ session_ptr_ptr

  err <- c_sp_session_create session_config_ptr session_ptr_ptr
  errMsg <- peekCString <=< c_sp_error_message $ err
  putStrLn . show $ errMsg

  threadDelay 10000000
