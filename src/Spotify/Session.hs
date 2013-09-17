module Spotify.Session (
      Version(..)
    , SampleType(..)
    , AudioFormat(..)
    , AudioBufferStats(..)
    , SessionCallbacks(..)
    , SessionConfig(..)
--    , sessionCreate
) where

import Bindings.Spotify.Session

import Spotify.Struct
import Spotify.Error

import Data.Int
import qualified Data.ByteString.Lazy as B

newtype Version = Version Int
    deriving (Show)

data SampleType = Int16NativeEndian
    deriving (Show)

data AudioFormat = AudioFormat
    { sampleType :: SampleType
    , sampleRate :: Int
    , channels   :: Int
    } deriving (Show)

data AudioBufferStats = AudioBufferStats
    { samples :: Int
    , stutter :: Int
    } deriving (Show)

data SessionCallbacks = SessionCallbacks
    { loggedIn                  :: Session -> Error -> IO ()
    , loggedOut                 :: Session -> IO ()
    , metadataUpdated           :: Session -> IO ()
    , connectionError           :: Session -> Error -> IO ()
    , messageToUser             :: Session -> String -> IO ()
    , notifyMainThread          :: Session -> IO ()
    , musicDelivery             :: Session -> AudioFormat -> B.ByteString -> Int -> IO Int
    , playTokenLost             :: Session -> IO ()
    , logMessage                :: Session -> String -> IO ()
    , endOfTrack                :: Session -> IO ()
    , streamingError            :: Session -> Error -> IO ()
    , userinfoUpdated           :: Session -> IO ()
    , startPlayback             :: Session -> IO ()
    , stopPlayback              :: Session -> IO ()
    , getAudioBufferStats       :: Session -> AudioBufferStats -> IO ()
    , offlineStatusUpdated      :: Session -> IO ()
    , offlineError              :: Session -> Error -> IO ()
    , credentialsBlobUpdated    :: Session -> String -> IO ()
    , connectionstateUpdated    :: Session -> IO ()
    , scrobbleError             :: Session -> Error -> IO ()
    , privateSessionModeChanged :: Session -> Bool -> IO ()
    }

data SessionConfig = SessionConfig
    { apiVersion                   :: Version
    , cacheLocation                :: FilePath
    , settingsLocation             :: FilePath
    , applicationKey               :: B.ByteString
    , applicationKeySize           :: Int64
    , userAgent                    :: String
    , callbacks                    :: SessionCallbacks
    , userdata                     :: B.ByteString
    , compressPlaylists            :: Bool
    , dontSaveMetadataForPlaylists :: Bool
    , initiallyUnloadPlaylists     :: Bool
    , deviceId                     :: String
    , proxy                        :: String
    , proxyUsername                :: String
    , proxyPassword                :: String
    , caCertsFilename              :: String
    , tracefile                    :: FilePath
    }

{-
hs2cCallbacks :: SessionCallbacks -> IO (Ptr Sp_Session_Callbacks)
hs2cCallbacks callbacks = return nullPtr
  where
    cLoggedInCb sessionPtr err = (loggedIn callbacks) (Session sessionPtr) (wrapError err)

sessionCreate :: SessionConfig -> IO (Either Error Session)
sessionCreate config = do
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
    key_ptr <- newArray . B.unpack $ key

    session_callbacks_ptr <- new session_callbacks

    let session_config = Sp_Session_Config {
          api_version                      = spotifyApiVersion
        , cache_location                   = cachelocation
        , settings_location                = cachelocation
        , application_key                  = castPtr key_ptr
        , application_key_size             = fromIntegral . B.length $ key
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

    c_sp_session_create 
-}
