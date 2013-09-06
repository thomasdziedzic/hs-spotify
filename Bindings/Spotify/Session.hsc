{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bindings.Spotify.Session
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Applicative

import Bindings.Spotify.Error
import Bindings.Spotify.Struct

#include <libspotify/api.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype Version = Version { unVersion :: CInt }
  deriving (Show)

spotifyApiVersion :: Version
spotifyApiVersion = Version #const SPOTIFY_API_VERSION

newtype Sp_ConnectionState = Sp_ConnectionState { unSp_ConnectionState :: CInt }
  deriving (Show)

#{enum Sp_ConnectionState, Sp_ConnectionState
  , sp_connection_state_logged_out   = SP_CONNECTION_STATE_LOGGED_OUT
  , sp_connection_state_logged_in    = SP_CONNECTION_STATE_LOGGED_IN
  , sp_connection_state_disconnected = SP_CONNECTION_STATE_DISCONNECTED
  , sp_connection_state_undefined    = SP_CONNECTION_STATE_UNDEFINED
  , sp_connection_state_offline      = SP_CONNECTION_STATE_OFFLINE
  }

newtype Sp_SampleType = Sp_SampleType { unSampleType :: CInt }
  deriving (Show, Storable)

#{enum Sp_SampleType, Sp_SampleType
  , sp_sampletype_int16_native_endian = SP_SAMPLETYPE_INT16_NATIVE_ENDIAN
  }

data Sp_AudioFormat = Sp_AudioFormat {
    sample_type :: Sp_SampleType
  , sample_rate :: CInt
  , channels    :: CInt
  } deriving (Show)

type Sp_AudioFormatPtr = Ptr Sp_AudioFormat

instance Storable Sp_AudioFormat where
  sizeOf _ = #size sp_audioformat
  alignment _ = #alignment sp_audioformat
  peek ptr =
    Sp_AudioFormat
      <$> (#peek sp_audioformat, sample_type) ptr
      <*> (#peek sp_audioformat, sample_rate) ptr
      <*> (#peek sp_audioformat, channels) ptr
  poke ptr audioFormat = do
    (#poke sp_audioformat, sample_type) ptr (sample_type audioFormat)
    (#poke sp_audioformat, sample_rate) ptr (sample_rate audioFormat)
    (#poke sp_audioformat, channels) ptr (channels audioFormat)

newtype Sp_Bitrate = Sp_Bitrate { unSp_Bitrate :: CInt }
  deriving (Show)

#{enum Sp_Bitrate, Sp_Bitrate
  , sp_bitrate_160k = SP_BITRATE_160k
  , sp_bitrate_320k = SP_BITRATE_320k
  , sp_bitrate_96k  = SP_BITRATE_96k
  }

newtype Sp_Playlist_Type = Sp_Playlist_Type { unSp_Playlist_Type :: CInt }
  deriving (Show)

#{enum Sp_Playlist_Type, Sp_Playlist_Type
  , sp_playlist_type_playlist     = SP_PLAYLIST_TYPE_PLAYLIST
  , sp_playlist_type_start_folder = SP_PLAYLIST_TYPE_START_FOLDER
  , sp_playlist_type_end_folder   = SP_PLAYLIST_TYPE_END_FOLDER
  , sp_playlist_type_placeholder  = SP_PLAYLIST_TYPE_PLACEHOLDER
  }

newtype Sp_Search_Type = Sp_Search_Type { unSp_Search_Type :: CInt }
  deriving (Show)

#{enum Sp_Search_Type, Sp_Search_Type
  , sp_search_standard = SP_SEARCH_STANDARD
  , sp_search_suggest  = SP_SEARCH_SUGGEST
  }

newtype Sp_Playlist_Offline_Status = Sp_Playlist_Offline_Status { unSp_Playlist_Offline_Status :: CInt }
  deriving (Show)

#{enum Sp_Playlist_Offline_Status, Sp_Playlist_Offline_Status
  , sp_playlist_offline_status_no          = SP_PLAYLIST_OFFLINE_STATUS_NO
  , sp_playlist_offline_status_yes         = SP_PLAYLIST_OFFLINE_STATUS_YES
  , sp_playlist_offline_status_downloading = SP_PLAYLIST_OFFLINE_STATUS_DOWNLOADING
  , sp_playlist_offline_status_waiting     = SP_PLAYLIST_OFFLINE_STATUS_WAITING
  }

newtype Sp_Track_Availability = Sp_Track_Availability { unSp_Availability :: CInt }
  deriving (Show)

#{enum Sp_Track_Availability, Sp_Track_Availability
  , sp_track_availability_unavailable      = SP_TRACK_AVAILABILITY_UNAVAILABLE
  , sp_track_availability_available        = SP_TRACK_AVAILABILITY_AVAILABLE
  , sp_track_availability_not_streamable   = SP_TRACK_AVAILABILITY_NOT_STREAMABLE
  , sp_track_availability_banned_by_artist = SP_TRACK_AVAILABILITY_BANNED_BY_ARTIST
  }

newtype Sp_Track_Offline_Status = Sp_Track_Offline_Status { unSp_Track_Offline_Status :: CInt }
  deriving (Show)

#{enum Sp_Track_Offline_Status, Sp_Track_Offline_Status
  , sp_track_offline_no             = SP_TRACK_OFFLINE_NO
  , sp_track_offline_waiting        = SP_TRACK_OFFLINE_WAITING
  , sp_track_offline_downloading    = SP_TRACK_OFFLINE_DOWNLOADING
  , sp_track_offline_done           = SP_TRACK_OFFLINE_DONE
  , sp_track_offline_error          = SP_TRACK_OFFLINE_ERROR
  , sp_track_offline_done_expired   = SP_TRACK_OFFLINE_DONE_EXPIRED
  , sp_track_offline_limit_exceeded = SP_TRACK_OFFLINE_LIMIT_EXCEEDED
  , sp_track_offline_done_resync    = SP_TRACK_OFFLINE_DONE_RESYNC
  }

newtype Sp_Image_Size = Sp_Image_Size { unSp_Image_Size :: CInt }
  deriving (Show)

#{enum Sp_Image_Size, Sp_Image_Size
  , sp_image_size_normal = SP_IMAGE_SIZE_NORMAL
  , sp_image_size_small  = SP_IMAGE_SIZE_SMALL
  , sp_image_size_large  = SP_IMAGE_SIZE_LARGE
  }

data Sp_Audio_Buffer_Stats = Sp_Audio_Buffer_Stats {
    samples :: CInt
  , stutter :: CInt
  } deriving (Show)

type Sp_Audio_Buffer_StatsPtr = Ptr Sp_Audio_Buffer_Stats

instance Storable Sp_Audio_Buffer_Stats where
  sizeOf _ = #size sp_audio_buffer_stats
  alignment _ = #alignment sp_audio_buffer_stats
  peek ptr =
    Sp_Audio_Buffer_Stats
      <$> (#peek sp_audio_buffer_stats, samples) ptr
      <*> (#peek sp_audio_buffer_stats, stutter) ptr
  poke ptr audioBufferStats = do
    (#poke sp_audio_buffer_stats, samples) ptr (samples audioBufferStats)
    (#poke sp_audio_buffer_stats, stutter) ptr (stutter audioBufferStats)

data Sp_Subscribers = Sp_Subscribers {
    count       :: CUInt
  , subscribers :: Ptr CString
  } deriving (Show)

instance Storable Sp_Subscribers where
  sizeOf _ = #size sp_subscribers
  alignment _ = #alignment sp_subscribers
  peek ptr =
    Sp_Subscribers
      <$> (#peek sp_subscribers, count) ptr
      <*> (#peek sp_subscribers, subscribers) ptr
  poke ptr spSubscribers = do
    (#poke sp_subscribers, count) ptr (count spSubscribers)
    (#poke sp_subscribers, subscribers) ptr (subscribers spSubscribers)

newtype Sp_Connection_Type = Sp_Connection_Type { unSp_Connection_Type :: CInt }
  deriving (Show)

#{enum Sp_Connection_Type, Sp_Connection_Type
  , sp_connection_type_unknown        = SP_CONNECTION_TYPE_UNKNOWN
  , sp_connection_type_none           = SP_CONNECTION_TYPE_NONE
  , sp_connection_type_mobile         = SP_CONNECTION_TYPE_MOBILE
  , sp_connection_type_mobile_roaming = SP_CONNECTION_TYPE_MOBILE_ROAMING
  , sp_connection_type_wifi           = SP_CONNECTION_TYPE_WIFI
  , sp_connection_type_wired          = SP_CONNECTION_TYPE_WIRED
  }

-- TODO connection rules can be combined with a bitwise or operation
--      need to write a function that does this
newtype Sp_Connection_Rules = Sp_Connection_Rules { unSp_Connection_Rules :: CInt }
  deriving (Show)

#{enum Sp_Connection_Rules, Sp_Connection_Rules
  , sp_connection_rule_network                = SP_CONNECTION_RULE_NETWORK
  , sp_connection_rule_network_if_roaming     = SP_CONNECTION_RULE_NETWORK_IF_ROAMING
  , sp_connection_rule_allow_sync_over_mobile = SP_CONNECTION_RULE_ALLOW_SYNC_OVER_MOBILE
  , sp_connection_rule_allow_sync_over_wifi   = SP_CONNECTION_RULE_ALLOW_SYNC_OVER_WIFI
  }

newtype Sp_ArtistBrowse_Type = Sp_ArtistBrowse_Type { unSp_ArtistBrowse_Type :: CInt }
  deriving (Show)

#{enum Sp_ArtistBrowse_Type, Sp_ArtistBrowse_Type
  , sp_artistbrowse_full      = SP_ARTISTBROWSE_FULL
  , sp_artistbrowse_no_tracks = SP_ARTISTBROWSE_NO_TRACKS
  , sp_artistbrowse_no_albums = SP_ARTISTBROWSE_NO_ALBUMS
  }

newtype Sp_Social_Provider = Sp_Social_Provider { unSp_Social_Provider :: CInt }
  deriving (Show)

#{enum Sp_Social_Provider, Sp_Social_Provider
  , sp_social_provider_spotify  = SP_SOCIAL_PROVIDER_SPOTIFY
  , sp_social_provider_facebook = SP_SOCIAL_PROVIDER_FACEBOOK
  , sp_social_provider_lastfm   = SP_SOCIAL_PROVIDER_LASTFM
  }

newtype Sp_Scrobbling_State = Sp_Scrobbling_State { unSp_Scrobbling_State :: CInt }
  deriving (Show)

#{enum Sp_Scrobbling_State, Sp_Scrobbling_State
  , sp_scrobbling_state_use_global_setting = SP_SCROBBLING_STATE_USE_GLOBAL_SETTING
  , sp_scrobbling_state_local_enabled      = SP_SCROBBLING_STATE_LOCAL_ENABLED
  , sp_scrobbling_state_local_disabled     = SP_SCROBBLING_STATE_LOCAL_DISABLED
  , sp_scrobbling_state_global_enabled     = SP_SCROBBLING_STATE_GLOBAL_ENABLED
  , sp_scrobbling_state_global_disabled    = SP_SCROBBLING_STATE_GLOBAL_DISABLED
  }

data Sp_Offline_Sync_Status = Sp_Offline_Sync_Status {
    queued_tracks      :: CInt
  , queued_bytes       :: CUIntMax
  , done_tracks        :: CInt
  , done_bytes         :: CUIntMax
  , copied_tracks      :: CInt
  , copied_bytes       :: CUIntMax
  , willnotcopy_tracks :: CInt
  , error_tracks       :: CInt
  , syncing            :: CUChar
  }

instance Storable Sp_Offline_Sync_Status where
  sizeOf _ = #size sp_offline_sync_status
  alignment _ = #alignment sp_offline_sync_status
  peek ptr =
    Sp_Offline_Sync_Status
      <$> (#peek sp_offline_sync_status, queued_tracks) ptr
      <*> (#peek sp_offline_sync_status, queued_bytes) ptr
      <*> (#peek sp_offline_sync_status, done_tracks) ptr
      <*> (#peek sp_offline_sync_status, done_bytes) ptr
      <*> (#peek sp_offline_sync_status, copied_tracks) ptr
      <*> (#peek sp_offline_sync_status, copied_bytes) ptr
      <*> (#peek sp_offline_sync_status, willnotcopy_tracks) ptr
      <*> (#peek sp_offline_sync_status, error_tracks) ptr
      <*> (#peek sp_offline_sync_status, syncing) ptr
  poke ptr offlineSyncStatus = do
    (#poke sp_offline_sync_status, queued_tracks) ptr (queued_tracks offlineSyncStatus)
    (#poke sp_offline_sync_status, queued_bytes) ptr (queued_bytes offlineSyncStatus)
    (#poke sp_offline_sync_status, done_tracks) ptr (done_tracks offlineSyncStatus)
    (#poke sp_offline_sync_status, done_bytes) ptr (done_bytes offlineSyncStatus)
    (#poke sp_offline_sync_status, copied_tracks) ptr (copied_tracks offlineSyncStatus)
    (#poke sp_offline_sync_status, copied_bytes) ptr (copied_bytes offlineSyncStatus)
    (#poke sp_offline_sync_status, willnotcopy_tracks) ptr (willnotcopy_tracks offlineSyncStatus)
    (#poke sp_offline_sync_status, error_tracks) ptr (error_tracks offlineSyncStatus)
    (#poke sp_offline_sync_status, syncing) ptr (syncing offlineSyncStatus)

data Sp_Session_Callbacks = Sp_Session_Callbacks
  { logged_in                    :: FunPtr (Ptr Sp_Session -> Sp_Error -> IO ())
  , logged_out                   :: FunPtr (Ptr Sp_Session -> IO ())
  , metadata_updated             :: FunPtr (Ptr Sp_Session -> IO ())
  , connection_error             :: FunPtr (Ptr Sp_Session -> Sp_Error -> IO ())
  , message_to_user              :: FunPtr (Ptr Sp_Session -> CString -> IO ())
  , notify_main_thread           :: FunPtr (Ptr Sp_Session -> IO ())
  , music_delivery               :: FunPtr (Ptr Sp_Session -> Ptr Sp_AudioFormat -> Ptr () -> CInt -> IO CInt)
  , play_token_lost              :: FunPtr (Ptr Sp_Session -> IO ())
  , log_message                  :: FunPtr (Ptr Sp_Session -> CString -> IO ())
  , end_of_track                 :: FunPtr (Ptr Sp_Session -> IO ())
  , streaming_error              :: FunPtr (Ptr Sp_Session -> Sp_Error -> IO ())
  , userinfo_updated             :: FunPtr (Ptr Sp_Session -> IO ())
  , start_playback               :: FunPtr (Ptr Sp_Session -> IO ())
  , stop_playback                :: FunPtr (Ptr Sp_Session -> IO ())
  , get_audio_buffer_stats       :: FunPtr (Ptr Sp_Session -> Ptr Sp_Audio_Buffer_Stats -> IO ())
  , offline_status_updated       :: FunPtr (Ptr Sp_Session -> IO ())
  , offline_error                :: FunPtr (Ptr Sp_Session -> Sp_Error -> IO ())
  , credentials_blob_updated     :: FunPtr (Ptr Sp_Session -> CString -> IO ())
  , connectionstate_updated      :: FunPtr (Ptr Sp_Session -> IO ())
  , scrobble_error               :: FunPtr (Ptr Sp_Session -> Sp_Error -> IO ())
  , private_session_mode_changed :: FunPtr (Ptr Sp_Session -> CUChar -> IO ())
  }

instance Storable Sp_Session_Callbacks where
  sizeOf _ = #size sp_session_callbacks
  alignment _ = #alignment sp_session_callbacks
  peek ptr =
    Sp_Session_Callbacks
      <$> (#peek sp_session_callbacks, logged_in) ptr
      <*> (#peek sp_session_callbacks, logged_out) ptr
      <*> (#peek sp_session_callbacks, metadata_updated) ptr
      <*> (#peek sp_session_callbacks, connection_error) ptr
      <*> (#peek sp_session_callbacks, message_to_user) ptr
      <*> (#peek sp_session_callbacks, notify_main_thread) ptr
      <*> (#peek sp_session_callbacks, music_delivery) ptr
      <*> (#peek sp_session_callbacks, play_token_lost) ptr
      <*> (#peek sp_session_callbacks, log_message) ptr
      <*> (#peek sp_session_callbacks, end_of_track) ptr
      <*> (#peek sp_session_callbacks, streaming_error) ptr
      <*> (#peek sp_session_callbacks, userinfo_updated) ptr
      <*> (#peek sp_session_callbacks, start_playback) ptr
      <*> (#peek sp_session_callbacks, stop_playback) ptr
      <*> (#peek sp_session_callbacks, get_audio_buffer_stats) ptr
      <*> (#peek sp_session_callbacks, offline_status_updated) ptr
      <*> (#peek sp_session_callbacks, offline_error) ptr
      <*> (#peek sp_session_callbacks, credentials_blob_updated) ptr
      <*> (#peek sp_session_callbacks, connectionstate_updated) ptr
      <*> (#peek sp_session_callbacks, scrobble_error) ptr
      <*> (#peek sp_session_callbacks, private_session_mode_changed) ptr
  poke ptr sessionCallbacks = do
    (#poke sp_session_callbacks, logged_in) ptr (logged_in sessionCallbacks)
    (#poke sp_session_callbacks, logged_out) ptr (logged_out sessionCallbacks)
    (#poke sp_session_callbacks, metadata_updated) ptr (metadata_updated sessionCallbacks)
    (#poke sp_session_callbacks, connection_error) ptr (connection_error sessionCallbacks)
    (#poke sp_session_callbacks, message_to_user) ptr (message_to_user sessionCallbacks)
    (#poke sp_session_callbacks, notify_main_thread) ptr (notify_main_thread sessionCallbacks)
    (#poke sp_session_callbacks, music_delivery) ptr (music_delivery sessionCallbacks)
    (#poke sp_session_callbacks, play_token_lost) ptr (play_token_lost sessionCallbacks)
    (#poke sp_session_callbacks, log_message) ptr (log_message sessionCallbacks)
    (#poke sp_session_callbacks, end_of_track) ptr (end_of_track sessionCallbacks)
    (#poke sp_session_callbacks, streaming_error) ptr (streaming_error sessionCallbacks)
    (#poke sp_session_callbacks, userinfo_updated) ptr (userinfo_updated sessionCallbacks)
    (#poke sp_session_callbacks, start_playback) ptr (start_playback sessionCallbacks)
    (#poke sp_session_callbacks, stop_playback) ptr (stop_playback sessionCallbacks)
    (#poke sp_session_callbacks, get_audio_buffer_stats) ptr (get_audio_buffer_stats sessionCallbacks)
    (#poke sp_session_callbacks, offline_status_updated) ptr (offline_status_updated sessionCallbacks)
    (#poke sp_session_callbacks, offline_error) ptr (offline_error sessionCallbacks)
    (#poke sp_session_callbacks, credentials_blob_updated) ptr (credentials_blob_updated sessionCallbacks)
    (#poke sp_session_callbacks, connectionstate_updated) ptr (connectionstate_updated sessionCallbacks)
    (#poke sp_session_callbacks, scrobble_error) ptr (scrobble_error sessionCallbacks)
    (#poke sp_session_callbacks, private_session_mode_changed) ptr (private_session_mode_changed sessionCallbacks)

data Sp_Session_Config = Sp_Session_Config
  { api_version                      :: CInt
  , cache_location                   :: CString
  , settings_location                :: CString
  , application_key                  :: Ptr ()
  , application_key_size             :: CSize
  , user_agent                       :: CString
  , callbacks                        :: Ptr Sp_Session_Callbacks
  , userdata                         :: Ptr ()
  , compress_playlists               :: CUChar
  , dont_save_metadata_for_playlists :: CUChar
  , initially_unload_playlists       :: CUChar
  , device_id                        :: CString
  , proxy                            :: CString
  , proxy_username                   :: CString
  , proxy_password                   :: CString
  , ca_certs_filename                :: CString
  , tracefile                        :: CString
  }

instance Storable Sp_Session_Config where
  sizeOf _ = #size sp_session_config
  alignment _ = #alignment sp_session_config
  peek ptr =
    Sp_Session_Config
      <$> (#peek sp_session_config, api_version) ptr
      <*> (#peek sp_session_config, cache_location) ptr
      <*> (#peek sp_session_config, settings_location) ptr
      <*> (#peek sp_session_config, application_key) ptr
      <*> (#peek sp_session_config, application_key_size) ptr
      <*> (#peek sp_session_config, user_agent) ptr
      <*> (#peek sp_session_config, callbacks) ptr
      <*> (#peek sp_session_config, userdata) ptr
      <*> (#peek sp_session_config, compress_playlists) ptr
      <*> (#peek sp_session_config, dont_save_metadata_for_playlists) ptr
      <*> (#peek sp_session_config, initially_unload_playlists) ptr
      <*> (#peek sp_session_config, device_id) ptr
      <*> (#peek sp_session_config, proxy) ptr
      <*> (#peek sp_session_config, proxy_username) ptr
      <*> (#peek sp_session_config, proxy_password) ptr
      <*> (#peek sp_session_config, ca_certs_filename) ptr
      <*> (#peek sp_session_config, tracefile) ptr
  poke ptr sessionConfig = do
    (#poke sp_session_config, api_version) ptr (api_version sessionConfig)
    (#poke sp_session_config, cache_location) ptr (cache_location sessionConfig)
    (#poke sp_session_config, settings_location) ptr (settings_location sessionConfig)
    (#poke sp_session_config, application_key) ptr (application_key sessionConfig)
    (#poke sp_session_config, application_key_size) ptr (application_key_size sessionConfig)
    (#poke sp_session_config, user_agent) ptr (user_agent sessionConfig)
    (#poke sp_session_config, callbacks) ptr (callbacks sessionConfig)
    (#poke sp_session_config, userdata) ptr (userdata sessionConfig)
    (#poke sp_session_config, compress_playlists) ptr (compress_playlists sessionConfig)
    (#poke sp_session_config, dont_save_metadata_for_playlists) ptr (dont_save_metadata_for_playlists sessionConfig)
    (#poke sp_session_config, initially_unload_playlists) ptr (initially_unload_playlists sessionConfig)
    (#poke sp_session_config, device_id) ptr (device_id sessionConfig)
    (#poke sp_session_config, proxy) ptr (proxy sessionConfig)
    (#poke sp_session_config, proxy_username) ptr (proxy_username sessionConfig)
    (#poke sp_session_config, proxy_password) ptr (proxy_password sessionConfig)
    (#poke sp_session_config, ca_certs_filename) ptr (ca_certs_filename sessionConfig)
    (#poke sp_session_config, tracefile) ptr (tracefile sessionConfig)

foreign import ccall "libspotify/api.h sp_session_create"
  c_sp_session_create :: Ptr Sp_Session_Config -> Ptr (Ptr Sp_Session) -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_release"
  c_sp_session_release :: Ptr (Ptr Sp_Session) -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_login"
  c_sp_session_login :: Ptr Sp_Session -> CString -> CString -> CUChar -> CString -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_relogin"
  c_sp_session_relogin :: Ptr Sp_Session -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_remembered_user"
  c_sp_session_remembered_user :: Ptr Sp_Session -> CString -> CSize -> IO CInt

foreign import ccall "libspotify/api.h sp_session_user_name"
  c_sp_session_user_name :: Ptr Sp_Session -> IO CString

foreign import ccall "libspotify/api.h sp_session_forget_me"
  c_sp_session_forget_me :: Ptr Sp_Session -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_user"
  c_sp_session_user :: Ptr Sp_Session -> IO (Ptr Sp_User)

foreign import ccall "libspotify/api.h sp_session_logout"
  c_sp_session_logout :: Ptr Sp_Session -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_flush_caches"
  c_sp_session_flush_caches :: Ptr Sp_Session -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_connectionstate"
  c_sp_session_connectionstate :: Ptr Sp_Session -> IO Sp_ConnectionState

foreign import ccall "libspotify/api.h sp_session_userdata"
  c_sp_session_userdata :: Ptr Sp_Session -> IO (Ptr ())

foreign import ccall "libspotify/api.h sp_session_set_cache_size"
  c_sp_session_set_cache_size :: Ptr Sp_Session -> CSize -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_process_events"
  c_sp_session_process_events :: Ptr Sp_Session -> Ptr CInt -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_player_load"
  c_sp_session_player_load :: Ptr Sp_Session -> Ptr Sp_Track -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_player_seek"
  c_sp_session_player_seek :: Ptr Sp_Session -> CInt -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_player_play"
  c_sp_session_player_play :: Ptr Sp_Session -> CUChar -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_player_unload"
  c_sp_session_player_unload :: Ptr Sp_Session -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_player_prefetch"
  c_sp_session_player_prefetch :: Ptr Sp_Session -> Ptr Sp_Track -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_playlistcontainer"
  c_sp_session_playlistcontainer :: Ptr Sp_Session -> IO (Ptr Sp_Playlistcontainer)

foreign import ccall "libspotify/api.h sp_session_inbox_create"
  c_sp_session_inbox_create :: Ptr Sp_Session -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_session_starred_create"
  c_sp_session_starred_create :: Ptr Sp_Session -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_session_starred_for_user_create"
  c_sp_session_starred_for_user_create :: Ptr Sp_Session -> CString -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_session_publishedcontainer_for_user_create"
  c_sp_session_publishedcontainer_for_user_create :: Ptr Sp_Session -> CString -> IO (Ptr Sp_Playlistcontainer)

foreign import ccall "libspotify/api.h sp_session_preferred_bitrate"
  c_sp_session_preferred_bitrate :: Ptr Sp_Session -> Sp_Bitrate -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_preferred_offline_bitrate"
  c_sp_session_preferred_offline_bitrate :: Ptr Sp_Session -> Sp_Bitrate -> CUChar -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_get_volume_normalization"
  c_sp_session_get_volume_normalization :: Ptr Sp_Session -> IO CUChar

foreign import ccall "libspotify/api.h sp_session_set_volume_normalization"
  c_sp_session_set_volume_normalization :: Ptr Sp_Session -> CUChar -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_set_private_session"
  c_sp_session_set_private_session :: Ptr Sp_Session -> CUChar -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_is_private_session"
  c_sp_session_is_private_session :: Ptr Sp_Session -> IO CUChar

foreign import ccall "libspotify/api.h sp_session_set_scrobbling"
  c_sp_session_set_scrobbling :: Ptr Sp_Session -> Sp_Social_Provider -> Sp_Scrobbling_State -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_is_scrobbling"
  c_sp_session_is_scrobbling :: Ptr Sp_Session -> Sp_Social_Provider -> Ptr Sp_Scrobbling_State -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_is_scrobbling_possible"
  c_sp_session_is_scrobbling_possible :: Ptr Sp_Session -> Sp_Social_Provider -> Ptr CUChar -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_set_social_credentials"
  c_sp_session_set_social_credentials :: Ptr Sp_Session -> Sp_Social_Provider -> CString -> CString -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_set_connection_type"
  c_sp_session_set_connection_type :: Ptr Sp_Session -> Sp_Connection_Type -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_session_set_connection_rules"
  c_sp_session_set_connection_rules :: Ptr Sp_Session -> Sp_Connection_Rules -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_offline_tracks_to_sync"
  c_sp_offline_tracks_to_sync :: Ptr Sp_Session -> IO CInt

foreign import ccall "libspotify/api.h sp_offline_num_playlists"
  c_sp_offline_num_playlists :: Ptr Sp_Session -> IO CInt

foreign import ccall "libspotify/api.h sp_offline_sync_get_status"
  c_sp_offline_sync_get_status :: Ptr Sp_Session -> Ptr Sp_Offline_Sync_Status -> IO CUChar

foreign import ccall "libspotify/api.h sp_offline_time_left"
  c_sp_offline_time_left :: Ptr Sp_Session -> IO CInt

foreign import ccall "libspotify/api.h sp_session_user_country"
  c_sp_session_user_country :: Ptr Sp_Session -> IO CInt
