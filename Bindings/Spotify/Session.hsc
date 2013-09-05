{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bindings.Spotify.Session (
    Version(..)
  , spotifyApiVersion
)where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Applicative

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

newtype Sp_Availability = Sp_Availability { unSp_Availability :: CInt }
  deriving (Show)

#{enum Sp_Availability, Sp_Availability
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
