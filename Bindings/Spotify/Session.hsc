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
