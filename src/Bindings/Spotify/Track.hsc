module Bindings.Spotify.Track (
      c_sp_track_is_loaded
    , c_sp_track_error
    , c_sp_track_offline_get_status
    , c_sp_track_get_availability
    , c_sp_track_is_local
    , c_sp_track_is_autolinked
    , c_sp_track_get_playable
    , c_sp_track_is_placeholder
    , c_sp_track_is_starred
    , c_sp_track_set_starred
    , c_sp_track_num_artists
    , c_sp_track_artist
    , c_sp_track_album
    , c_sp_track_name
    , c_sp_track_duration
    , c_sp_track_popularity
    , c_sp_track_disc
    , c_sp_track_index
    , c_sp_localtrack_create
    , c_sp_track_add_ref
    , c_sp_track_release
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Struct
import Bindings.Spotify.Session
import Bindings.Spotify.Error

foreign import ccall "libspotify/api.h sp_track_is_loaded"
  c_sp_track_is_loaded :: Ptr Sp_Track -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_track_error"
  c_sp_track_error :: Ptr Sp_Track -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_track_offline_get_status"
  c_sp_track_offline_get_status :: Ptr Sp_Track -> IO Sp_Track_Offline_Status

foreign import ccall "libspotify/api.h sp_track_get_availability"
  c_sp_track_get_availability :: Ptr Sp_Session -> Ptr Sp_Track -> IO Sp_Track_Availability

foreign import ccall "libspotify/api.h sp_track_is_local"
  c_sp_track_is_local :: Ptr Sp_Session -> Ptr Sp_Track -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_track_is_autolinked"
  c_sp_track_is_autolinked :: Ptr Sp_Session -> Ptr Sp_Track -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_track_get_playable"
  c_sp_track_get_playable :: Ptr Sp_Session -> Ptr Sp_Track -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_track_is_placeholder"
  c_sp_track_is_placeholder :: Ptr Sp_Track -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_track_is_starred"
  c_sp_track_is_starred :: Ptr Sp_Session -> Ptr Sp_Track -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_track_set_starred"
  c_sp_track_set_starred :: Ptr Sp_Session -> Ptr (Ptr Sp_Track) -> CInt -> Sp_Bool -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_track_num_artists"
  c_sp_track_num_artists :: Ptr Sp_Track -> IO CInt

foreign import ccall "libspotify/api.h sp_track_artist"
  c_sp_track_artist :: Ptr Sp_Track -> CInt -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_track_album"
  c_sp_track_album :: Ptr Sp_Track -> IO (Ptr Sp_Album)

foreign import ccall "libspotify/api.h sp_track_name"
  c_sp_track_name :: Ptr Sp_Track -> IO CString

foreign import ccall "libspotify/api.h sp_track_duration"
  c_sp_track_duration :: Ptr Sp_Track -> IO CInt

foreign import ccall "libspotify/api.h sp_track_popularity"
  c_sp_track_popularity :: Ptr Sp_Track -> IO CInt

foreign import ccall "libspotify/api.h sp_track_disc"
  c_sp_track_disc :: Ptr Sp_Track -> IO CInt

foreign import ccall "libspotify/api.h sp_track_index"
  c_sp_track_index :: Ptr Sp_Track -> IO CInt

foreign import ccall "libspotify/api.h sp_localtrack_create"
  c_sp_localtrack_create :: CString -> CString -> CString -> CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_track_add_ref"
  c_sp_track_add_ref :: Ptr Sp_Track -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_track_release"
  c_sp_track_release :: Ptr Sp_Track -> IO Sp_Error
