module Bindings.Spotify.Search (
      Search_Complete_CB
    , c_sp_search_create
    , c_sp_search_is_loaded
    , c_sp_search_error
    , c_sp_search_num_tracks
    , c_sp_search_track
    , c_sp_search_num_albums
    , c_sp_search_album
    , c_sp_search_num_playlists
    , c_sp_search_playlist
    , c_sp_search_playlist_name
    , c_sp_search_playlist_uri
    , c_sp_search_playlist_image_uri
    , c_sp_search_num_artists
    , c_sp_search_artist
    , c_sp_search_query
    , c_sp_search_did_you_mean
    , c_sp_search_total_tracks
    , c_sp_search_total_albums
    , c_sp_search_total_artists
    , c_sp_search_total_playlists
    , c_sp_search_add_ref
    , c_sp_search_release
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Struct
import Bindings.Spotify.Session
import Bindings.Spotify.Error

type Search_Complete_CB = Ptr Sp_Search -> Ptr () -> IO ()

foreign import ccall "libspotify/api.h sp_search_create"
  c_sp_search_create :: Ptr Sp_Session -> CString -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Sp_Search_Type -> FunPtr Search_Complete_CB -> Ptr () -> IO (Ptr Sp_Search)

foreign import ccall "libspotify/api.h sp_search_is_loaded"
  c_sp_search_is_loaded :: Ptr Sp_Search -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_search_error"
  c_sp_search_error :: Ptr Sp_Search -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_search_num_tracks"
  c_sp_search_num_tracks :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_track"
  c_sp_search_track :: Ptr Sp_Search -> CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_search_num_albums"
  c_sp_search_num_albums :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_album"
  c_sp_search_album :: Ptr Sp_Search -> CInt -> IO (Ptr Sp_Album)

foreign import ccall "libspotify/api.h sp_search_num_playlists"
  c_sp_search_num_playlists :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_playlist"
  c_sp_search_playlist :: Ptr Sp_Search -> CInt -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_search_playlist_name"
  c_sp_search_playlist_name :: Ptr Sp_Search -> CInt -> IO CString

foreign import ccall "libspotify/api.h sp_search_playlist_uri"
  c_sp_search_playlist_uri :: Ptr Sp_Search -> CInt -> IO CString

foreign import ccall "libspotify/api.h sp_search_playlist_image_uri"
  c_sp_search_playlist_image_uri :: Ptr Sp_Search -> CInt -> IO CString

foreign import ccall "libspotify/api.h sp_search_num_artists"
  c_sp_search_num_artists :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_artist"
  c_sp_search_artist :: Ptr Sp_Search -> CInt -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_search_query"
  c_sp_search_query :: Ptr Sp_Search -> IO CString

foreign import ccall "libspotify/api.h sp_search_did_you_mean"
  c_sp_search_did_you_mean :: Ptr Sp_Search -> IO CString

foreign import ccall "libspotify/api.h sp_search_total_tracks"
  c_sp_search_total_tracks :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_total_albums"
  c_sp_search_total_albums :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_total_artists"
  c_sp_search_total_artists :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_total_playlists"
  c_sp_search_total_playlists :: Ptr Sp_Search -> IO CInt

foreign import ccall "libspotify/api.h sp_search_add_ref"
  c_sp_search_add_ref :: Ptr Sp_Search -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_search_release"
  c_sp_search_release :: Ptr Sp_Search -> IO Sp_Error
