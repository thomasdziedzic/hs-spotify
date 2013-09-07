module Bindings.Spotify.ArtistBrowse
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Struct
import Bindings.Spotify.Session
import Bindings.Spotify.Error

type Artistbrowse_Comblete_CB = Ptr Sp_Artistbrowse -> Ptr () -> IO ()

foreign import ccall "libspotify/api.h sp_artistbrowse_create"
  c_sp_artistbrowse_create :: Ptr Sp_Session -> Ptr Sp_Artist -> Sp_ArtistBrowse_Type -> FunPtr Artistbrowse_Comblete_CB -> Ptr () -> IO (Ptr Sp_Artistbrowse)

foreign import ccall "libspotify/api.h sp_artistbrowse_is_loaded"
  c_sp_artistbrowse_is_loaded :: Ptr Sp_Artistbrowse -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_artistbrowse_error"
  c_sp_artistbrowse_error :: Ptr Sp_Artistbrowse -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_artistbrowse_artist"
  c_sp_artistbrowse_artist :: Ptr Sp_Artistbrowse -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_artistbrowse_num_portraits"
  c_sp_artistbrowse_num_portraits :: Ptr Sp_Artistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_artistbrowse_portrait"
  c_sp_artistbrowse_portrait :: Ptr Sp_Artistbrowse -> CInt -> IO (Ptr Sp_Byte)

foreign import ccall "libspotify/api.h sp_artistbrowse_num_tracks"
  c_sp_artistbrowse_num_tracks :: Ptr Sp_Artistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_artistbrowse_track"
  c_sp_artistbrowse_track :: Ptr Sp_Artistbrowse -> CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_artistbrowse_num_tophit_tracks"
  c_sp_artistbrowse_num_tophit_tracks :: Ptr Sp_Artistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_artistbrowse_tophit_track"
  c_sp_artistbrowse_tophit_track :: Ptr Sp_Artistbrowse -> CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_artistbrowse_num_albums"
  c_sp_artistbrowse_num_albums :: Ptr Sp_Artistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_artistbrowse_album"
  c_sp_artistbrowse_album :: Ptr Sp_Artistbrowse -> CInt -> IO (Ptr Sp_Album)

foreign import ccall "libspotify/api.h sp_artistbrowse_num_similar_artists"
  c_sp_artistbrowse_num_similar_artists :: Ptr Sp_Artistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_artistbrowse_similar_artist"
  c_sp_artistbrowse_similar_artist :: Ptr Sp_Artistbrowse -> CInt -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_artistbrowse_biography"
  c_sp_artistbrowse_biography :: Ptr Sp_Artistbrowse -> IO CString

foreign import ccall "libspotify/api.h sp_artistbrowse_backend_request_duration"
  c_sp_artistbrowse_backend_request_duration :: Ptr Sp_Artistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_artistbrowse_add_ref"
  c_sp_artistbrowse_add_ref :: Ptr Sp_Artistbrowse -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_artistbrowse_release"
  c_sp_artistbrowse_release :: Ptr Sp_Artistbrowse -> IO Sp_Error
