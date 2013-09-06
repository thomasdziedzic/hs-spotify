module Bindings.Spotify.Artist
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.Struct
import Bindings.Spotify.Session
import Bindings.Spotify.Error

foreign import ccall "libspotify/api.h sp_artist_name"
  c_sp_artist_name :: Ptr Sp_Artist -> IO CString

foreign import ccall "libspotify/api.h sp_artist_is_loaded"
  c_sp_artist_is_loaded :: Ptr Sp_Artist -> IO CUChar

foreign import ccall "libspotify/api.h sp_artist_portrait"
  c_sp_artist_portrait :: Ptr Sp_Artist -> Sp_Image_Size -> IO (Ptr CUChar)

foreign import ccall "libspotify/api.h sp_artist_add_ref"
  c_sp_artist_add_ref :: Ptr Sp_Artist -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_artist_release"
  c_sp_artist_release :: Ptr Sp_Artist -> IO Sp_Error
