module Bindings.Spotify.Album (
      Sp_Albumtype(..)
    , sp_albumtype_album
    , sp_albumtype_single
    , sp_albumtype_compilation
    , sp_albumtype_unknown
    , c_sp_album_is_loaded
    , c_sp_album_is_available
    , c_sp_album_artist
    , c_sp_album_cover
    , c_sp_album_name
    , c_sp_album_year
    , c_sp_album_type
    , c_sp_album_add_ref
    , c_sp_album_release
) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Struct
import Bindings.Spotify.Session
import Bindings.Spotify.Error

#include <libspotify/api.h>

newtype Sp_Albumtype = Sp_Albumtype { unSp_Albumtype :: CInt }
  deriving (Show)

#{enum Sp_Albumtype, Sp_Albumtype
  , sp_albumtype_album       = SP_ALBUMTYPE_ALBUM
  , sp_albumtype_single      = SP_ALBUMTYPE_SINGLE
  , sp_albumtype_compilation = SP_ALBUMTYPE_COMPILATION
  , sp_albumtype_unknown     = SP_ALBUMTYPE_UNKNOWN
  }

foreign import ccall "libspotify/api.h sp_album_is_loaded"
  c_sp_album_is_loaded :: Ptr Sp_Album -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_album_is_available"
  c_sp_album_is_available :: Ptr Sp_Album -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_album_artist"
  c_sp_album_artist :: Ptr Sp_Album -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_album_cover"
  c_sp_album_cover :: Ptr Sp_Album -> Sp_Image_Size -> IO (Ptr Sp_Byte)

foreign import ccall "libspotify/api.h sp_album_name"
  c_sp_album_name :: Ptr Sp_Album -> IO CString

foreign import ccall "libspotify/api.h sp_album_year"
  c_sp_album_year :: Ptr Sp_Album -> IO CInt

foreign import ccall "libspotify/api.h sp_album_type"
  c_sp_album_type :: Ptr Sp_Album -> IO Sp_Albumtype

foreign import ccall "libspotify/api.h sp_album_add_ref"
  c_sp_album_add_ref :: Ptr Sp_Album -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_album_release"
  c_sp_album_release :: Ptr Sp_Album -> IO Sp_Error
