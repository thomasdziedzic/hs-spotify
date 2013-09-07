module Bindings.Spotify.Link
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.Struct
import Bindings.Spotify.Session
import Bindings.Spotify.Error

#include <libspotify/api.h>

newtype Sp_Linktype = Sp_Linktype { unSp_Linktype :: CInt }
  deriving (Show)

#{enum Sp_Linktype, Sp_Linktype
  , sp_linktype_invalid    = SP_LINKTYPE_INVALID
  , sp_linktype_track      = SP_LINKTYPE_TRACK
  , sp_linktype_album      = SP_LINKTYPE_ALBUM
  , sp_linktype_artist     = SP_LINKTYPE_ARTIST
  , sp_linktype_search     = SP_LINKTYPE_SEARCH
  , sp_linktype_playlist   = SP_LINKTYPE_PLAYLIST
  , sp_linktype_profile    = SP_LINKTYPE_PROFILE
  , sp_linktype_starred    = SP_LINKTYPE_STARRED
  , sp_linktype_localtrack = SP_LINKTYPE_LOCALTRACK
  , sp_linktype_image      = SP_LINKTYPE_IMAGE
  }

foreign import ccall "libspotify/api.h sp_link_create_from_string"
  c_sp_link_create_from_string :: CString -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_track"
  c_sp_link_create_from_track :: Ptr Sp_Track -> CInt -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_album"
  c_sp_link_create_from_album :: Ptr Sp_Album -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_album_cover"
  c_sp_link_create_from_album_cover :: Ptr Sp_Album -> Sp_Image_Size -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_artist"
  c_sp_link_create_from_artist :: Ptr Sp_Artist -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_artist_portrait"
  c_sp_link_create_from_artist_portrait :: Ptr Sp_Artist -> Sp_Image_Size -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_artistbrowse_portrait"
  c_sp_link_create_from_artistbrowse_portrait :: Ptr Sp_Artistbrowse -> CInt -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_search"
  c_sp_link_create_from_search :: Ptr Sp_Search -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_playlist"
  c_sp_link_create_from_playlist :: Ptr Sp_Playlist -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_user"
  c_sp_link_create_from_user :: Ptr Sp_User -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_create_from_image"
  c_sp_link_create_from_image :: Ptr Sp_Image -> IO (Ptr Sp_Link)

foreign import ccall "libspotify/api.h sp_link_as_string"
  c_sp_link_as_string :: Ptr Sp_Link -> CString -> CInt -> IO CInt

foreign import ccall "libspotify/api.h sp_link_type"
  c_sp_link_type :: Ptr Sp_Link -> IO Sp_Linktype

foreign import ccall "libspotify/api.h sp_link_as_track"
  c_sp_link_as_track :: Ptr Sp_Link -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_link_as_track_and_offset"
  c_sp_link_as_track_and_offset :: Ptr Sp_Link -> Ptr CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_link_as_album"
  c_sp_link_as_album :: Ptr Sp_Link -> IO (Ptr Sp_Album)

foreign import ccall "libspotify/api.h sp_link_as_artist"
  c_sp_link_as_artist :: Ptr Sp_Link -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_link_as_user"
  c_sp_link_as_user :: Ptr Sp_Link -> IO (Ptr Sp_User)

foreign import ccall "libspotify/api.h sp_link_add_ref"
  c_sp_link_add_ref :: Ptr Sp_Link -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_link_release"
  c_sp_link_release :: Ptr Sp_Link -> IO Sp_Error
