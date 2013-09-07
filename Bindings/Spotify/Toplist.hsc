module Bindings.Spotify.Toplist
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Error
import Bindings.Spotify.Struct

#include <libspotify/api.h>

newtype Sp_Toplisttype = Sp_Toplisttype { unSp_Toplisttype :: CInt }
  deriving (Show)

#{enum Sp_Toplisttype, Sp_Toplisttype
  , sp_toplist_type_artists = SP_TOPLIST_TYPE_ARTISTS
  , sp_toplist_type_albums  = SP_TOPLIST_TYPE_ALBUMS
  , sp_toplist_type_tracks  = SP_TOPLIST_TYPE_TRACKS
  }

newtype Sp_Toplistregion = Sp_Toplistregion { unSp_Toplistregion :: CInt }
  deriving (Show)

#{enum Sp_Toplistregion, Sp_Toplistregion
  , sp_toplist_region_everywhere = SP_TOPLIST_REGION_EVERYWHERE
  , sp_toplist_region_user       = SP_TOPLIST_REGION_USER
  }

type Toplistbrowse_Complete_CB = Ptr Sp_Toplistbrowse -> Ptr () -> IO ()

foreign import ccall "libspotify/api.h sp_toplistbrowse_create"
  c_sp_toplistbrowse_create :: Ptr Sp_Session -> Sp_Toplisttype -> Sp_Toplistregion -> CString -> FunPtr Toplistbrowse_Complete_CB -> Ptr () -> IO (Ptr Sp_Toplistbrowse)

foreign import ccall "libspotify/api.h sp_toplistbrowse_is_loaded"
  c_sp_toplistbrowse_is_loaded :: Ptr Sp_Toplistbrowse -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_toplistbrowse_error"
  c_sp_toplistbrowse_error :: Ptr Sp_Toplistbrowse -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_toplistbrowse_add_ref"
  c_sp_toplistbrowse_add_ref :: Ptr Sp_Toplistbrowse -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_toplistbrowse_release"
  c_sp_toplistbrowse_release :: Ptr Sp_Toplistbrowse -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_toplistbrowse_num_artists"
  c_sp_toplistbrowse_num_artists :: Ptr Sp_Toplistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_toplistbrowse_artist"
  c_sp_toplistbrowse_artist :: Ptr Sp_Toplistbrowse -> CInt -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_toplistbrowse_num_albums"
  c_sp_toplistbrowse_num_albums :: Ptr Sp_Toplistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_toplistbrowse_album"
  c_sp_toplistbrowse_album :: Ptr Sp_Toplistbrowse -> CInt -> IO (Ptr Sp_Album)

foreign import ccall "libspotify/api.h sp_toplistbrowse_num_tracks"
  c_sp_toplistbrowse_num_tracks :: Ptr Sp_Toplistbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_toplistbrowse_track"
  c_sp_toplistbrowse_track :: Ptr Sp_Toplistbrowse -> CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_toplistbrowse_backend_request_duration"
  c_sp_toplistbrowse_backend_request_duration :: Ptr Sp_Toplistbrowse -> IO CInt
