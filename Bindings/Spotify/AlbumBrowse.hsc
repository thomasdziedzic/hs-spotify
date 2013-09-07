module Bindings.Spotify.AlbumBrowse
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Struct
import Bindings.Spotify.Error

type Albumbrowse_Comblete_CB = Ptr Sp_Albumbrowse -> Ptr () -> IO ()

foreign import ccall "libspotify/api.h sp_albumbrowse_create"
  c_sp_albumbrowse_create :: Ptr Sp_Session -> Ptr Sp_Album -> FunPtr Albumbrowse_Comblete_CB -> Ptr () -> IO (Ptr Sp_Albumbrowse)

foreign import ccall "libspotify/api.h sp_albumbrowse_is_loaded"
  c_sp_albumbrowse_is_loaded :: Ptr Sp_Albumbrowse -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_albumbrowse_error"
  c_sp_albumbrowse_error :: Ptr Sp_Albumbrowse -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_albumbrowse_album"
  c_sp_albumbrowse_album :: Ptr Sp_Albumbrowse -> IO (Ptr Sp_Album)

foreign import ccall "libspotify/api.h sp_albumbrowse_artist"
  c_sp_albumbrowse_artist :: Ptr Sp_Albumbrowse -> IO (Ptr Sp_Artist)

foreign import ccall "libspotify/api.h sp_albumbrowse_num_copyrights"
  c_sp_albumbrowse_num_copyrights :: Ptr Sp_Albumbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_albumbrowse_copyright"
  c_sp_albumbrowse_copyright :: Ptr Sp_Albumbrowse -> CInt -> IO CString

foreign import ccall "libspotify/api.h sp_albumbrowse_num_tracks"
  c_sp_albumbrowse_num_tracks :: Ptr Sp_Albumbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_albumbrowse_track"
  c_sp_albumbrowse_track :: Ptr Sp_Albumbrowse -> CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_albumbrowse_review"
  c_sp_albumbrowse_review :: Ptr Sp_Albumbrowse -> IO CString

foreign import ccall "libspotify/api.h sp_albumbrowse_backend_request_duration"
  c_sp_albumbrowse_backend_request_duration :: Ptr Sp_Albumbrowse -> IO CInt

foreign import ccall "libspotify/api.h sp_albumbrowse_add_ref"
  c_sp_albumbrowse_add_ref :: Ptr Sp_Albumbrowse -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_albumbrowse_release"
  c_sp_albumbrowse_release :: Ptr Sp_Albumbrowse -> IO Sp_Error
