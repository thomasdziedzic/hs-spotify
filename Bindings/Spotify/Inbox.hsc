module Bindings.Spotify.Inbox
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.Error
import Bindings.Spotify.Session
import Bindings.Spotify.Struct

type Inboxpost_Complete_CB = Ptr Sp_Inbox -> Ptr () -> IO ()

foreign import ccall "libspotify/api.h sp_inbox_post_tracks"
  c_sp_inbox_post_tracks :: Ptr Sp_Session -> CString -> Ptr (Ptr Sp_Track) -> CInt -> CString -> FunPtr Inboxpost_Complete_CB -> Ptr () -> IO (Ptr Sp_Inbox)

foreign import ccall "libspotify/api.h sp_inbox_error"
  c_sp_inbox_error :: Ptr Sp_Inbox -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_inbox_add_ref"
  c_sp_inbox_add_ref :: Ptr Sp_Inbox -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_inbox_release"
  c_sp_inbox_release :: Ptr Sp_Inbox -> IO Sp_Error
