module Bindings.Spotify.Misc
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "libspotify/api.h sp_build_id"
  c_sp_build_id :: IO CString
