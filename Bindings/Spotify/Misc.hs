{-# LINE 1 "Misc.hsc" #-}
module Bindings.Spotify.Misc
{-# LINE 2 "Misc.hsc" #-}
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "libspotify/api.h sp_build_id"
  c_sp_build_id :: IO CString
