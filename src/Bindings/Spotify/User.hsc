module Bindings.Spotify.User
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Error
import Bindings.Spotify.Struct

#include <libspotify/api.h>

data Sp_Relation_Type = Sp_Relation_Type { unSp_Relation_Type :: CInt }
  deriving (Show)

#{enum Sp_Relation_Type, Sp_Relation_Type
  , sp_relation_type_unknown        = SP_RELATION_TYPE_UNKNOWN
  , sp_relation_type_none           = SP_RELATION_TYPE_NONE
  , sp_relation_type_unidirectional = SP_RELATION_TYPE_UNIDIRECTIONAL
  , sp_relation_type_bidirectional  = SP_RELATION_TYPE_BIDIRECTIONAL
  }

foreign import ccall "libspotify/api.h sp_user_canonical_name"
  c_sp_user_canonical_name :: Ptr Sp_User -> IO CString

foreign import ccall "libspotify/api.h sp_user_display_name"
  c_sp_user_display_name :: Ptr Sp_User -> IO CString

foreign import ccall "libspotify/api.h sp_user_is_loaded"
  c_sp_user_is_loaded :: Ptr Sp_User -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_user_add_ref"
  c_sp_user_add_ref :: Ptr Sp_User -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_user_release"
  c_sp_user_release :: Ptr Sp_User -> IO Sp_Error
