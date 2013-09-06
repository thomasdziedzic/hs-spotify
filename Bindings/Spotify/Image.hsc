module Bindings.Spotify.Image
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Bindings.Spotify.Struct
import Bindings.Spotify.Session
import Bindings.Spotify.Error

#include <libspotify/api.h>

newtype Sp_Imageformat = Sp_Imageformat { unSp_Imageformat :: CInt }
  deriving (Show)

#{enum Sp_Imageformat, Sp_Imageformat
  , sp_image_format_unknown = SP_IMAGE_FORMAT_UNKNOWN
  , sp_image_format_jpeg    = SP_IMAGE_FORMAT_JPEG
  }

type Image_Loaded_CB = Ptr Sp_Image -> Ptr () -> IO ()

foreign import ccall "libspotify/api.h sp_image_create"
  c_sp_image_create :: Ptr Sp_Session -> Ptr CUChar -> IO (Ptr Sp_Image)

foreign import ccall "libspotify/api.h sp_image_create_from_link"
  c_sp_image_create_from_link :: Ptr Sp_Session -> Ptr Sp_Link -> IO (Ptr Sp_Image)

foreign import ccall "libspotify/api.h sp_image_add_load_callback"
  c_sp_image_add_load_callback :: Ptr Sp_Image -> FunPtr Image_Loaded_CB -> Ptr () -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_image_remove_load_callback"
  c_sp_image_remove_load_callback :: Ptr Sp_Image -> FunPtr Image_Loaded_CB -> Ptr () -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_image_is_loaded"
  c_sp_image_is_loaded :: Ptr Sp_Image -> IO CUChar

foreign import ccall "libspotify/api.h sp_image_error"
  c_sp_image_error :: Ptr Sp_Image -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_image_format"
  c_sp_image_format :: Ptr Sp_Image -> IO Sp_Imageformat

foreign import ccall "libspotify/api.h sp_image_data"
  c_sp_image_data :: Ptr Sp_Image -> Ptr CSize -> IO (Ptr ())

foreign import ccall "libspotify/api.h sp_image_image_id"
  c_sp_image_image_id :: Ptr Sp_Image -> IO (Ptr CUChar)

foreign import ccall "libspotify/api.h sp_image_add_ref"
  c_sp_image_add_ref :: Ptr Sp_Image -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_image_release"
  c_sp_image_release :: Ptr Sp_Image -> IO Sp_Error
