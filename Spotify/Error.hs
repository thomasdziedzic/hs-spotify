module Spotify.Error
  ( Sp_Error(..)
) where

import Prelude hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String

import Bindings.Spotify.Error

-- TODO move Sp_Error into a data type so we wont be dealing with CInt

sp_error_message :: Sp_Error -> String
sp_error_message = unsafePerformIO . peekCString . c_sp_error_message
