module Bindings.Spotify.CommonTypes (
      Sp_Bool
    , Sp_Byte
) where

import Foreign.C.Types

type Sp_Bool = CUChar
type Sp_Byte = CUChar
