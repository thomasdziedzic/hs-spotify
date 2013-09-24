module Actions (
      Action(..)
) where

import Spotify.Struct

type Username = String
type Password = String

data Action = ProcessEvents Session
            | Login Session Username Password
