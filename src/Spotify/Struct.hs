module Spotify.Struct (
      Session(..)
    , Track(..)
    , Album(..)
    , Artist(..)
    , Artistbrowse(..)
    , Albumbrowse(..)
    , Toplistbrowse(..)
    , Search(..)
    , Link(..)
    , Image(..)
    , User(..)
    , Playlist(..)
    , Playlistcontainer(..)
    , Inbox(..)
) where

import Foreign.Ptr
import Bindings.Spotify.Struct

data Session           = Session (Ptr Sp_Session)
data Track             = Track (Ptr Sp_Track)
data Album             = Album (Ptr Sp_Album)
data Artist            = Artist (Ptr Sp_Artist)
data Artistbrowse      = Artistbrowse (Ptr Sp_Artistbrowse)
data Albumbrowse       = Albumbrowse (Ptr Sp_Albumbrowse)
data Toplistbrowse     = Toplistbrowse (Ptr Sp_Toplistbrowse)
data Search            = Search (Ptr Sp_Search)
data Link              = Link (Ptr Sp_Link)
data Image             = Image (Ptr Sp_Image)
data User              = User (Ptr Sp_User)
data Playlist          = Playlist (Ptr Sp_Playlist)
data Playlistcontainer = Playlistcontainer (Ptr Sp_Playlistcontainer)
data Inbox             = Inbox (Ptr Sp_Inbox)
