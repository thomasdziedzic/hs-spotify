module Spotify.Link (
      linkCreateFromString
    , linkAsTrack
    , linkRelease
) where

import Foreign.C.String
import Foreign.Ptr

import Bindings.Spotify.Link
import Bindings.Spotify.Error

import Spotify.Struct
import Spotify.Error

-- returned Link needs to be freed after you use it with linkRelease
-- TODO look into releasing this with a ForeignPtr
linkCreateFromString :: String -> IO Link
linkCreateFromString str = do
    withCString str $ \cstr -> do
        linkPtr <- c_sp_link_create_from_string cstr
        return (Link linkPtr)

linkAsTrack :: Link -> IO (Maybe Track)
linkAsTrack (Link linkPtr) = do
    trackPtr <- c_sp_link_as_track linkPtr
    if trackPtr == nullPtr
        then
            return Nothing
        else
            return $ Just (Track trackPtr)

linkRelease :: Link -> IO (Maybe Error)
linkRelease (Link linkPtr) = do
    spError <- c_sp_link_release linkPtr
    if spError == sp_error_ok
        then
            return Nothing
        else
            return $ Just (wrapError spError)
