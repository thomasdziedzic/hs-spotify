module Bindings.Spotify.Playlist
where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import Control.Applicative

import Bindings.Spotify.CommonTypes
import Bindings.Spotify.Error
import Bindings.Spotify.Session
import Bindings.Spotify.Struct

#include <libspotify/api.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data Sp_Playlist_Callbacks = Sp_Playlist_Callbacks
  { tracks_added                :: FunPtr (Ptr Sp_Playlist -> Ptr Sp_Track -> Ptr (Ptr Sp_Track) -> CInt -> CInt -> Ptr () -> IO ())
  , tracks_removed              :: FunPtr (Ptr Sp_Playlist -> Ptr CInt -> CInt -> Ptr () -> IO ())
  , tracks_moved                :: FunPtr (Ptr Sp_Playlist -> Ptr CInt -> CInt -> CInt -> Ptr () -> IO ())
  , playlist_renamed            :: FunPtr (Ptr Sp_Playlist -> Ptr () -> IO ())
  , playlist_state_changed      :: FunPtr (Ptr Sp_Playlist -> Ptr () -> IO ())
  , playlist_update_in_progress :: FunPtr (Ptr Sp_Playlist -> Sp_Bool -> Ptr () -> IO ())
  , playlist_metadata_updated   :: FunPtr (Ptr Sp_Playlist -> Ptr () -> IO ())
  , track_created_changed       :: FunPtr (Ptr Sp_Playlist -> CInt -> Ptr Sp_User -> CInt -> Ptr () -> IO ())
  , track_seen_changed          :: FunPtr (Ptr Sp_Playlist -> CInt -> Sp_Bool -> Ptr () -> IO ())
  , description_changed         :: FunPtr (Ptr Sp_Playlist -> CString -> Ptr () -> IO ())
  , image_changed               :: FunPtr (Ptr Sp_Playlist -> Ptr Sp_Byte -> Ptr () -> IO ())
  , track_message_changed       :: FunPtr (Ptr Sp_Playlist -> CInt -> CString -> Ptr () -> IO ())
  , subscribers_changed         :: FunPtr (Ptr Sp_Playlist -> Ptr () -> IO ())
  }

instance Storable Sp_Playlist_Callbacks where
  sizeOf _ = #size sp_playlist_callbacks
  alignment _ = #alignment sp_playlist_callbacks
  peek ptr =
    Sp_Playlist_Callbacks
      <$> (#peek sp_playlist_callbacks, tracks_added) ptr
      <*> (#peek sp_playlist_callbacks, tracks_removed) ptr
      <*> (#peek sp_playlist_callbacks, tracks_moved) ptr
      <*> (#peek sp_playlist_callbacks, playlist_renamed) ptr
      <*> (#peek sp_playlist_callbacks, playlist_state_changed) ptr
      <*> (#peek sp_playlist_callbacks, playlist_update_in_progress) ptr
      <*> (#peek sp_playlist_callbacks, playlist_metadata_updated) ptr
      <*> (#peek sp_playlist_callbacks, track_created_changed) ptr
      <*> (#peek sp_playlist_callbacks, track_seen_changed) ptr
      <*> (#peek sp_playlist_callbacks, description_changed) ptr
      <*> (#peek sp_playlist_callbacks, image_changed) ptr
      <*> (#peek sp_playlist_callbacks, track_message_changed) ptr
      <*> (#peek sp_playlist_callbacks, subscribers_changed) ptr
  poke ptr playlistCallbacks = do
    (#poke sp_playlist_callbacks, tracks_added) ptr (tracks_added playlistCallbacks)
    (#poke sp_playlist_callbacks, tracks_removed) ptr (tracks_removed playlistCallbacks)
    (#poke sp_playlist_callbacks, tracks_moved) ptr (tracks_moved playlistCallbacks)
    (#poke sp_playlist_callbacks, playlist_renamed) ptr (playlist_renamed playlistCallbacks)
    (#poke sp_playlist_callbacks, playlist_state_changed) ptr (playlist_state_changed playlistCallbacks)
    (#poke sp_playlist_callbacks, playlist_update_in_progress) ptr (playlist_update_in_progress playlistCallbacks)
    (#poke sp_playlist_callbacks, playlist_metadata_updated) ptr (playlist_metadata_updated playlistCallbacks)
    (#poke sp_playlist_callbacks, track_created_changed) ptr (track_created_changed playlistCallbacks)
    (#poke sp_playlist_callbacks, track_seen_changed) ptr (track_seen_changed playlistCallbacks)
    (#poke sp_playlist_callbacks, description_changed) ptr (description_changed playlistCallbacks)
    (#poke sp_playlist_callbacks, image_changed) ptr (image_changed playlistCallbacks)
    (#poke sp_playlist_callbacks, track_message_changed) ptr (track_message_changed playlistCallbacks)
    (#poke sp_playlist_callbacks, subscribers_changed) ptr (subscribers_changed playlistCallbacks)

foreign import ccall "libspotify/api.h sp_playlist_is_loaded"
  c_sp_playlist_is_loaded :: Ptr Sp_Playlist -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_playlist_add_callbacks"
  c_sp_playlist_add_callbacks :: Ptr Sp_Playlist -> Ptr Sp_Playlist_Callbacks -> Ptr () -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_remove_callbacks"
  c_sp_playlist_remove_callbacks :: Ptr Sp_Playlist -> Ptr Sp_Playlist_Callbacks -> Ptr () -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_num_tracks"
  c_sp_playlist_num_tracks :: Ptr Sp_Playlist -> IO CInt

foreign import ccall "libspotify/api.h sp_playlist_track"
  c_sp_playlist_track :: Ptr Sp_Playlist -> CInt -> IO (Ptr Sp_Track)

foreign import ccall "libspotify/api.h sp_playlist_track_create_time"
  c_sp_playlist_track_create_time :: Ptr Sp_Playlist -> CInt -> IO CInt

foreign import ccall "libspotify/api.h sp_playlist_track_creator"
  c_sp_playlist_track_creator :: Ptr Sp_Playlist -> CInt -> IO (Ptr Sp_User)

foreign import ccall "libspotify/api.h sp_playlist_track_seen"
  c_sp_playlist_track_seen :: Ptr Sp_Playlist -> CInt -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_playlist_track_set_seen"
  c_sp_playlist_track_set_seen :: Ptr Sp_Playlist -> CInt -> Sp_Bool -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_track_message"
  c_sp_playlist_track_message :: Ptr Sp_Playlist -> CInt -> IO CString

foreign import ccall "libspotify/api.h sp_playlist_name"
  c_sp_playlist_name :: Ptr Sp_Playlist -> IO CString

foreign import ccall "libspotify/api.h sp_playlist_rename"
  c_sp_playlist_rename :: Ptr Sp_Playlist -> CString -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_owner"
  c_sp_playlist_owner :: Ptr Sp_Playlist -> IO (Ptr Sp_User)

foreign import ccall "libspotify/api.h sp_playlist_is_collaborative"
  c_sp_playlist_is_collaborative :: Ptr Sp_Playlist -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_playlist_set_collaborative"
  c_sp_playlist_set_collaborative :: Ptr Sp_Playlist -> Sp_Bool -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_set_autolink_tracks"
  c_sp_playlist_set_autolink_tracks :: Ptr Sp_Playlist -> Sp_Bool -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_get_description"
  c_sp_playlist_get_description :: Ptr Sp_Playlist -> IO CString

foreign import ccall "libspotify/api.h sp_playlist_get_image"
  c_sp_playlist_get_image :: Ptr Sp_Playlist -> Ptr Sp_Byte -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_playlist_has_pending_changes"
  c_sp_playlist_has_pending_changes :: Ptr Sp_Playlist -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_playlist_add_tracks"
  c_sp_playlist_add_tracks :: Ptr Sp_Playlist -> Ptr (Ptr Sp_Track) -> CInt -> CInt -> Ptr Sp_Session -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_remove_tracks"
  c_sp_playlist_remove_tracks :: Ptr Sp_Playlist -> Ptr (Ptr Sp_Track) -> CInt -> CInt -> Ptr Sp_Session -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_reorder_tracks"
  c_sp_playlist_reorder_tracks :: Ptr Sp_Playlist -> Ptr CInt -> CInt -> CInt -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_num_subscribers"
  c_sp_playlist_num_subscribers :: Ptr Sp_Playlist -> IO CUInt

foreign import ccall "libspotify/api.h sp_playlist_subscribers"
  c_sp_playlist_subscribers :: Ptr Sp_Playlist -> IO (Ptr Sp_Subscribers)

foreign import ccall "libspotify/api.h sp_playlist_subscribers_free"
  c_sp_playlist_subscribers_free :: Ptr Sp_Subscribers -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_update_subscribers"
  c_sp_playlist_update_subscribers :: Ptr Sp_Session -> Ptr Sp_Playlist -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_is_in_ram"
  c_sp_playlist_is_in_ram :: Ptr Sp_Session -> Ptr Sp_Playlist -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_playlist_set_in_ram"
  c_sp_playlist_set_in_ram :: Ptr Sp_Session -> Ptr Sp_Playlist -> Sp_Bool -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_create"
  c_sp_playlist_create :: Ptr Sp_Session -> Ptr Sp_Link -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_playlist_set_offline_mode"
  c_sp_playlist_set_offline_mode :: Ptr Sp_Session -> Ptr Sp_Playlist -> Sp_Bool -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_get_offline_status"
  c_sp_playlist_get_offline_status :: Ptr Sp_Session -> Ptr Sp_Playlist -> IO Sp_Playlist_Offline_Status

foreign import ccall "libspotify/api.h sp_playlist_get_offline_download_completed"
  c_sp_playlist_get_offline_download_completed :: Ptr Sp_Session -> Ptr Sp_Playlist -> IO CInt

foreign import ccall "libspotify/api.h sp_playlist_add_ref"
  c_sp_playlist_add_ref :: Ptr Sp_Playlist -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlist_release"
  c_sp_playlist_release :: Ptr Sp_Playlist -> IO Sp_Error

data Sp_Playlistcontainer_Callbacks = Sp_Playlistcontainer_Callbacks
  { playlist_added :: FunPtr (Ptr Sp_Playlistcontainer -> Ptr Sp_Playlist -> CInt -> Ptr () -> IO ())
  , playlist_removed :: FunPtr (Ptr Sp_Playlistcontainer -> Ptr Sp_Playlist -> CInt -> Ptr () -> IO ())
  , playlist_moved :: FunPtr (Ptr Sp_Playlistcontainer -> Ptr Sp_Playlist -> CInt -> CInt -> Ptr () -> IO ())
  , container_loaded :: FunPtr (Ptr Sp_Playlistcontainer -> Ptr () -> IO ())
  }

instance Storable Sp_Playlistcontainer_Callbacks where
  sizeOf _ = #size sp_playlistcontainer_callbacks
  alignment _ = #alignment sp_playlistcontainer_callbacks
  peek ptr =
    Sp_Playlistcontainer_Callbacks
      <$> (#peek sp_playlistcontainer_callbacks, playlist_added) ptr
      <*> (#peek sp_playlistcontainer_callbacks, playlist_removed) ptr
      <*> (#peek sp_playlistcontainer_callbacks, playlist_moved) ptr
      <*> (#peek sp_playlistcontainer_callbacks, container_loaded) ptr
  poke ptr playlistcontainerCallbacks = do
    (#poke sp_playlistcontainer_callbacks, playlist_added) ptr (playlist_added playlistcontainerCallbacks)
    (#poke sp_playlistcontainer_callbacks, playlist_removed) ptr (playlist_removed playlistcontainerCallbacks)
    (#poke sp_playlistcontainer_callbacks, playlist_moved) ptr (playlist_moved playlistcontainerCallbacks)
    (#poke sp_playlistcontainer_callbacks, container_loaded) ptr (container_loaded playlistcontainerCallbacks)

foreign import ccall "libspotify/api.h sp_playlistcontainer_add_callbacks"
  c_sp_playlistcontainer_add_callbacks :: Ptr Sp_Playlistcontainer -> Ptr Sp_Playlistcontainer_Callbacks -> Ptr () -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_remove_callbacks"
  c_sp_playlistcontainer_remove_callbacks :: Ptr Sp_Playlistcontainer -> Ptr Sp_Playlistcontainer_Callbacks -> Ptr () -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_num_playlists"
  c_sp_playlistcontainer_num_playlists :: Ptr Sp_Playlistcontainer -> IO CInt

foreign import ccall "libspotify/api.h sp_playlistcontainer_is_loaded"
  c_sp_playlistcontainer_is_loaded :: Ptr Sp_Playlistcontainer -> IO Sp_Bool

foreign import ccall "libspotify/api.h sp_playlistcontainer_playlist"
  c_sp_playlistcontainer_playlist :: Ptr Sp_Playlistcontainer -> CInt -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_playlistcontainer_playlist_type"
  c_sp_playlistcontainer_playlist_type :: Ptr Sp_Playlistcontainer -> CInt -> IO Sp_Playlist_Type

foreign import ccall "libspotify/api.h sp_playlistcontainer_playlist_folder_name"
  c_sp_playlistcontainer_playlist_folder_name :: Ptr Sp_Playlistcontainer -> CInt -> CString -> CInt -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_playlist_folder_id"
  c_sp_playlistcontainer_playlist_folder_id :: Ptr Sp_Playlistcontainer -> CInt -> IO CULong

foreign import ccall "libspotify/api.h sp_playlistcontainer_add_new_playlist"
  c_sp_playlistcontainer_add_new_playlist :: Ptr Sp_Playlistcontainer -> CString -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_playlistcontainer_add_playlist"
  c_sp_playlistcontainer_add_playlist :: Ptr Sp_Playlistcontainer -> Ptr Sp_Link -> IO (Ptr Sp_Playlist)

foreign import ccall "libspotify/api.h sp_playlistcontainer_remove_playlist"
  c_sp_playlistcontainer_remove_playlist :: Ptr Sp_Playlistcontainer -> CInt -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_move_playlist"
  c_sp_playlistcontainer_move_playlist :: Ptr Sp_Playlistcontainer -> CInt -> CInt -> Sp_Bool -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_add_folder"
  c_sp_playlistcontainer_add_folder :: Ptr Sp_Playlistcontainer -> CInt -> CString -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_owner"
  c_sp_playlistcontainer_owner :: Ptr Sp_Playlistcontainer -> IO (Ptr Sp_User)

foreign import ccall "libspotify/api.h sp_playlistcontainer_add_ref"
  c_sp_playlistcontainer_add_ref :: Ptr Sp_Playlistcontainer -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_release"
  c_sp_playlistcontainer_release :: Ptr Sp_Playlistcontainer -> IO Sp_Error

foreign import ccall "libspotify/api.h sp_playlistcontainer_get_unseen_tracks"
  c_sp_playlistcontainer_get_unseen_tracks :: Ptr Sp_Playlistcontainer -> Ptr Sp_Playlist -> Ptr (Ptr Sp_Track) -> CInt -> IO CInt

foreign import ccall "libspotify/api.h sp_playlistcontainer_clear_unseen_tracks"
  c_sp_playlistcontainer_clear_unseen_tracks :: Ptr Sp_Playlistcontainer -> Ptr Sp_Playlist -> IO CInt
