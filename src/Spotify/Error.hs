module Spotify.Error
  ( Error(..)
  , unwrapError
  , wrapError
  , sp_error_message
) where

-- hiding unsafePerformIO in Foreign since it is deprecated
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String

import Bindings.Spotify.Error

import Control.Monad
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

data Error =
    Error_Ok
  | Error_Bad_Api_Version
  | Error_Api_Initialization_Failed
  | Error_Track_Not_Playable
  | Error_Bad_Application_Key
  | Error_Bad_Username_Or_Password
  | Error_User_Banned
  | Error_Unable_To_Contact_Server
  | Error_Client_Too_Old
  | Error_Other_Permanent
  | Error_Bad_User_Agent
  | Error_Missing_Callback
  | Error_Invalid_Indata
  | Error_Index_Out_Of_Range
  | Error_User_Needs_Premium
  | Error_Other_Transient
  | Error_Is_Loading
  | Error_No_Stream_Available
  | Error_Permission_Denied
  | Error_Inbox_Is_Full
  | Error_No_Cache
  | Error_No_Such_User
  | Error_No_Credentials
  | Error_Network_Disabled
  | Error_Invalid_Device_Id
  | Error_Cant_Open_Trace_File
  | Error_Application_Banned
  | Error_Offline_Too_Many_Tracks
  | Error_Offline_Disk_Cache
  | Error_Offline_Expired
  | Error_Offline_Not_Allowed
  | Error_Offline_License_Lost
  | Error_Offline_License_Error
  | Error_Lastfm_Auth_Error
  | Error_Invalid_Argument
  | Error_System_Failure

-- TODO could we write unwrapError and wrapError with a bijection
unwrapError :: Error -> Sp_Error
unwrapError Error_Ok                        = sp_error_ok
unwrapError Error_Bad_Api_Version           = sp_error_bad_api_version
unwrapError Error_Api_Initialization_Failed = sp_error_api_initialization_failed
unwrapError Error_Track_Not_Playable        = sp_error_track_not_playable
unwrapError Error_Bad_Application_Key       = sp_error_bad_application_key
unwrapError Error_Bad_Username_Or_Password  = sp_error_bad_username_or_password
unwrapError Error_User_Banned               = sp_error_user_banned
unwrapError Error_Unable_To_Contact_Server  = sp_error_unable_to_contact_server
unwrapError Error_Client_Too_Old            = sp_error_client_too_old
unwrapError Error_Other_Permanent           = sp_error_other_permanent
unwrapError Error_Bad_User_Agent            = sp_error_bad_user_agent
unwrapError Error_Missing_Callback          = sp_error_missing_callback
unwrapError Error_Invalid_Indata            = sp_error_invalid_indata
unwrapError Error_Index_Out_Of_Range        = sp_error_index_out_of_range
unwrapError Error_User_Needs_Premium        = sp_error_user_needs_premium
unwrapError Error_Other_Transient           = sp_error_other_transient
unwrapError Error_Is_Loading                = sp_error_is_loading
unwrapError Error_No_Stream_Available       = sp_error_no_stream_available
unwrapError Error_Permission_Denied         = sp_error_permission_denied
unwrapError Error_Inbox_Is_Full             = sp_error_inbox_is_full
unwrapError Error_No_Cache                  = sp_error_no_cache
unwrapError Error_No_Such_User              = sp_error_no_such_user
unwrapError Error_No_Credentials            = sp_error_no_credentials
unwrapError Error_Network_Disabled          = sp_error_network_disabled
unwrapError Error_Invalid_Device_Id         = sp_error_invalid_device_id
unwrapError Error_Cant_Open_Trace_File      = sp_error_cant_open_trace_file
unwrapError Error_Application_Banned        = sp_error_application_banned
unwrapError Error_Offline_Too_Many_Tracks   = sp_error_offline_too_many_tracks
unwrapError Error_Offline_Disk_Cache        = sp_error_offline_disk_cache
unwrapError Error_Offline_Expired           = sp_error_offline_expired
unwrapError Error_Offline_Not_Allowed       = sp_error_offline_not_allowed
unwrapError Error_Offline_License_Lost      = sp_error_offline_license_lost
unwrapError Error_Offline_License_Error     = sp_error_offline_license_error
unwrapError Error_Lastfm_Auth_Error         = sp_error_lastfm_auth_error
unwrapError Error_Invalid_Argument          = sp_error_invalid_argument
unwrapError Error_System_Failure            = sp_error_system_failure

wrapError :: Sp_Error -> Error
wrapError err = errMap M.! err
  where
    errMap = M.fromList
        [ (sp_error_ok, Error_Ok)
        , (sp_error_bad_api_version, Error_Bad_Api_Version)
        , (sp_error_api_initialization_failed, Error_Api_Initialization_Failed)
        , (sp_error_track_not_playable, Error_Track_Not_Playable)
        , (sp_error_bad_application_key, Error_Bad_Application_Key)
        , (sp_error_bad_username_or_password, Error_Bad_Username_Or_Password)
        , (sp_error_user_banned, Error_User_Banned)
        , (sp_error_unable_to_contact_server, Error_Unable_To_Contact_Server)
        , (sp_error_client_too_old, Error_Client_Too_Old)
        , (sp_error_other_permanent, Error_Other_Permanent)
        , (sp_error_bad_user_agent, Error_Bad_User_Agent)
        , (sp_error_missing_callback, Error_Missing_Callback)
        , (sp_error_invalid_indata, Error_Invalid_Indata)
        , (sp_error_index_out_of_range, Error_Index_Out_Of_Range)
        , (sp_error_user_needs_premium, Error_User_Needs_Premium)
        , (sp_error_other_transient, Error_Other_Transient)
        , (sp_error_is_loading, Error_Is_Loading)
        , (sp_error_no_stream_available, Error_No_Stream_Available)
        , (sp_error_permission_denied, Error_Permission_Denied)
        , (sp_error_inbox_is_full, Error_Inbox_Is_Full)
        , (sp_error_no_cache, Error_No_Cache)
        , (sp_error_no_such_user, Error_No_Such_User)
        , (sp_error_no_credentials, Error_No_Credentials)
        , (sp_error_network_disabled, Error_Network_Disabled)
        , (sp_error_invalid_device_id, Error_Invalid_Device_Id)
        , (sp_error_cant_open_trace_file, Error_Cant_Open_Trace_File)
        , (sp_error_application_banned, Error_Application_Banned)
        , (sp_error_offline_too_many_tracks, Error_Offline_Too_Many_Tracks)
        , (sp_error_offline_disk_cache, Error_Offline_Disk_Cache)
        , (sp_error_offline_expired, Error_Offline_Expired)
        , (sp_error_offline_not_allowed, Error_Offline_Not_Allowed)
        , (sp_error_offline_license_lost, Error_Offline_License_Lost)
        , (sp_error_offline_license_error, Error_Offline_License_Error)
        , (sp_error_lastfm_auth_error, Error_Lastfm_Auth_Error)
        , (sp_error_invalid_argument, Error_Invalid_Argument)
        , (sp_error_system_failure, Error_System_Failure)
        ]

sp_error_message :: Error -> String
{-# NOINLINE sp_error_message #-}
sp_error_message = unsafePerformIO . (peekCString <=< c_sp_error_message) . unwrapError
