module Spotify.Error
  ( Sp_ErrorW(..)
  , sp_error_message
) where

-- hiding unsafePerformIO in Foreign since it is deprecated
import Prelude hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String

import Bindings.Spotify.Error

data Sp_ErrorW =
    Sp_ErrorW_Ok
  | Sp_ErrorW_Bad_Api_Version
  | Sp_ErrorW_Api_Initialization_Failed
  | Sp_ErrorW_Track_Not_Playable
  | Sp_ErrorW_Bad_Application_Key
  | Sp_ErrorW_Bad_Username_Or_Password
  | Sp_ErrorW_User_Banned
  | Sp_ErrorW_Unable_To_Contact_Server
  | Sp_ErrorW_Client_Too_Old
  | Sp_ErrorW_Other_Permanent
  | Sp_ErrorW_Bad_User_Agent
  | Sp_ErrorW_Missing_Callback
  | Sp_ErrorW_Invalid_Indata
  | Sp_ErrorW_Index_Out_Of_Range
  | Sp_ErrorW_User_Needs_Premium
  | Sp_ErrorW_Other_Transient
  | Sp_ErrorW_Is_Loading
  | Sp_ErrorW_No_Stream_Available
  | Sp_ErrorW_Permission_Denied
  | Sp_ErrorW_Inbox_Is_Full
  | Sp_ErrorW_No_Cache
  | Sp_ErrorW_No_Such_User
  | Sp_ErrorW_No_Credentials
  | Sp_ErrorW_Network_Disabled
  | Sp_ErrorW_Invalid_Device_Id
  | Sp_ErrorW_Cant_Open_Trace_File
  | Sp_ErrorW_Application_Banned
  | Sp_ErrorW_Offline_Too_Many_Tracks
  | Sp_ErrorW_Offline_Disk_Cache
  | Sp_ErrorW_Offline_Expired
  | Sp_ErrorW_Offline_Not_Allowed
  | Sp_ErrorW_Offline_License_Lost
  | Sp_ErrorW_Offline_License_Error
  | Sp_ErrorW_Lastfm_Auth_Error
  | Sp_ErrorW_Invalid_Argument
  | Sp_ErrorW_System_Failure

unwrapError :: Sp_ErrorW -> Sp_Error
unwrapError Sp_ErrorW_Ok                        = sp_error_ok
unwrapError Sp_ErrorW_Bad_Api_Version           = sp_error_bad_api_version
unwrapError Sp_ErrorW_Api_Initialization_Failed = sp_error_api_initialization_failed
unwrapError Sp_ErrorW_Track_Not_Playable        = sp_error_track_not_playable
unwrapError Sp_ErrorW_Bad_Application_Key       = sp_error_bad_application_key
unwrapError Sp_ErrorW_Bad_Username_Or_Password  = sp_error_bad_username_or_password
unwrapError Sp_ErrorW_User_Banned               = sp_error_user_banned
unwrapError Sp_ErrorW_Unable_To_Contact_Server  = sp_error_unable_to_contact_server
unwrapError Sp_ErrorW_Client_Too_Old            = sp_error_client_too_old
unwrapError Sp_ErrorW_Other_Permanent           = sp_error_other_permanent
unwrapError Sp_ErrorW_Bad_User_Agent            = sp_error_bad_user_agent
unwrapError Sp_ErrorW_Missing_Callback          = sp_error_missing_callback
unwrapError Sp_ErrorW_Invalid_Indata            = sp_error_invalid_indata
unwrapError Sp_ErrorW_Index_Out_Of_Range        = sp_error_index_out_of_range
unwrapError Sp_ErrorW_User_Needs_Premium        = sp_error_user_needs_premium
unwrapError Sp_ErrorW_Other_Transient           = sp_error_other_transient
unwrapError Sp_ErrorW_Is_Loading                = sp_error_is_loading
unwrapError Sp_ErrorW_No_Stream_Available       = sp_error_no_stream_available
unwrapError Sp_ErrorW_Permission_Denied         = sp_error_permission_denied
unwrapError Sp_ErrorW_Inbox_Is_Full             = sp_error_inbox_is_full
unwrapError Sp_ErrorW_No_Cache                  = sp_error_no_cache
unwrapError Sp_ErrorW_No_Such_User              = sp_error_no_such_user
unwrapError Sp_ErrorW_No_Credentials            = sp_error_no_credentials
unwrapError Sp_ErrorW_Network_Disabled          = sp_error_network_disabled
unwrapError Sp_ErrorW_Invalid_Device_Id         = sp_error_invalid_device_id
unwrapError Sp_ErrorW_Cant_Open_Trace_File      = sp_error_cant_open_trace_file
unwrapError Sp_ErrorW_Application_Banned        = sp_error_application_banned
unwrapError Sp_ErrorW_Offline_Too_Many_Tracks   = sp_error_offline_too_many_tracks
unwrapError Sp_ErrorW_Offline_Disk_Cache        = sp_error_offline_disk_cache
unwrapError Sp_ErrorW_Offline_Expired           = sp_error_offline_expired
unwrapError Sp_ErrorW_Offline_Not_Allowed       = sp_error_offline_not_allowed
unwrapError Sp_ErrorW_Offline_License_Lost      = sp_error_offline_license_lost
unwrapError Sp_ErrorW_Offline_License_Error     = sp_error_offline_license_error
unwrapError Sp_ErrorW_Lastfm_Auth_Error         = sp_error_lastfm_auth_error
unwrapError Sp_ErrorW_Invalid_Argument          = sp_error_invalid_argument
unwrapError Sp_ErrorW_System_Failure            = sp_error_system_failure

sp_error_message :: Sp_ErrorW -> String
{-# NOINLINE sp_error_message #-}
sp_error_message = unsafePerformIO . peekCString . c_sp_error_message . unwrapError
