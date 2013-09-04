module Bindings.Spotify.Error
where

-- hiding unsafePerformIO in Foreign since it is deprecated
import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

#include <libspotify/api.h>

newtype Sp_Error = Sp_Error { unSp_Error :: CInt }

#{enum Sp_Error, Sp_Error
  , sp_error_ok                        = SP_ERROR_OK
  , sp_error_bad_api_version           = SP_ERROR_BAD_API_VERSION
  , sp_error_api_initialization_failed = SP_ERROR_API_INITIALIZATION_FAILED
  , sp_error_track_not_playable        = SP_ERROR_TRACK_NOT_PLAYABLE
  , sp_error_bad_application_key       = SP_ERROR_BAD_APPLICATION_KEY
  , sp_error_bad_username_or_password  = SP_ERROR_BAD_USERNAME_OR_PASSWORD
  , sp_error_user_banned               = SP_ERROR_USER_BANNED
  , sp_error_unable_to_contact_server  = SP_ERROR_UNABLE_TO_CONTACT_SERVER
  , sp_error_client_too_old            = SP_ERROR_CLIENT_TOO_OLD
  , sp_error_other_permanent           = SP_ERROR_OTHER_PERMANENT
  , sp_error_bad_user_agent            = SP_ERROR_BAD_USER_AGENT
  , sp_error_missing_callback          = SP_ERROR_MISSING_CALLBACK
  , sp_error_invalid_indata            = SP_ERROR_INVALID_INDATA
  , sp_error_index_out_of_range        = SP_ERROR_INDEX_OUT_OF_RANGE
  , sp_error_user_needs_premium        = SP_ERROR_USER_NEEDS_PREMIUM
  , sp_error_other_transient           = SP_ERROR_OTHER_TRANSIENT
  , sp_error_is_loading                = SP_ERROR_IS_LOADING
  , sp_error_no_stream_available       = SP_ERROR_NO_STREAM_AVAILABLE
  , sp_error_permission_denied         = SP_ERROR_PERMISSION_DENIED
  , sp_error_inbox_is_full             = SP_ERROR_INBOX_IS_FULL
  , sp_error_no_cache                  = SP_ERROR_NO_CACHE
  , sp_error_no_such_user              = SP_ERROR_NO_SUCH_USER
  , sp_error_no_credentials            = SP_ERROR_NO_CREDENTIALS
  , sp_error_network_disabled          = SP_ERROR_NETWORK_DISABLED
  , sp_error_invalid_device_id         = SP_ERROR_INVALID_DEVICE_ID
  , sp_error_cant_open_trace_file      = SP_ERROR_CANT_OPEN_TRACE_FILE
  , sp_error_application_banned        = SP_ERROR_APPLICATION_BANNED
  , sp_error_offline_too_many_tracks   = SP_ERROR_OFFLINE_TOO_MANY_TRACKS
  , sp_error_offline_disk_cache        = SP_ERROR_OFFLINE_DISK_CACHE
  , sp_error_offline_expired           = SP_ERROR_OFFLINE_EXPIRED
  , sp_error_offline_not_allowed       = SP_ERROR_OFFLINE_NOT_ALLOWED
  , sp_error_offline_license_lost      = SP_ERROR_OFFLINE_LICENSE_LOST
  , sp_error_offline_license_error     = SP_ERROR_OFFLINE_LICENSE_ERROR
  , sp_error_lastfm_auth_error         = SP_ERROR_LASTFM_AUTH_ERROR
  , sp_error_invalid_argument          = SP_ERROR_INVALID_ARGUMENT
  , sp_error_system_failure            = SP_ERROR_SYSTEM_FAILURE
  }

foreign import ccall "libspotify/api.h sp_error_message"
  c_sp_error_message :: Sp_Error -> CString

sp_error_message :: Sp_Error -> String
sp_error_message = unsafePerformIO . peekCString . c_sp_error_message
