module Spotify.Session (
      SampleType(..)
    , AudioFormat(..)
    , AudioBufferStats(..)
    , SessionCallbacks(..)
    , SessionConfig(..)
    , sessionCreate
    , sessionLogin
    , processEvents
    , sessionPlayerLoad
    , sessionPlayerPlay
) where

import Bindings.Spotify.Session
import Bindings.Spotify.Struct
import Bindings.Spotify.Error

import Spotify.Struct
import Spotify.Error

import qualified Data.ByteString.Lazy as B
import Foreign.C.String (peekCString, newCString)
import qualified Data.Map.Lazy as M
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Array (peekArray, newArray)
import Foreign.Storable
import Foreign.Marshal.Utils (fromBool, toBool, new, maybeNew)
import Foreign.Marshal.Alloc (malloc)
import Control.Applicative ((<$>), (<*>))
import Foreign.C.Types (CInt)

data SampleType = Int16NativeEndian
    deriving (Show)

-- returns the number of bytes in this sample size
sampleTypeSize :: Num a => SampleType -> a
sampleTypeSize Int16NativeEndian = 2

data AudioFormat = AudioFormat
    { sampleType :: SampleType
    , sampleRate :: Int
    , channels   :: Int
    } deriving (Show)

data AudioBufferStats = AudioBufferStats
    { samples :: Int
    , stutter :: Int
    } deriving (Show)

data SessionCallbacks = SessionCallbacks
    { loggedIn                  :: Session -> Error -> IO ()
    , loggedOut                 :: Session -> IO ()
    , metadataUpdated           :: Session -> IO ()
    , connectionError           :: Session -> Error -> IO ()
    , messageToUser             :: Session -> String -> IO ()
    , notifyMainThread          :: Session -> IO ()
    , musicDelivery             :: Session -> AudioFormat -> B.ByteString -> IO Int
    , playTokenLost             :: Session -> IO ()
    , logMessage                :: Session -> String -> IO ()
    , endOfTrack                :: Session -> IO ()
    , streamingError            :: Session -> Error -> IO ()
    , userinfoUpdated           :: Session -> IO ()
    , startPlayback             :: Session -> IO ()
    , stopPlayback              :: Session -> IO ()
    , getAudioBufferStats       :: Session -> AudioBufferStats -> IO ()
    , offlineStatusUpdated      :: Session -> IO ()
    , offlineError              :: Session -> Error -> IO ()
    , credentialsBlobUpdated    :: Session -> String -> IO ()
    , connectionstateUpdated    :: Session -> IO ()
    , scrobbleError             :: Session -> Error -> IO ()
    , privateSessionModeChanged :: Session -> Bool -> IO ()
    }

data SessionConfig = SessionConfig
    { cacheLocation                :: FilePath
    , settingsLocation             :: FilePath
    , applicationKey               :: B.ByteString
    , userAgent                    :: String
    , callbacks                    :: SessionCallbacks
    , userdata                     :: B.ByteString
    , compressPlaylists            :: Bool
    , dontSaveMetadataForPlaylists :: Bool
    , initiallyUnloadPlaylists     :: Bool
    , deviceId                     :: String
    , proxy                        :: String
    , proxyUsername                :: String
    , proxyPassword                :: String
    , caCertsFilename              :: String
    , tracefile                    :: FilePath
    }

c2hsSampleType :: Sp_SampleType -> SampleType
c2hsSampleType spSampleType = sampleMap M.! spSampleType
  where
    sampleMap = M.fromList
        [ (sp_sampletype_int16_native_endian, Int16NativeEndian)
        ]

c2hsAudioFormat :: Ptr Sp_AudioFormat -> IO AudioFormat
c2hsAudioFormat spAudioFormatPtr = do
    spAudioFormat <- peek spAudioFormatPtr
    let audioFormat = AudioFormat {
          sampleType = c2hsSampleType (sp_sample_type spAudioFormat)
        , sampleRate = fromIntegral (sp_sample_rate spAudioFormat)
        , channels   = fromIntegral (sp_channels spAudioFormat)
        }
    return audioFormat

c2hsAudioBufferStats :: Ptr Sp_Audio_Buffer_Stats -> IO AudioBufferStats
c2hsAudioBufferStats spAudioBufferStatsPtr = do
    spAudioBufferStats <- peek spAudioBufferStatsPtr
    let audioBufferStats = AudioBufferStats {
          samples = fromIntegral (sp_samples spAudioBufferStats)
        , stutter = fromIntegral (sp_stutter spAudioBufferStats)
        }
    return audioBufferStats

hs2cCallbacks :: SessionCallbacks -> IO (Ptr Sp_Session_Callbacks)
hs2cCallbacks sessionCallbacks = do
    cSessionCallbacks <- Sp_Session_Callbacks
        <$> mkLoggedInCb cLoggedIn
        <*> mkLoggedOutCb cLoggedOut
        <*> mkMetadataUpdatedCb cMetadataUpdated
        <*> mkConnectionErrorCb cConnectionError
        <*> mkMessageToUserCb cMessageToUser
        <*> mkNotifyMainThreadCb cNotifyMainThread
        <*> mkMusicDeliveryCb cMusicDelivery
        <*> mkPlayTokenLostCb cPlayTokenLost
        <*> mkLogMessageCb cLogMessage
        <*> mkEndOfTrackCb cEndOfTrack
        <*> mkStreamingErrorCb cStreamingError
        <*> mkUserinfoUpdatedCb cUserinfoUpdated
        <*> mkStartPlaybackCb cStartPlayback
        <*> mkStopPlaybackCb cStopPlayback
        <*> mkGetAudioBufferStatsCb cGetAudioBufferStats
        <*> mkOfflineStatusUpdatedCb cOfflineStatusUpdated
        <*> mkOfflineErrorCb cOfflineError
        <*> mkCredentialsBlobUpdatedCb cCredentialsBlobUpdated
        <*> mkConnectionstateUpdatedCb cConnectionstateUpdated
        <*> mkScrobbleErrorCb cScrobbleError
        <*> mkPrivateSessionModeChangedCb cPrivateSessionModeChanged

    new cSessionCallbacks
  where
    cLoggedIn sessionPtr err = (loggedIn sessionCallbacks) (Session sessionPtr) (wrapError err)
    cLoggedOut sessionPtr = (loggedOut sessionCallbacks) (Session sessionPtr)
    cMetadataUpdated sessionPtr = (metadataUpdated sessionCallbacks) (Session sessionPtr)
    cConnectionError sessionPtr err = (connectionError sessionCallbacks) (Session sessionPtr) (wrapError err)
    cMessageToUser sessionPtr msgPtr = do
        msg <- peekCString msgPtr
        (messageToUser sessionCallbacks) (Session sessionPtr) msg
    cNotifyMainThread sessionPtr = (notifyMainThread sessionCallbacks) (Session sessionPtr)
    cMusicDelivery sessionPtr spAudioFormatPtr frames numFrames = do
        audioFormat <- c2hsAudioFormat spAudioFormatPtr
        let sampleTypeBytes = sampleTypeSize $ sampleType audioFormat
        let numChannels = channels audioFormat
        frameList <- peekArray ((fromIntegral numFrames) * sampleTypeBytes * numChannels) (castPtr frames)
        framesRead <- (musicDelivery sessionCallbacks) (Session sessionPtr) audioFormat (B.pack frameList)
        return . fromIntegral $ framesRead
    cPlayTokenLost sessionPtr = (playTokenLost sessionCallbacks) (Session sessionPtr)
    cLogMessage sessionPtr msgPtr = do
        msg <- peekCString msgPtr
        (logMessage sessionCallbacks) (Session sessionPtr) msg
    cEndOfTrack sessionPtr = (endOfTrack sessionCallbacks) (Session sessionPtr)
    cStreamingError sessionPtr err = (streamingError sessionCallbacks) (Session sessionPtr) (wrapError err)
    cUserinfoUpdated sessionPtr = (userinfoUpdated sessionCallbacks) (Session sessionPtr)
    cStartPlayback sessionPtr = (startPlayback sessionCallbacks) (Session sessionPtr)
    cStopPlayback sessionPtr = (stopPlayback sessionCallbacks) (Session sessionPtr)
    cGetAudioBufferStats sessionPtr spAudioBufferStatsPtr = do
        audioBufferStats <- c2hsAudioBufferStats spAudioBufferStatsPtr
        (getAudioBufferStats sessionCallbacks) (Session sessionPtr) audioBufferStats
    cOfflineStatusUpdated sessionPtr = (offlineStatusUpdated sessionCallbacks) (Session sessionPtr)
    cOfflineError sessionPtr err = (offlineError sessionCallbacks) (Session sessionPtr) (wrapError err)
    cCredentialsBlobUpdated sessionPtr blobPtr = do
        blob <- peekCString blobPtr
        (credentialsBlobUpdated sessionCallbacks) (Session sessionPtr) blob
    cConnectionstateUpdated sessionPtr = (connectionstateUpdated sessionCallbacks) (Session sessionPtr)
    cScrobbleError sessionPtr err = (scrobbleError sessionCallbacks) (Session sessionPtr) (wrapError err)
    cPrivateSessionModeChanged sessionPtr isPrivate = (privateSessionModeChanged sessionCallbacks) (Session sessionPtr) (toBool isPrivate)

hs2cConfig :: SessionConfig -> IO (Ptr Sp_Session_Config)
hs2cConfig config = do
    cCacheLocation <- newCString . cacheLocation $ config
    cSettingsLocation <- newCString . settingsLocation $ config
    cApplicationKey <- newArray . B.unpack . applicationKey $ config
    cUserAgent <- newCString . userAgent $ config
    cCallbacks <- hs2cCallbacks . callbacks $ config
    cUserdata <- newArray . B.unpack . userdata $ config
    cDeviceId <- newCString . deviceId $ config
    cProxy <- newCString . proxy $ config
    cProxyUsername <- newCString . proxyUsername $ config
    cProxyPassword <- newCString . proxyPassword $ config
    cCaCertsFilename <- newCString . caCertsFilename $ config
    cTracefile <- newCString . tracefile $ config
    let session_config = Sp_Session_Config {
          sp_api_version                      = spotify_api_version
        , sp_cache_location                   = cCacheLocation
        , sp_settings_location                = cSettingsLocation
        , sp_application_key                  = castPtr cApplicationKey
        , sp_application_key_size             = fromIntegral . B.length . applicationKey $ config
        , sp_user_agent                       = cUserAgent
        , sp_callbacks                        = cCallbacks
        , sp_userdata                         = castPtr cUserdata
        , sp_compress_playlists               = fromBool . compressPlaylists $ config
        , sp_dont_save_metadata_for_playlists = fromBool . dontSaveMetadataForPlaylists $ config
        , sp_initially_unload_playlists       = fromBool . initiallyUnloadPlaylists $ config
        , sp_device_id                        = cDeviceId
        , sp_proxy                            = cProxy
        , sp_proxy_username                   = cProxyUsername
        , sp_proxy_password                   = cProxyPassword
        , sp_ca_certs_filename                = cCaCertsFilename
        , sp_tracefile                        = cTracefile
        }

    new session_config

sessionCreate :: SessionConfig -> IO (Either Error Session)
sessionCreate sessionConfig = do
    sessionConfigPtr <- hs2cConfig sessionConfig
    sessionPtrPtr <- (malloc :: IO (Ptr (Ptr Sp_Session)))
    err <- c_sp_session_create sessionConfigPtr sessionPtrPtr
    if err == sp_error_ok
        then do
            sessionPtr <- peek sessionPtrPtr
            return $ Right (Session sessionPtr)
        else
            return $ Left (wrapError err)

sessionLogin :: Session -> String -> String -> Bool -> Maybe String -> IO (Maybe Error)
sessionLogin (Session sessionPtr) username password rememberMe blob = do
    cUsername <- newCString username
    cPassword <- newCString password
    cBlob <- maybeNew newCString blob

    err <- c_sp_session_login sessionPtr cUsername cPassword (fromBool rememberMe) cBlob
    if err == sp_error_ok
        then
            return Nothing
        else
            return $ Just (wrapError err)

processEvents :: Session -> IO (Either Error Int)
processEvents (Session sessionPtr) = do
    nextTimeoutPtr <- (malloc :: IO (Ptr CInt))
    err <- c_sp_session_process_events sessionPtr nextTimeoutPtr
    if err == sp_error_ok
        then do
            nextTimeoutMs <- peek nextTimeoutPtr
            return $ Right (fromIntegral nextTimeoutMs)
        else
            return $ Left (wrapError err)

sessionPlayerLoad :: Session -> Track -> IO (Maybe Error)
sessionPlayerLoad (Session sessionPtr) (Track trackPtr) = do
    spError <- c_sp_session_player_load sessionPtr trackPtr
    if spError == sp_error_ok
        then
            return Nothing
        else
            return $ Just (wrapError spError)

sessionPlayerPlay :: Session -> Bool -> IO (Maybe Error)
sessionPlayerPlay (Session sessionPtr) play = do
    spError <- c_sp_session_player_play sessionPtr (fromBool play)
    if spError == sp_error_ok
        then
            return Nothing
        else
            return $ Just (wrapError spError)
