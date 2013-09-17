module Spotify.Session (
      Version(..)
    , SampleType(..)
    , AudioFormat(..)
    , AudioBufferStats(..)
    , SessionCallbacks(..)
    , SessionConfig(..)
--    , sessionCreate
) where

import Bindings.Spotify.Session

import Spotify.Struct
import Spotify.Error

import Data.Int
import qualified Data.ByteString.Lazy as B
import Foreign.C.String (peekCString, newCString)
import qualified Data.Map.Lazy as M
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Array (peekArray, newArray)
import Foreign.Storable
import Foreign.Marshal.Utils (fromBool, toBool, new)
import Control.Applicative ((<$>), (<*>))

newtype Version = Version { unVersion :: Int }
    deriving (Show)

spotifyApiVersion :: Version
spotifyApiVersion = Version (fromIntegral spotify_api_version)

data SampleType = Int16NativeEndian
    deriving (Show)

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
    { apiVersion                   :: Version
    , cacheLocation                :: FilePath
    , settingsLocation             :: FilePath
    , applicationKey               :: B.ByteString
    , applicationKeySize           :: Int64
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
hs2cCallbacks callbacks = do
    session_callbacks <- Sp_Session_Callbacks
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

    new session_callbacks
  where
    cLoggedIn sessionPtr err = (loggedIn callbacks) (Session sessionPtr) (wrapError err)
    cLoggedOut sessionPtr = (loggedOut callbacks) (Session sessionPtr)
    cMetadataUpdated sessionPtr = (metadataUpdated callbacks) (Session sessionPtr)
    cConnectionError sessionPtr err = (connectionError callbacks) (Session sessionPtr) (wrapError err)
    cMessageToUser sessionPtr msgPtr = do
        msg <- peekCString msgPtr
        (messageToUser callbacks) (Session sessionPtr) msg
    cNotifyMainThread sessionPtr = (notifyMainThread callbacks) (Session sessionPtr)
    cMusicDelivery sessionPtr spAudioFormatPtr frames numFrames = do
        audioFormat <- c2hsAudioFormat spAudioFormatPtr
        frameList <- peekArray (fromIntegral numFrames) (castPtr frames)
        framesRead <- (musicDelivery callbacks) (Session sessionPtr) audioFormat (B.pack frameList)
        return . fromIntegral $ framesRead
    cPlayTokenLost sessionPtr = (playTokenLost callbacks) (Session sessionPtr)
    cLogMessage sessionPtr msgPtr = do
        msg <- peekCString msgPtr
        (logMessage callbacks) (Session sessionPtr) msg
    cEndOfTrack sessionPtr = (endOfTrack callbacks) (Session sessionPtr)
    cStreamingError sessionPtr err = (streamingError callbacks) (Session sessionPtr) (wrapError err)
    cUserinfoUpdated sessionPtr = (userinfoUpdated callbacks) (Session sessionPtr)
    cStartPlayback sessionPtr = (startPlayback callbacks) (Session sessionPtr)
    cStopPlayback sessionPtr = (stopPlayback callbacks) (Session sessionPtr)
    cGetAudioBufferStats sessionPtr spAudioBufferStatsPtr = do
        audioBufferStats <- c2hsAudioBufferStats spAudioBufferStatsPtr
        (getAudioBufferStats callbacks) (Session sessionPtr) audioBufferStats
    cOfflineStatusUpdated sessionPtr = (offlineStatusUpdated callbacks) (Session sessionPtr)
    cOfflineError sessionPtr err = (offlineError callbacks) (Session sessionPtr) (wrapError err)
    cCredentialsBlobUpdated sessionPtr blobPtr = do
        blob <- peekCString blobPtr
        (credentialsBlobUpdated callbacks) (Session sessionPtr) blob
    cConnectionstateUpdated sessionPtr = (connectionstateUpdated callbacks) (Session sessionPtr)
    cScrobbleError sessionPtr err = (scrobbleError callbacks) (Session sessionPtr) (wrapError err)
    cPrivateSessionModeChanged sessionPtr isPrivate = (privateSessionModeChanged callbacks) (Session sessionPtr) (toBool isPrivate)

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
          sp_api_version                      = fromIntegral . unVersion . apiVersion $ config
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
