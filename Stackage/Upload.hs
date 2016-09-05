{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provide ability to upload tarballs to Hackage.
module Stackage.Upload
    ( -- * Upload
      mkUploader
    , Uploader
    , upload
    , UploadSettings
    , defaultUploadSettings
    , setUploadUrl
    , setGetManager
    , setCredsSource
    , setSaveCreds
      -- * Credentials
    , HackageCreds
    , loadCreds
    , saveCreds
    , FromFile
      -- ** Credentials source
    , HackageCredsSource
    , fromAnywhere
    , fromPrompt
    , fromFile
    , fromMemory
    ) where

import           Control.Applicative                   ((<$>), (<*>))
import           Control.Exception                     (bracket)
import qualified Control.Exception                     as E
import           Control.Monad                         (when)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..),
                                                        eitherDecode', encode,
                                                        object, withObject,
                                                        (.:), (.=))
import qualified Data.ByteString.Char8                 as S
import qualified Data.ByteString.Lazy                  as L
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)
import qualified Data.Text.IO                          as TIO
import           Data.Typeable                         (Typeable)
import           Network.HTTP.Client                   (BodyReader, Manager,
                                                        Response,
                                                        applyBasicAuth, brRead,
#if MIN_VERSION_http_client(0,5,0)
                                                        checkResponse,
#else
                                                        checkStatus,
#endif
                                                        newManager,
#if MIN_VERSION_http_client(0,4,30)
                                                        parseUrlThrow,
#else
                                                        parseUrl,
#endif
                                                        requestHeaders,
                                                        responseBody,
                                                        responseStatus,
                                                        withResponse)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partFile)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.HTTP.Types                    (statusCode)
import           System.Directory                      (createDirectoryIfMissing,
                                                        doesDirectoryExist,
                                                        doesFileExist,
                                                        getAppUserDataDirectory,
                                                        getDirectoryContents, removeDirectoryRecursive,
                                                        removeFile)
import           System.Exit                           (ExitCode (ExitSuccess))
import           System.FilePath                       (takeExtension, (</>))
import           System.IO                             (hClose, hFlush,
                                                        hGetEcho, hSetEcho,
                                                        stdin, stdout)
import           System.IO.Temp                        (withSystemTempDirectory)
import           System.Process                        (StdStream (CreatePipe),
                                                        createProcess, cwd,
                                                        proc, std_in,
                                                        waitForProcess)

-- | Username and password to log into Hackage.
--
-- Since 0.1.0.0
data HackageCreds = HackageCreds
    { hcUsername :: !Text
    , hcPassword :: !Text
    }
    deriving Show

instance ToJSON HackageCreds where
    toJSON (HackageCreds u p) = object
        [ "username" .= u
        , "password" .= p
        ]
instance FromJSON HackageCreds where
    parseJSON = withObject "HackageCreds" $ \o -> HackageCreds
        <$> o .: "username"
        <*> o .: "password"

-- | A source for getting Hackage credentials.
--
-- Since 0.1.0.0
newtype HackageCredsSource = HackageCredsSource
    { getCreds :: IO (HackageCreds, FromFile)
    }

-- | Whether the Hackage credentials were loaded from a file.
--
-- This information is useful since, typically, you only want to save the
-- credentials to a file if it wasn't already loaded from there.
--
-- Since 0.1.0.0
type FromFile = Bool

-- | Load Hackage credentials from the given source.
--
-- Since 0.1.0.0
loadCreds :: HackageCredsSource -> IO (HackageCreds, FromFile)
loadCreds = getCreds

-- | Save the given credentials to the credentials file.
--
-- Since 0.1.0.0
saveCreds :: HackageCreds -> IO ()
saveCreds creds = do
    fp <- credsFile
    L.writeFile fp $ encode creds

-- | Load the Hackage credentials from the prompt, asking the user to type them
-- in.
--
-- Since 0.1.0.0
fromPrompt :: HackageCredsSource
fromPrompt = HackageCredsSource $ do
    putStr "Hackage username: "
    hFlush stdout
    username <- TIO.getLine
    password <- promptPassword
    return (HackageCreds
        { hcUsername = username
        , hcPassword = password
        }, False)

credsFile :: IO FilePath
credsFile = do
    olddir <- getAppUserDataDirectory "stackage-upload"
    exists <- doesDirectoryExist olddir
    when exists $ do
        putStrLn $ "Removing old config directory: " ++ olddir
        removeDirectoryRecursive olddir

    dir <- fmap (</> "upload") $ getAppUserDataDirectory "stackage"
    createDirectoryIfMissing True dir
    return $ dir </> "credentials.json"

-- | Load the Hackage credentials from the JSON config file.
--
-- Since 0.1.0.0
fromFile :: HackageCredsSource
fromFile = HackageCredsSource $ do
    fp <- credsFile
    lbs <- L.readFile fp
    case eitherDecode' lbs of
        Left e -> E.throwIO $ Couldn'tParseJSON fp e
        Right creds -> return (creds, True)

-- | Load the Hackage credentials from the given arguments.
--
-- Since 0.1.0.0
fromMemory :: Text -> Text -> HackageCredsSource
fromMemory u p = HackageCredsSource $ return (HackageCreds
    { hcUsername = u
    , hcPassword = p
    }, False)

data HackageCredsExceptions = Couldn'tParseJSON FilePath String
    deriving (Show, Typeable)
instance E.Exception HackageCredsExceptions

-- | Try to load the credentials from the config file. If that fails, ask the
-- user to enter them.
--
-- Since 0.1.0.0
fromAnywhere = HackageCredsSource $
    getCreds fromFile `E.catches`
        [ E.Handler $ \(_ :: E.IOException) -> getCreds fromPrompt
        , E.Handler $ \(_ :: HackageCredsExceptions) -> getCreds fromPrompt
        ]

-- | Lifted from cabal-install, Distribution.Client.Upload
promptPassword :: IO Text
promptPassword = do
  putStr "Hackage password: "
  hFlush stdout
  -- save/restore the terminal echoing status
  passwd <- bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
    hSetEcho stdin False  -- no echoing for entering the password
    fmap T.pack getLine
  putStrLn ""
  return passwd

-- | Turn the given settings into an @Uploader@.
--
-- Since 0.1.0.0
mkUploader :: UploadSettings -> IO Uploader
mkUploader us = do
    manager <- usGetManager us
    (creds, fromFile) <- loadCreds $ usCredsSource us
    when (not fromFile && usSaveCreds us) $ saveCreds creds
#if MIN_VERSION_http_client(0,4,30)
    req0 <- parseUrlThrow $ usUploadUrl us
#else
    req0 <- parseUrl $ usUploadUrl us
#endif
    let req1 = req0
            { requestHeaders = [("Accept", "text/plain")]
#if MIN_VERSION_http_client(0,5,0)
            , checkResponse = \_ _ -> return ()
#else
            , checkStatus = \_ _ _ -> Nothing
#endif
            }
    return Uploader
        { upload_ = \fp0 -> withTarball fp0 $ \fp -> do
            let formData = [partFile "package" fp]
            req2 <- formDataBody formData req1
            let req3 = applyBasicAuth
                    (encodeUtf8 $ hcUsername creds)
                    (encodeUtf8 $ hcPassword creds)
                    req2
            putStr $ "Uploading " ++ fp ++ "... "
            hFlush stdout
            withResponse req3 manager $ \res ->
                case statusCode $ responseStatus res of
                    200 -> putStrLn "done!"
                    401 -> do
                        putStrLn "authentication failure"
                        cfp <- credsFile
                        handleIO (const $ return ()) (removeFile cfp)
                        error $ "Authentication failure uploading to server"
                    403 -> do
                        putStrLn "forbidden upload"
                        putStrLn "Usually means: you've already uploaded this package/version combination"
                        putStrLn "Ignoring error and continuing, full message from Hackage below:\n"
                        printBody res
                    503 -> do
                        putStrLn "service unavailable"
                        putStrLn "This error some times gets sent even though the upload succeeded"
                        putStrLn "Check on Hackage to see if your pacakge is present"
                        printBody res
                    code -> do
                        putStrLn $ "unhandled status code: " ++ show code
                        printBody res
                        error $ "Upload failed on " ++ fp
        }

-- | Given either a file, return it. Given a directory, run @cabal sdist@ and
-- get the resulting tarball.
withTarball :: FilePath -> (FilePath -> IO a) -> IO a
withTarball fp0 inner = do
    isFile <- doesFileExist fp0
    if isFile then inner fp0 else withSystemTempDirectory "stackage-upload-tarball" $ \dir -> do
        isDir <- doesDirectoryExist fp0
        when (not isDir) $ error $ "Invalid argument: " ++ fp0

        (Just h, Nothing, Nothing, ph) <-
            createProcess $ (proc "cabal" ["sdist", "--builddir=" ++ dir])
                { cwd = Just fp0
                , std_in = CreatePipe
                }
        hClose h
        ec <- waitForProcess ph
        when (ec /= ExitSuccess) $
            error $ "Could not create tarball for " ++ fp0
        contents <- getDirectoryContents dir
        case filter ((== ".gz") . takeExtension) contents of
            [x] -> inner (dir </> x)
            _ -> error $ "Unexpected directory contents after cabal sdist: " ++ show contents

printBody :: Response BodyReader -> IO ()
printBody res =
    loop
  where
    loop = do
        bs <- brRead $ responseBody res
        when (not $ S.null bs) $ do
            S.hPut stdout bs
            loop

-- | The computed value from a @UploadSettings@.
--
-- Typically, you want to use this with 'upload'.
--
-- Since 0.1.0.0
data Uploader = Uploader
    { upload_ :: !(FilePath -> IO ())
    }

-- | Upload a single tarball with the given @Uploader@.
--
-- Since 0.1.0.0
upload :: Uploader -> FilePath -> IO ()
upload = upload_

-- | Settings for creating an @Uploader@.
--
-- Since 0.1.0.0
data UploadSettings = UploadSettings
    { usUploadUrl   :: !String
    , usGetManager  :: !(IO Manager)
    , usCredsSource :: !HackageCredsSource
    , usSaveCreds   :: !Bool
    }

-- | Default value for @UploadSettings@.
--
-- Use setter functions to change defaults.
--
-- Since 0.1.0.0
defaultUploadSettings :: UploadSettings
defaultUploadSettings = UploadSettings
    { usUploadUrl = "https://hackage.haskell.org/packages/"
    , usGetManager = newManager tlsManagerSettings
    , usCredsSource = fromAnywhere
    , usSaveCreds = True
    }

-- | Change the upload URL.
--
-- Default: "https://hackage.haskell.org/packages/"
--
-- Since 0.1.0.0
setUploadUrl :: String -> UploadSettings -> UploadSettings
setUploadUrl x us = us { usUploadUrl = x }

-- | How to get an HTTP connection manager.
--
-- Default: @newManager tlsManagerSettings@
--
-- Since 0.1.0.0
setGetManager :: IO Manager -> UploadSettings -> UploadSettings
setGetManager x us = us { usGetManager = x }

-- | How to get the Hackage credentials.
--
-- Default: @fromAnywhere@
--
-- Since 0.1.0.0
setCredsSource :: HackageCredsSource -> UploadSettings -> UploadSettings
setCredsSource x us = us { usCredsSource = x }

-- | Save new credentials to the config file.
--
-- Default: @True@
--
-- Since 0.1.0.0
setSaveCreds :: Bool -> UploadSettings -> UploadSettings
setSaveCreds x us = us { usSaveCreds = x }

handleIO :: (E.IOException -> IO a) -> IO a -> IO a
handleIO = E.handle
