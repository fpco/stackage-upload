{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Network.HTTP.Client                   (Manager, applyBasicAuth,
                                                        brRead, checkStatus,
                                                        newManager, parseUrl,
                                                        requestHeaders,
                                                        responseBody,
                                                        responseStatus,
                                                        withResponse)
import           Network.HTTP.Client.MultipartFormData (formDataBody, partFile)
import           Network.HTTP.Client.TLS               (tlsManagerSettings)
import           Network.HTTP.Types                    (statusCode)
import           System.Directory                      (createDirectoryIfMissing,
                                                        getAppUserDataDirectory,
                                                        removeFile)
import           System.FilePath                       ((</>))
import           System.IO                             (hFlush, hGetEcho,
                                                        hSetEcho, stdin, stdout)

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

newtype HackageCredsSource = HackageCredsSource
    { getCreds :: IO (HackageCreds, FromFile)
    }

type FromFile = Bool

loadCreds :: HackageCredsSource -> IO (HackageCreds, FromFile)
loadCreds = getCreds

saveCreds :: HackageCreds -> IO ()
saveCreds creds = do
    fp <- credsFile
    L.writeFile fp $ encode creds

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
    dir <- getAppUserDataDirectory "stackage-upload"
    createDirectoryIfMissing True dir
    return $ dir </> "credentials.json"

fromFile :: HackageCredsSource
fromFile = HackageCredsSource $ do
    fp <- credsFile
    lbs <- L.readFile fp
    case eitherDecode' lbs of
        Left e -> E.throwIO $ Couldn'tParseJSON fp e
        Right creds -> return (creds, True)

fromMemory :: Text -> Text -> HackageCredsSource
fromMemory u p = HackageCredsSource $ return (HackageCreds
    { hcUsername = u
    , hcPassword = p
    }, False)

data HackageCredsExceptions = Couldn'tParseJSON FilePath String
    deriving (Show, Typeable)
instance E.Exception HackageCredsExceptions

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

mkUploader :: UploadSettings -> IO Uploader
mkUploader us = do
    manager <- usGetManager us
    (creds, fromFile) <- loadCreds $ usCredsSource us
    when (not fromFile && usSaveCreds us) $ saveCreds creds
    req0 <- parseUrl $ usUploadUrl us
    let req1 = req0
            { requestHeaders = [("Accept", "text/plain")]
            , checkStatus = \_ _ _ -> Nothing
            }
    return Uploader
        { upload_ = \fp -> do
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
                    code -> do
                        putStrLn $ "unhandled status code: " ++ show code
                        printBody res
                        error $ "Upload failed on " ++ fp
        }

printBody res =
    loop
  where
    loop = do
        bs <- brRead $ responseBody res
        when (not $ S.null bs) $ do
            S.hPut stdout bs
            loop

data Uploader = Uploader
    { upload_ :: !(FilePath -> IO ())
    }

upload :: Uploader -> FilePath -> IO ()
upload = upload_

data UploadSettings = UploadSettings
    { usUploadUrl   :: !String
    , usGetManager  :: !(IO Manager)
    , usCredsSource :: !HackageCredsSource
    , usSaveCreds   :: !Bool
    }

defaultUploadSettings = UploadSettings
    { usUploadUrl = "https://hackage.haskell.org/packages/"
    , usGetManager = newManager tlsManagerSettings
    , usCredsSource = fromAnywhere
    , usSaveCreds = True
    }

setUploadUrl x us = us { usUploadUrl = x }
setGetManager x us = us { usGetManager = x }
setCredsSource x us = us { usCredsSource = x }
setSaveCreds x us = us { usSaveCreds = x }

handleIO :: (E.IOException -> IO a) -> IO a -> IO a
handleIO = E.handle
