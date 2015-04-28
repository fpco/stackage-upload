{-# LANGUAGE TemplateHaskell #-}
import Options.Applicative   (argument, empty, metavar, some, str)
import Paths_stackage_upload (version)
import Stackage.CLI          (simpleOptions, simpleVersion)
import Stackage.Upload

main :: IO ()
main = do
    (files, ()) <- simpleOptions
        $(simpleVersion version)
        "Secure upload of packages to Hackage"
        "Specifying a directory will cause cabal sdist to be run in that directory"
        options
        empty
    uploader <- mkUploader defaultUploadSettings
    mapM_ (upload uploader) files
  where
    options = some (argument str (metavar "TARBALLS/DIRECTORIES..."))
