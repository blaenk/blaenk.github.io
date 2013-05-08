module Site.ShellFilter (shellFilter) where

import           Control.Concurrent   (forkIO)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import           System.IO            (Handle, hClose, hGetContents, hPutStr,
                                       hSetEncoding, localeEncoding)
import           System.Process


--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler

shellFilter :: String           -- ^ Program name
           -> String           -- ^ Program input
           -> Compiler String  -- ^ Program output
shellFilter = shellFilterWith writer reader
  where
    writer handle input = do
        hSetEncoding handle localeEncoding
        hPutStr handle input
    reader handle = do
        hSetEncoding handle localeEncoding
        hGetContents handle


--------------------------------------------------------------------------------
-- | Variant of 'unixFilter' that should be used for binary files
--
-- > match "music.wav" $ do
-- >     route   $ setExtension "ogg"
-- >     compile $ getResourceLBS >>= withItemBody (unixFilter "oggenc" ["-"])
shellFilterLBS :: String               -- ^ Program name
              -> ByteString           -- ^ Program input
              -> Compiler ByteString  -- ^ Program output
shellFilterLBS = shellFilterWith LB.hPutStr LB.hGetContents


--------------------------------------------------------------------------------
-- | Overloaded compiler
shellFilterWith :: (Handle -> i -> IO ())  -- ^ Writer
               -> (Handle -> IO o)        -- ^ Reader
               -> String                  -- ^ Program name
               -> i                       -- ^ Program input
               -> Compiler o              -- ^ Program output
shellFilterWith writer reader shell_cmd input = do
    debugCompiler ("Executing external program " ++ shell_cmd)
    unsafeCompiler $ shellFilterIO writer reader shell_cmd input


--------------------------------------------------------------------------------
-- | Internally used function
shellFilterIO :: (Handle -> i -> IO ())
             -> (Handle -> IO o)
             -> String
             -> i
             -> IO o
shellFilterIO writer reader shell_cmd input = do
    let process = (shell shell_cmd)
            { std_in    = CreatePipe
            , std_out   = CreatePipe
            , close_fds = True
            }

    (Just stdinWriteHandle, Just stdoutReadHandle, _, _) <-
        createProcess process

    -- Write the input to the child pipe
    _ <- forkIO $ do
        writer stdinWriteHandle input
        hClose stdinWriteHandle

    -- Receive the output from the child
    reader stdoutReadHandle