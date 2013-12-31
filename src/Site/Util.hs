{-# LANGUAGE CPP #-}

module Site.Util (
  procArgs
) where

procArgs :: String -> [String] -> (String, [String])
#ifndef mingw32_HOST_OS
procArgs cmd args = (cmd, args)
#else
procArgs cmd args = ("cmd.exe", [unwords $ "/c" : cmd : args])
#endif
