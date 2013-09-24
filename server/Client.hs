{-# LANGUAGE OverloadedStrings #-}

import Network (connectTo, PortID(..))
import System.IO (hSetBinaryMode, hPutStrLn, hGetLine, hFlush, hClose, BufferMode(..), hSetBuffering)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy.Char8 as C

import NetworkActions

main :: IO ()
main = do
  username <- getLine
  password <- getLine

  handle <- connectTo "0.0.0.0" (PortNumber 3000)

  putStrLn "Sending to server"
  C.hPut handle $ encode (Login (C.pack username) (C.pack password))

  putStrLn "reading from server"
  echoString <- C.hGetContents handle
  putStrLn $ "Received from server: " ++ show echoString

  hClose handle

