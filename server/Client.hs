{-# LANGUAGE OverloadedStrings #-}

import Network (connectTo, PortID(..))
import System.IO (hSetBinaryMode, hPutStrLn, hGetLine, hFlush, hClose, BufferMode(..), hSetBuffering)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy.Char8 as C
import System.Environment

import NetworkActions

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> login
        (x:xs) -> runArg x

runArg :: String -> IO ()
runArg "login" = login
runArg "load" = load
runArg "play" = play
runArg "stop" = stop
runArg _ = error "unsupported arg"

login :: IO ()
login = do
  username <- getLine
  password <- getLine

  handle <- connectTo "0.0.0.0" (PortNumber 3000)

  putStrLn "Sending to server"
  C.hPut handle $ encode (Login (C.pack username) (C.pack password))

  putStrLn "reading from server"
  echoString <- C.hGetContents handle
  putStrLn $ "Received from server: " ++ show echoString

  hClose handle

load :: IO ()
load = do
  link <- getLine

  handle <- connectTo "0.0.0.0" (PortNumber 3000)

  putStrLn "Sending to server"
  C.hPut handle $ encode (Load (C.pack link))

  putStrLn "reading from server"
  echoString <- C.hGetContents handle
  putStrLn $ "Received from server: " ++ show echoString

  hClose handle

play :: IO ()
play = do
  handle <- connectTo "0.0.0.0" (PortNumber 3000)

  putStrLn "Sending to server"
  C.hPut handle $ encode Play

  putStrLn "reading from server"
  echoString <- C.hGetContents handle
  putStrLn $ "Received from server: " ++ show echoString

  hClose handle

stop :: IO ()
stop = do
  handle <- connectTo "0.0.0.0" (PortNumber 3000)

  putStrLn "Sending to server"
  C.hPut handle $ encode Stop

  putStrLn "reading from server"
  echoString <- C.hGetContents handle
  putStrLn $ "Received from server: " ++ show echoString

  hClose handle
