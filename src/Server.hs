{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network
import qualified Data.ByteString as B
import Control.Concurrent.STM
import qualified Data.Map as M
import System.Environment (getArgs)
import System.IO
import Control.Concurrent (forkIO)
import Control.Applicative ((<$>), (<|>), empty)
import Data.Attoparsec

-- PDU: Portable Data Unit
data PDU = Word16 B.ByteString deriving (Show, Eq)

data Action =
      Connect
    | Disconnect
    | Pipeline [Command]
    | Command deriving (Show, Eq)

data Command =
      Get PDU
    | Set PDU PDU
    | DeleteCommand PDU deriving (Show, Eq)

newtype State = State {state :: M.Map B.ByteString (TVar B.ByteString)}

get :: State -> B.ByteString -> Maybe B.ByteString
get m k = undefined

set :: State -> B.ByteString -> B.ByteString -> State
set m k v = undefined

delete :: State -> B.ByteString -> State
delete = undefined


actions :: [B.ByteString]
actions = ["connect", "disconnect", "pipeline"]

commands :: [B.ByteString]
commands = ["get", "set", "delete"]

action = foldr ((<|>) . string) empty actions

commandProcessor :: Handle -> IO ()
commandProcessor _ = undefined

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

parsePort :: [String] -> Maybe Int
parsePort [] = Nothing
parsePort (x:_) = Just (read x :: Int)

getSocket :: Maybe Int -> IO Socket
getSocket Nothing = error "usage: Server <port>"
getSocket (Just port) = listenOn $ PortNumber (fromIntegral port)

main :: IO ()
main = parsePort <$> getArgs >>= getSocket >>= sockHandler