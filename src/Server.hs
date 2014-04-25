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
import qualified Data.Attoparsec as A
import qualified Data.Serialize as S

-- PDU: Portable Data Unit
data PDU = Word16 B.ByteString deriving (Show, Eq)

newtype State = State {state :: M.Map B.ByteString (TVar B.ByteString)}

get :: State -> B.ByteString -> Maybe B.ByteString
get m k = undefined

set :: State -> B.ByteString -> B.ByteString -> State
set m k v = undefined

delete :: State -> B.ByteString -> State
delete = undefined


commands :: [B.ByteString]
commands = ["get", "set", "delete"]

action = foldr ((<|>) . A.string) empty commands

parsePDU :: B.ByteString -> A.Result B.ByteString
parsePDU msg =
  case A.parse (A.take 2) msg of
      A.Fail{} -> error "blah"
      A.Done rest n ->
          case S.runGet S.getWord16le n of
              Left{} -> error "blah blah"
              Right len ->
                  A.parse parser rest
                  where parser = A.take (fromIntegral len)

data PartialCommand =
      Get B.ByteString
    | Set B.ByteString
    | Delete B.ByteString deriving (Show, Eq)

parseCommand :: B.ByteString -> Maybe PartialCommand
parseCommand msg =
    case A.parse action msg of
      A.Done rest "get" -> Just $ Get rest
      A.Done rest "set" -> Just $ Set rest
      A.Done rest "delete" -> Just $ Delete rest
      A.Fail{} -> Nothing

data Command =
      TGet B.ByteString
    | TSet B.ByteString
    | TDelete B.ByteString deriving (Show, Eq)

parseCommandData :: PartialCommand -> Command
parseCommandData (Get _) = undefined
parseCommandData (Set _) = undefined
parseCommandData (Delete _) = undefined

commandProcessor :: Handle -> IO ()
commandProcessor h = do
    _ <- B.hGetLine h
    commandProcessor h

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ commandProcessor handle
    sockHandler sock

runServer :: Int -> IO ()
runServer port = do
  sock <- getSocket (Just port)
  sockHandler sock

parsePort :: [String] -> Maybe Int
parsePort [] = Nothing
parsePort (x:_) = Just (read x :: Int)

getSocket :: Maybe Int -> IO Socket
getSocket Nothing = error "usage: Server <port>"
getSocket (Just port) = listenOn $ PortNumber (fromIntegral port)

main :: IO ()
main = parsePort <$> getArgs >>= getSocket >>= sockHandler