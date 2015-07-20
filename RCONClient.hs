{-# LANGUAGE OverloadedStrings #-}

import           Network.Socket hiding ( send, recv )
import           Network.Socket.ByteString 
import           Network.BSD
import           Control.Monad (liftM)
import           Data.Char
import           Data.List as L
import           Data.Word
import           Data.Bits
import qualified Data.ByteString.Lazy as BL ( unpack, pack )
import qualified Data.ByteString.Lazy.Char8 as B8 ( pack, unpack )
import qualified Data.ByteString as B ( pack, unpack, take, drop, init )
import System.Environment (getArgs)
  
-- | Data constructor for the RCON request packet
data RCONPacket = 
  RCONPacket { size :: Int
             , pktID :: Int
			 , pktType :: Int
			 , payload :: [Word8]
			 } deriving (Show, Eq)
  
main :: IO ()
main = do
  args <- getArgs
  s <- if (args == [])
        then connectTo "127.0.0.1" 25575
        else connectTo (head args) (( read . last ) args)
  (responseType, pId) <- authenticateRCON s
  if (responseType == pId) 
    then putStrLn "Authenticated!! Enter Q to exit!" 
    else putStrLn "Error!! Try again."
  commandProcessor s
  sClose s

-- | Read the user inputs in command line and process them  
commandProcessor :: Socket -> IO ()
commandProcessor socket = do
  msg <- getLine
  let cmd = words msg
  let command = map toLower (head cmd)
  if command == "q"
    then sClose socket
    else do processRCONCommands socket msg 
            commandProcessor socket
      
-- | Authenticate the client with the Minecraft server 
authenticateRCON socket = do
  putStrLn "Enter RCON password: "
  msg <- getLine
  let (authPacket, pId) = buildRCONRequest msg 3
  let authReq = B.pack authPacket
  send socket authReq
  response <- recv socket 4096
  let resType = B.unpack $ B.take 4 (B.drop 4 response)
  return (resType, pId)
   
-- | Process the RCON commands except authentication 
processRCONCommands :: Socket -> String -> IO ()
processRCONCommands socket msg = do
  let (rconPacket, pId) = buildRCONRequest msg 2
  let rconMessage = B.pack rconPacket
  send socket rconMessage
  response <- recv socket 4096
  let initResPayload = B.init $ B.init $ B.drop 12 response             -- get the Body of the RCON response
  let resPayload = B8.unpack $ BL.pack $ B.unpack initResPayload        -- convert from ByteString to String    
  if resPayload == ""
    then putStr ""
    else putStrLn $ show resPayload
   
-- | Establish a connection with the remote server 
connectTo :: String -> Int -> IO Socket
connectTo host port_ = do
      sock <- socket AF_INET Stream defaultProtocol
      addrs <- liftM hostAddresses $ getHostByName host
      if null addrs then error $ "no such host : " ++ host else return ()
      connect sock $ SockAddrInet (toEnum port_) (head addrs)
      return sock
  
-- {
-- Build the RCON request
-- Inputs -> Command/Password :: String
--           Packet type      :: Int
-- Output -> (RCON request packet :: [Word8], Packet ID :: [Word8])
-- } 
buildRCONRequest :: String -> Int -> ([Word8], [Word8])
buildRCONRequest cmd pType = do
  let packet = RCONPacket { size = 8
                          , pktID = 0
					      , pktType = pType
					      , payload = BL.unpack $ B8.pack (cmd ++ "\0\0") }
  let cmdLength = L.length (payload packet)
  let pSize = processData ((size packet) + cmdLength)
  let pId = processData (pktID packet)
  let pType = processData (pktType packet)
  let command = payload packet
  let rconPacket = pSize ++ pId ++ pType ++ command
  (rconPacket, pId)
  where processData s = L.reverse $ toOctets (fromIntegral s :: Word32)

-- | Conversion of Word32 type to [Word8] to construct RCON request
toOctets :: Word32 -> [Word8]
toOctets w = 
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]