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
import qualified Data.ByteString.Char8 as BL8
import qualified Data.ByteString as B ( pack, unpack, take, drop, init )
import           System.Environment ( getArgs )
import           Data.Binary.Strict.Get  

_MAX_RECV_BYTES = 1024  
  
main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  s <- if (args == [])
        then connectTo "127.0.0.1" 25575
        else connectTo (head args) (( read . last ) args)
  (responseType, pId) <- authenticateRCON s
  if (responseType == pId) 
    then do putStrLn "Authenticated!! Enter Q to exit!" 
            commandProcessor s
            sClose s 
    else do putStrLn "Error!! Bailing out..."
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
authenticateRCON :: Socket -> IO ( [Word8], [Word8]) 
authenticateRCON socket = do
  putStrLn "Enter RCON password: "
  msg <- getLine
  let (authPacket, pId) = buildRCONRequest msg 3
  let authReq = B.pack authPacket
  send socket authReq
  response <- recv socket _MAX_RECV_BYTES
  let pResponse = runGet constructResponse response
  resType <- case (fst pResponse) of
                Left err -> return 0
                Right res -> getResType res
  let responseType = L.reverse $ toOctets resType
  return (responseType, pId)
  where getResType (pid, ptype, pbody) = return ptype

-- | Process the RCON commands except authentication 
processRCONCommands :: Socket -> String -> IO ()
processRCONCommands socket msg = do
  let (rconPacket, pId) = buildRCONRequest msg 2
  let rconMessage = B.pack rconPacket
  send socket rconMessage
  response <- recv socket _MAX_RECV_BYTES
  let pResponse = runGet constructResponse response
  let resPayload =  processResponse pResponse
  if resPayload == ""
    then putStr ""
    else putStrLn $ show resPayload
  where processResponse = B.init . B.init . snd 
   
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
buildRCONRequest cmd pktType = (rconPacket, pId)
  where command = BL.unpack $ B8.pack (cmd ++ "\0\0")
        cmdLength = L.length command
        pSize = processData (8 + cmdLength)
        pId = processData 0
        pType = processData pktType
        rconPacket = pSize ++ pId ++ pType ++ command
        processData s = L.reverse $ toOctets (fromIntegral s :: Word32)

-- | Break the response ByteString into fields and construct a tuple
constructResponse :: Get ( Word32, Word32, Word32 )  
constructResponse = do
            pktId <- getWord32le
            pktType <- getWord32le
            pBody <- getWord32le
            return ( pktId, pktType, pBody )     
        
-- | Conversion of Word32 type to [Word8] to construct RCON request
toOctets :: Word32 -> [Word8]
toOctets w = 
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]