{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "SDL2/SDL_net.h"
module SDL.Raw.Net
-- General
( init
, quit
, write16
, write32
, read16
, read32

-- Name Resolution
, resolveHost
, resolveIP

-- TCP Sockets
, tcpOpen
, tcpClose
, tcpAccept
, tcpGetPeerAddress
, tcpSend
, tcpRecv

-- UDP Sockets
, udpOpen
, udpClose
, udpBind
, udpUnbind
, getPeerAddress
, udpSend
, udpRecv
, udpSendV
, udpRecvV

-- UDP Packets
, allocPacket
, resizePacket
, freePacket
, allocPacketV
, freePacketV

-- Socket Sets
, allocSocketSet
, freeSocketSet
--, addSocket
, tcpAddSocket
, udpAddSocket
--, delSocket
, tcpDelSocket
, udpDelSocket
, checkSockets
--, socketReady

, module SDL.Raw.Net.Enum
, module SDL.Raw.Net.Types
) where

import Control.Monad.IO.Class
import Data.Word
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (init)
import SDL.Raw hiding (init,quit)

-- Internal Imports
import SDL.Raw.Net.Enum
import SDL.Raw.Net.Types

foreign import ccall unsafe "SDLNet_Init" initFFI :: IO CInt
foreign import ccall unsafe "SDLNet_Quit" quitFFI :: IO ()
foreign import ccall unsafe "SDLNet_Write16" write16FFI :: Word16 -> Ptr () -> IO ()
foreign import ccall unsafe "SDLNet_Write32" write32FFI :: Word32 -> Ptr () -> IO ()
foreign import ccall unsafe "SDLNet_Read16" read16FFI :: Ptr () -> IO Word16
foreign import ccall unsafe "SDLNet_Read32" read32FFI :: Ptr () -> IO Word32
foreign import ccall unsafe "SDLNet_ResolveHost" resolveHostFFI :: Ptr IPAddress -> CString -> Word16 -> IO CInt
foreign import ccall unsafe "SDLNet_ResolveIP" resolveIPFFI :: Ptr IPAddress -> IO CString
foreign import ccall unsafe "SDLNet_TCP_Open" tcpOpenFFI :: Ptr IPAddress -> IO TCPSocket
foreign import ccall unsafe "SDLNet_TCP_Close" tcpCloseFFI :: TCPSocket -> IO ()
foreign import ccall unsafe "SDLNet_TCP_Accept" tcpAcceptFFI :: TCPSocket -> IO TCPSocket
foreign import ccall unsafe "SDLNet_TCP_GetPeerAddress" tcpGetPeerAddressFFI :: TCPSocket -> IO (Ptr IPAddress)
foreign import ccall unsafe "SDLNet_TCP_Send" tcpSendFFI :: TCPSocket -> Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "SDLNet_TCP_Recv" tcpRecvFFI :: TCPSocket -> Ptr () -> CInt -> IO CInt
foreign import ccall unsafe "SDLNet_UDP_Open" udpOpenFFI :: Word16 -> IO UDPSocket
foreign import ccall unsafe "SDLNet_UDP_Close" udpCloseFFI :: UDPSocket -> IO ()
foreign import ccall unsafe "SDLNet_UDP_Bind" udpBindFFI :: UDPSocket -> CInt -> Ptr IPAddress -> IO CInt
foreign import ccall unsafe "SDLNet_UDP_Unbind" udpUnbindFFI :: UDPSocket -> CInt -> IO ()
foreign import ccall unsafe "SDLNet_UDP_GetPeerAddress" getPeerAddressFFI :: UDPSocket -> CInt -> IO (Ptr IPAddress)
foreign import ccall unsafe "SDLNet_UDP_Send" udpSendFFI :: UDPSocket -> CInt -> Ptr UDPPacket -> IO CInt
foreign import ccall unsafe "SDLNet_UDP_Recv" udpRecvFFI :: UDPSocket -> Ptr UDPPacket -> IO CInt
foreign import ccall unsafe "SDLNet_UDP_SendV" udpSendVFFI :: UDPSocket -> Ptr UDPPacket -> CInt -> IO CInt
foreign import ccall unsafe "SDLNet_UDP_RecvV" udpRecvVFFI :: UDPSocket -> Ptr UDPPacket -> IO CInt
foreign import ccall unsafe "SDLNet_AllocPacket" allocPacketFFI :: CInt -> IO (Ptr UDPPacket)
foreign import ccall unsafe "SDLNet_ResizePacket" resizePacketFFI :: Ptr UDPPacket -> CInt -> IO CInt
foreign import ccall unsafe "SDLNet_FreePacket" freePacketFFI :: Ptr UDPPacket -> IO ()
foreign import ccall unsafe "SDLNet_AllocPacketV" allocPacketVFFI :: CInt -> CInt -> IO (Ptr UDPPacket)
foreign import ccall unsafe "SDLNet_FreePacketV" freePacketVFFI :: Ptr UDPPacket -> IO ()
foreign import ccall unsafe "SDLNet_AllocSocketSet" allocSocketSetFFI :: CInt -> IO SocketSet
foreign import ccall unsafe "SDLNet_FreeSocketSet" freeSocketSetFFI :: SocketSet -> IO ()
--foreign import ccall unsafe "SDLNet_AddSocket" addSocketFFI :: SocketSet -> GenericSocket -> IO CInt
foreign import ccall unsafe "SDLNet_TCP_AddSocket" tcpAddSocketFFI :: SocketSet -> TCPSocket -> IO CInt
foreign import ccall unsafe "SDLNet_UDP_AddSocket" udpAddSocketFFI :: SocketSet -> UDPSocket -> IO CInt
--foreign import ccall unsafe "SDLNet_DelSocket" delSocketFFI :: SocketSet -> GenericSet -> IO CInt
foreign import ccall unsafe "SDLNet_TCP_DelSocket" tcpDelSocketFFI :: SocketSet -> TCPSocket -> IO CInt
foreign import ccall unsafe "SDLNet_UDP_DelSocket" udpDelSocketFFI :: SocketSet -> UDPSocket -> IO CInt
foreign import ccall unsafe "SDLNet_CheckSockets" checkSocketsFFI :: SocketSet -> Word32 -> IO CInt
--foreign import ccall unsafe "SDLNet_SocketReady" socketReadyFFI :: GenericSocket -> IO CInt


-- | Initialize the network API.
-- This must be called before using other functions in this library.
-- SDL must be initialized before this call.
--
-- __Returns:__ 0 on success, -1 on errors
init :: MonadIO m => m CInt
init = liftIO initFFI
{-# INLINE init #-}


-- | Shutdown and cleanup the network API.
-- After calling this all sockets are closed, and the SDL_net functions should
-- not be used. You may, of course, use SDLNet_Init to use the functionality
-- again.
quit :: MonadIO m => m ()
quit = liftIO quitFFI
{-# INLINE quit #-}


-- | Put the 16bit (a short on 32bit systems) value into the data buffer area 
-- in network byte order. This helps avoid byte order differences between two 
-- systems that are talking over the network. The value can be a signed number, 
-- the unsigned parameter type doesn't affect the data. The area pointer need 
-- not be at the beginning of a buffer, but must have at least 2 bytes of space 
-- left, including the byte currently pointed at.
write16 :: MonadIO m => Word16 -> Ptr () -> m ()
write16 v1 v2 = liftIO $ write16 v1 v2
{-# INLINE write16 #-}


write32 :: MonadIO m => Word32 -> Ptr () -> m ()
write32 v1 v2 = liftIO $ write32FFI v1 v2
{-# INLINE write32 #-}


read16 :: MonadIO m => Ptr () -> m Word16
read16 v1 = liftIO $ read16FFI v1
{-# INLINE read16 #-}


read32 :: MonadIO m => Ptr () -> m Word32
read32 v1 = liftIO $ read32FFI v1
{-# INLINE read32 #-}


resolveHost :: MonadIO m => Ptr IPAddress -> CString -> Word16 -> m CInt
resolveHost v1 v2 v3 = liftIO $ resolveHostFFI v1 v2 v3
{-# INLINE resolveHost #-}


resolveIP :: MonadIO m => Ptr IPAddress -> m CString
resolveIP v1 = liftIO $ resolveIPFFI v1
{-# INLINE resolveIP #-}


tcpOpen :: MonadIO m => Ptr IPAddress -> m TCPSocket
tcpOpen v1 = liftIO $ tcpOpenFFI v1
{-# INLINE tcpOpen #-}


tcpClose :: MonadIO m => TCPSocket -> m ()
tcpClose v1 = liftIO $ tcpCloseFFI v1
{-# INLINE tcpClose #-}


tcpAccept :: MonadIO m => TCPSocket -> m TCPSocket
tcpAccept v1 = liftIO $ tcpAcceptFFI v1
{-# INLINE tcpAccept #-}


tcpGetPeerAddress :: MonadIO m => TCPSocket -> m (Ptr IPAddress)
tcpGetPeerAddress v1 = liftIO $ tcpGetPeerAddressFFI v1
{-# INLINE tcpGetPeerAddress #-}


tcpSend :: MonadIO m => TCPSocket -> Ptr () -> CInt -> m CInt
tcpSend v1 v2 v3 = liftIO $ tcpSendFFI v1 v2 v3
{-# INLINE tcpSend #-}


tcpRecv :: MonadIO m => TCPSocket -> Ptr () -> CInt -> m CInt
tcpRecv v1 v2 v3 = liftIO $ tcpRecvFFI v1 v2 v3
{-# INLINE tcpRecv #-}


udpOpen :: MonadIO m => Word16 -> m UDPSocket
udpOpen v1 = liftIO $ udpOpenFFI v1
{-# INLINE udpOpen #-}


udpClose :: MonadIO m => UDPSocket -> m ()
udpClose v1 = liftIO $ udpCloseFFI v1
{-# INLINE udpClose #-}


udpBind :: MonadIO m => UDPSocket -> CInt -> Ptr IPAddress -> m CInt
udpBind v1 v2 v3 = liftIO $ udpBindFFI v1 v2 v3
{-# INLINE udpBind #-}


udpUnbind :: MonadIO m => UDPSocket -> CInt -> m ()
udpUnbind v1 v2 = liftIO $ udpUnbindFFI v1 v2
{-# INLINE udpUnbind #-}


getPeerAddress :: MonadIO m => UDPSocket -> CInt -> m (Ptr IPAddress)
getPeerAddress v1 v2 = liftIO $ getPeerAddressFFI v1 v2
{-# INLINE getPeerAddress #-}


udpSend :: MonadIO m => UDPSocket -> CInt -> Ptr UDPPacket -> m CInt
udpSend v1 v2 v3 = liftIO $ udpSendFFI v1 v2 v3
{-# INLINE udpSend #-}


udpRecv :: MonadIO m => UDPSocket -> Ptr UDPPacket -> m CInt
udpRecv v1 v2 = liftIO $ udpRecvFFI v1 v2
{-# INLINE udpRecv #-}


udpSendV :: MonadIO m => UDPSocket -> Ptr UDPPacket -> CInt -> m CInt
udpSendV v1 v2 v3 = liftIO $ udpSendVFFI v1 v2 v3
{-# INLINE udpSendV #-}


udpRecvV :: MonadIO m => UDPSocket -> Ptr UDPPacket -> m CInt
udpRecvV v1 v2 = liftIO $ udpRecvVFFI v1 v2
{-# INLINE udpRecvV #-}


allocPacket :: MonadIO m => CInt -> m (Ptr UDPPacket)
allocPacket v1 = liftIO $ allocPacketFFI v1
{-# INLINE allocPacket #-}


resizePacket :: MonadIO m => Ptr UDPPacket -> CInt -> m CInt
resizePacket v1 v2 = liftIO $ resizePacketFFI v1 v2
{-# INLINE resizePacket #-}


freePacket :: MonadIO m => Ptr UDPPacket -> m ()
freePacket v1 = liftIO $ freePacketFFI v1
{-# INLINE freePacket #-}


allocPacketV :: MonadIO m => CInt -> CInt -> m (Ptr UDPPacket)
allocPacketV v1 v2 = liftIO $ allocPacketVFFI v1 v2
{-# INLINE allocPacketV #-}


freePacketV :: MonadIO m => Ptr UDPPacket -> m ()
freePacketV v1 = liftIO $ freePacketVFFI v1
{-# INLINE freePacketV #-}


allocSocketSet :: MonadIO m => CInt -> m SocketSet
allocSocketSet v1 = liftIO $ allocSocketSetFFI v1
{-# INLINE allocSocketSet #-}


freeSocketSet :: MonadIO m => SocketSet -> m ()
freeSocketSet v1 = liftIO $ freeSocketSetFFI v1
{-# INLINE freeSocketSet #-}

{-
addSocket :: MonadIO m => SocketSet -> GenericSocket -> m CInt
addSocket v1 v2 = liftIO $ addSocketFFI v1 v2
{-# INLINE addSocket #-}
-}

tcpAddSocket :: MonadIO m => SocketSet -> TCPSocket -> m CInt
tcpAddSocket v1 v2 = liftIO $ tcpAddSocketFFI v1 v2
{-# INLINE tcpAddSocket #-}


udpAddSocket :: MonadIO m => SocketSet -> UDPSocket -> m CInt
udpAddSocket v1 v2 = liftIO $ udpAddSocketFFI v1 v2
{-# INLINE udpAddSocket #-}

{-
delSocket :: MonadIO m => SocketSet -> GenericSet -> m CInt
delSocket v1 v2 = liftIO $ delSocketFFI v1 v2
{-# INLINE delSocket #-}
-}

tcpDelSocket :: MonadIO m => SocketSet -> TCPSocket -> m CInt
tcpDelSocket v1 v2 = liftIO $ tcpDelSocketFFI v1 v2
{-# INLINE tcpDelSocket #-}


udpDelSocket :: MonadIO m => SocketSet -> UDPSocket -> m CInt
udpDelSocket v1 v2 = liftIO $ udpDelSocketFFI v1 v2
{-# INLINE udpDelSocket #-}


checkSockets :: MonadIO m => SocketSet -> Word32 -> m CInt
checkSockets v1 v2 = liftIO $ checkSocketsFFI v1 v2
{-# INLINE checkSockets #-}

{-
socketReady :: MonadIO m => GenericSocket -> m CInt
socketReady v1 = liftIO $ socketReadyFFI v1
{-# INLINE socketReady #-}
-}

-- | 1
-- SDL_net library major number at compilation time
pattern SDL_NET_MAJOR_VERSION = (#const SDL_NET_MAJOR_VERSION)
-- | 2
-- SDL_net library minor number at compilation time
pattern SDL_NET_MINOR_VERSION = (#const SDL_NET_MINOR_VERSION)
-- | 7
-- SDL_net library patch level at compilation time
pattern SDL_NET_PATCHLEVEL = (#const SDL_NET_PATCHLEVEL)

-- | 0x00000000 (0.0.0.0)
-- used for listening on all network interfaces
pattern INADDR_ANY = (#const INADDR_ANY)

-- | 0xFFFFFFFF (255.255.255.255)
-- which has limited applications
pattern INADDR_NONE = (#const INADDR_NONE)

-- | 0xFFFFFFFF (255.255.255.255)
-- used as destination when sending a message to all clients on a subnet that allows broadcasts
pattern INADDR_BROADCAST = (#const INADDR_BROADCAST)

-- | 32
-- The maximum number of channels on a UDP socket
pattern SDLNET_MAX_UDPCHANNELS = (#const SDLNET_MAX_UDPCHANNELS)

-- | 4
-- The maximum number of addresses bound to a single UDP socket channel
pattern SDLNET_MAX_UDPADDRESSES = (#const SDLNET_MAX_UDPADDRESSES)
