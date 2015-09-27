{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "SDL2/SDL_net.h"
module SDL.Raw.Net.Types
( IPAddress (..)
, TCPSocket
, UDPSocket
, UDPPacket (..)
, SocketSet
, GenericSocket
) where

import Control.Monad.IO.Class
import GHC.Word
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (init)
import SDL.Raw hiding (init,quit)


data IPAddress = IPAddress
  { host    :: Word32
  , port    :: Word16
  } deriving (Show,Eq)

instance Storable IPAddress where
  sizeOf _ = (#size IPaddress)
  alignment = sizeOf
  peek ptr = do
    h <- (#peek IPaddress, host) ptr
    p <- (#peek IPaddress, port) ptr
    return $! IPAddress h p
  poke ptr (IPAddress h p) = do
    (#poke IPaddress, host) ptr h
    (#poke IPaddress, port) ptr p

data TCPSocketTarget
type TCPSocket = Ptr TCPSocketTarget

data UDPSocketTarget
type UDPSocket = Ptr UDPSocketTarget

data GenericSocket = GenericSocket { ready :: !CInt } deriving (Show,Eq)

data UDPPacket = UDPPacket
  { channel     :: !CInt
  , payload     :: Ptr Word8
  , len         :: !CInt
  , maxlen      :: !CInt
  , status      :: !CInt
  , address     :: !IPAddress
  } deriving (Show,Eq)

instance Storable UDPPacket where
  sizeOf _ = (#size UDPpacket)
  alignment = sizeOf
  peek ptr = do
    c <- (#peek UDPpacket, channel) ptr
    d <- (#peek UDPpacket, data) ptr
    l <- (#peek UDPpacket, len) ptr
    m <- (#peek UDPpacket, maxlen) ptr
    s <- (#peek UDPpacket, status) ptr
    a <- (#peek UDPpacket, address) ptr
    return $! UDPPacket c d l m s a
  poke ptr (UDPPacket c d l m s a) = do
    (#poke UDPpacket, channel) ptr c
    (#poke UDPpacket, data) ptr d
    (#poke UDPpacket, len) ptr l
    (#poke UDPpacket, maxlen) ptr m
    (#poke UDPpacket, status) ptr s
    (#poke UDPpacket, address) ptr a

data SocketSetTarget
type SocketSet = Ptr SocketSetTarget
{-
data GenericSetTarget
type GenericSet = Ptr GenericSetTarget
-}

