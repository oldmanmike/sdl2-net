{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "SDL2/SDL_net.h"
module SDL.Raw.Net 
( init
, quit
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

data TCPSocket
type TCPSocketPtr = Ptr TCPSocket

data UDPSocket
type UDPSocketPtr = Ptr UDPSocket

data UDPPacket = UDPPacket
  { channel     :: !CInt
  , payload     :: Ptr Word8
  , len         :: !CInt
  , maxlen      :: !CInt
  , status      :: !CInt
  , address     :: !IPAddress
  } deriving (Show,Eq)


-------------------------------------------------------------------------------
-- General
-------------------------------------------------------------------------------
foreign import ccall unsafe "SDLNet_Init" initFFI :: IO CInt
foreign import ccall unsafe "SDLNet_Quit" quitFFI :: IO ()

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
