{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module SDL.Raw.Net.Enum
-- Defines
( pattern SDL_NET_MAJOR_VERSION
, pattern SDL_NET_MINOR_VERSION
, pattern SDL_NET_PATCHLEVEL
, pattern INADDR_ANY
, pattern INADDR_NONE
, pattern INADDR_BROADCAST
, pattern SDLNET_MAX_UDPCHANNELS
, pattern SDLNET_MAX_UDPADDRESSES
) where

#include "SDL2/SDL_net.h"

import GHC.Word
import SDL.Raw.Net.Types


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
