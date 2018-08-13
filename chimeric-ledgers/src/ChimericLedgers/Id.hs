module ChimericLedgers.Id where

import           Data.Word (Word32)

type Id = Word32

-- | @hash c@ is the cryptographic collision-resistant hash of object @c@.
--
hash :: c -> Id
hash = undefined
