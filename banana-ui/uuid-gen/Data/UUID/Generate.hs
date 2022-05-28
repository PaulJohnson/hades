{-
Copyright © Paul Johnson 2019. See LICENSE file for details.

This file is part of the Haskell Diagram Editing System (HADES) software.


-}

{- |

The uuid library has a "Random" instance for UUID, but we can't use it:

* The GHC System.Random.StdGen only has about 61 bits of randomness, and by default is
always seeded with 0, so using that is a Bad Thing.

* There is a suitable cryptographic random number generator in the crypto-api package, but
it isn't an instance of "RandomGen", so we can't use it with the Random instance for UUID.

The Data.UUID.V4 UUID generator uses cryptographic random numbers, but
that implies an IO action for every UUID. If you want a good stream of UUIDs, you have to
roll your own.

In theory this module could use any instance of "CryptoRandomGen", but in practice it is
specialised to "Generator" to simplify the types.
-}
module Data.UUID.Generate (
  Generator,
  uuidStreamIO,
  uuidStream,
  newGenerator,
  generateUUID,
  -- ** Re-exported for convenience.
  CryptoRandomGen (..),
  splitGen
) where

import Crypto.Random.DRBG
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.List
import Data.Text (Text, pack)
import Data.UUID
import System.IO.Unsafe

-- | The random number generator. This is an instance of "CryptoRandomGen", so use its
-- functions to produce and manipulate values of this type.
type Generator = CtrDRBG


-- Implicit single generator instance. This is modelled on the code in "System.Random". It uses
-- "unsafePerformIO" to create the singleton generator.
{-# NOINLINE theGenerator #-}
theGenerator :: IORef Generator
theGenerator = unsafePerformIO $ do
  rng <- newGenIO :: IO CtrDRBG
  newIORef rng


newGenerator :: IO Generator
newGenerator = atomicModifyIORef' theGenerator $ \g ->
  case splitGen g of
    Left err -> error $ show err  -- Should never happen: 2^48 bytes are available.
    Right v -> v


-- | A stream of UUIDs seeded by the entropy pool.
uuidStreamIO :: IO [UUID]
uuidStreamIO = uuidStream <$> newGenerator


-- | A stream of UUIDs from the random generator. Use with "splitGen" and "newGenIO".
--
-- >  do
-- >     gen <- newGenIO :: IO CtrDRBG
-- >     return $ uuidStream gen
uuidStream :: Generator -> [UUID]
uuidStream = unfoldr $ either (const Nothing) Just . generateUUID


-- | Produce one UUID. Should never fail.
generateUUID :: Generator -> Either Text (UUID, Generator)
generateUUID g1 = case genBytes 16 g1 of
  Left err -> Left $ pack $ "UUID generator: " ++ show err
  --  The generator is good for 2 to the power 48 UUIDs per session, so this should never happen.
  Right (bs, g2) -> case fromByteString $ LBS.fromStrict bs of
    Nothing -> Left $ pack "UUID generator conversion error"
    --  The conversion should always succeed when given 16 bytes.
    Just uuid -> Right (uuid, g2)
