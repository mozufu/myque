{-# LANGUAGE OverloadedStrings #-}

{- | Canonical work-item identity.

A t'Uuid' is a UUID in the textual 8-4-4-4-12 form, normalised to lowercase.
Parsing accepts any UUID version so that validation can distinguish
\"not a UUID at all\" from \"a UUID of the wrong version\"; canonical
work-item identities additionally have to satisfy 'isUuidV7'.

'newUuidV7' allocates a fresh UUIDv7 (RFC 9562 §5.7) locally, without
consulting existing identifiers, which is what makes concurrent creation on
independent Git branches collision-free.
-}
module Myque.Uuid
  ( Uuid
  , uuidText
  , parseUuid
  , uuidVersion
  , uuidVariant
  , isUuidV7
  , newUuidV7
  ) where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Char (digitToInt, intToDigit, isHexDigit, toLower)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64, Word8)
import System.IO (IOMode (ReadMode), withBinaryFile)

-- | A UUID in canonical lowercase textual form.
newtype Uuid = Uuid Text
  deriving (Eq, Ord)

instance Show Uuid where
  show (Uuid t) = T.unpack t

-- | The canonical textual form, lowercase, with group separators.
uuidText :: Uuid -> Text
uuidText (Uuid t) = t

{- | Parse a UUID from its textual form. The input may use either case; the
result is normalised to lowercase.
-}
parseUuid :: Text -> Either String Uuid
parseUuid raw
  | T.length trimmed /= 36 = Left "not a 36-character UUID"
  | map T.length groups /= [8, 4, 4, 4, 12] = Left "misplaced UUID group separators"
  | not (T.all isHexDigit (T.concat groups)) = Left "UUID contains non-hexadecimal characters"
  | otherwise = Right (Uuid (T.toLower trimmed))
 where
  trimmed = T.strip raw
  groups = T.splitOn "-" trimmed

{- | The version nibble, i.e. @7@ for a UUIDv7. Only meaningful for a value
produced by 'parseUuid'.
-}
uuidVersion :: Uuid -> Int
uuidVersion (Uuid t) = digitToInt (T.index t 14)

{- | The variant nibble. RFC 9562 variant-10 (\"RFC\") UUIDs have a variant
nibble of @8@, @9@, @a@ or @b@.
-}
uuidVariant :: Uuid -> Int
uuidVariant (Uuid t) = digitToInt (T.index t 19)

-- | Whether the UUID is a variant-10 version-7 UUID.
isUuidV7 :: Uuid -> Bool
isUuidV7 u = uuidVersion u == 7 && uuidVariant u `elem` [8, 9, 10, 11]

{- | Allocate a fresh UUIDv7: 48 bits of Unix milliseconds, the version and
variant markers, and 74 bits of entropy from @\/dev\/urandom@.
-}
newUuidV7 :: IO Uuid
newUuidV7 = do
  now <- getPOSIXTime
  entropy <- randomBytes 10
  let millis = floor (now * 1000) :: Word64
      stamp = [fromIntegral (millis `shiftR` s) | s <- [40, 32, 24, 16, 8, 0]]
      versioned = 0x70 .|. (BS.index entropy 0 .&. 0x0f)
      variant = 0x80 .|. (BS.index entropy 2 .&. 0x3f)
      bytes = stamp <> [versioned, BS.index entropy 1, variant] <> BS.unpack (BS.drop 3 entropy)
  pure (Uuid (renderBytes bytes))

-- | Render 16 bytes as @8-4-4-4-12@ lowercase hex.
renderBytes :: [Word8] -> Text
renderBytes bytes = T.intercalate "-" (map (T.concat . map hex) (chunks [4, 2, 2, 2, 6] bytes))
 where
  hex b = T.pack [nibble (b `shiftR` 4), nibble b]
  nibble b = toLower (intToDigit (fromIntegral (b .&. 0x0f)))

-- | Split a list into consecutive runs of the given lengths.
chunks :: [Int] -> [a] -> [[a]]
chunks [] _ = []
chunks (n : ns) xs = let (h, t) = splitAt n xs in h : chunks ns t

-- | Read exactly @n@ bytes of entropy from the operating system.
randomBytes :: Int -> IO BS.ByteString
randomBytes n = withBinaryFile "/dev/urandom" ReadMode (fill BS.empty)
 where
  fill acc handle
    | BS.length acc >= n = pure (BS.take n acc)
    | otherwise = do
        chunk <- BS.hGetSome handle (n - BS.length acc)
        if BS.null chunk
          then ioError (userError "/dev/urandom returned no entropy")
          else fill (acc <> chunk) handle
