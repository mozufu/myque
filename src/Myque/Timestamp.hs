{-# LANGUAGE OverloadedStrings #-}

{- | Work-item timestamps.

The schema requires ISO 8601 timestamps carrying an explicit UTC offset, so a
timestamp value keeps both the instant and the offset it was written with. The
offset is presentation only: equality and ordering compare instants, so
@2026-09-03T11:30:00+08:00@ and @2026-09-03T03:30:00+00:00@ are equal.

Naive (offset-less) timestamps are rejected by 'parseTimestamp'.
-}
module Myque.Timestamp
  ( Timestamp
  , timestampUtc
  , timestampZone
  , timestampText
  , parseTimestamp
  , currentTimestamp
  ) where

import Data.Function (on)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
  ( TimeZone
  , UTCTime
  , ZonedTime (..)
  , defaultTimeLocale
  , formatTime
  , getCurrentTimeZone
  , parseTimeM
  , utcToZonedTime
  , zonedTimeToUTC
  )
import Data.Time.Clock (getCurrentTime)

-- | An instant plus the UTC offset it is rendered with.
data Timestamp = Timestamp
  { timestampUtc :: UTCTime
  -- ^ The instant, normalised to UTC.
  , timestampZone :: TimeZone
  -- ^ The offset the value is written back with.
  }

instance Eq Timestamp where
  (==) = (==) `on` timestampUtc

instance Ord Timestamp where
  compare = compare `on` timestampUtc

instance Show Timestamp where
  show = T.unpack . timestampText

-- | Render in the canonical @%Y-%m-%dT%H:%M:%S±HH:MM@ form.
timestampText :: Timestamp -> Text
timestampText (Timestamp utc zone) =
  T.pack (formatTime defaultTimeLocale isoFormat (utcToZonedTime zone utc))

-- | The format used for writing timestamps back to frontmatter.
isoFormat :: String
isoFormat = "%Y-%m-%dT%H:%M:%S%Ez"

{- | Parse an ISO 8601 timestamp. An explicit offset is mandatory; @Z@,
@±HH:MM@ and @±HHMM@ are all accepted, as is a fractional-seconds part.
-}
parseTimestamp :: Text -> Either String Timestamp
parseTimestamp raw = case concatMap attempt formats of
  zoned : _ -> Right (Timestamp (zonedTimeToUTC zoned) (zonedTimeZone zoned))
  []
    | any (`T.isInfixOf` trimmed) ["+", "Z", "z"] -> Left "malformed ISO 8601 timestamp"
    | otherwise -> Left "timestamp is missing an explicit UTC offset"
 where
  trimmed = T.strip raw
  input = T.unpack trimmed
  attempt fmt = parseTimeM False defaultTimeLocale fmt input :: [ZonedTime]
  formats =
    [ "%Y-%m-%dT%H:%M:%S%Q%Ez"
    , "%Y-%m-%dT%H:%M:%S%QZ"
    , "%Y-%m-%dT%H:%M%Ez"
    , "%Y-%m-%dT%H:%MZ"
    ]

-- | The current instant, in the local offset.
currentTimestamp :: IO Timestamp
currentTimestamp = Timestamp <$> getCurrentTime <*> getCurrentTimeZone
