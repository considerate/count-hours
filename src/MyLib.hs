{-# LANGUAGE DeriveGeneric #-}

module MyLib (run) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Csv as Csv
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, UTCTime, parseTimeM)
import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as Time
import GHC.Generics (Generic)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

newtype ISOTime = ISOTime UTCTime
  deriving (Show)

instance Csv.FromField ISOTime where
  parseField t =
    ISOTime . Time.zonedTimeToUTC <$> Time.iso8601ParseM (Text.unpack . Text.decodeUtf8 $ t)

data Project = Project {started :: ISOTime, projectName :: String}
  deriving (Generic, Show)

instance Csv.FromRecord Project

newtype Duration = Duration
  { duration :: NominalDiffTime
  }

instance Show Duration where
  show (Duration d) = Time.formatTime Time.defaultTimeLocale "%H:%M:%S" d

instance Semigroup Duration where
  Duration a <> Duration b = Duration (a + b)

newtype Durations = Durations (Map String Duration)

instance Show Durations where
  show (Durations a) = unlines $ fmap showDuration (Map.toList a)
    where
      showDuration (project, d) = project <> ": " <> show d

instance Semigroup Durations where
  Durations a <> Durations b = Durations (Map.unionWith (<>) a b)

instance Monoid Durations where
  mempty = Durations mempty

timeDiff :: Project -> Project -> Durations
timeDiff (Project (ISOTime a) name) (Project (ISOTime b) _) =
  Durations $ Map.singleton name (Duration $ Time.diffUTCTime b a)

run :: IO ()
run = do
  [file] <- getArgs
  bytes <- LazyByteString.readFile file
  case Csv.decode Csv.HasHeader bytes of
    Left err -> hPutStrLn stderr err
    Right rows ->
      let rows' = toList rows
       in print $ mconcat $ zipWith timeDiff rows' (tail rows')
