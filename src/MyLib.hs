{-# LANGUAGE DeriveGeneric #-}

module MyLib (run) where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Csv as Csv
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (NominalDiffTime, UTCTime, parseTimeM)
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import qualified Data.Time.Format.ISO8601 as Time
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Options.Applicative (strOption)
import qualified Options.Applicative as Options
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

newtype ISOTime = ISOTime {getISOTime :: UTCTime}
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

instance Monoid Duration where
  mempty = Duration 0

data Durations = Durations Duration (Map String Duration)

durationPercent :: Duration -> Duration -> Float
durationPercent (Duration a) (Duration b) = realToFrac a / realToFrac b

instance Show Durations where
  show (Durations week a) =
    unlines $
      fmap showDuration (Map.toList a)
        <> [ "",
             showTotal
           ]
    where
      total = mconcat (toList a)
      weekRatio :: Duration -> Float
      weekRatio d = durationPercent d week * 100
      showTotal =
        mconcat
          [ "total: ",
            show total,
            " ",
            "(" <> printf "%3.3f" (weekRatio total) <> "% of working hours",
            " - ",
            show week,
            ")"
          ]
      showDuration (project, d) =
        mconcat
          [ project,
            ": ",
            show d,
            " ",
            "(" <> printf "%3.3f" ratio <> "%)",
            " ",
            "(" <> printf "%3.3f" (weekRatio d) <> "% of working hours)"
          ]
        where
          ratio :: Float
          ratio = durationPercent d total * 100

instance Semigroup Durations where
  Durations week a <> Durations _ b = Durations week (Map.unionWith (<>) a b)

timeDiff :: Duration -> Project -> Project -> Durations
timeDiff week (Project (ISOTime a) name) (Project (ISOTime b) _) =
  Durations week $ Map.singleton name (Duration $ Time.diffUTCTime b a)

removeLogout :: Durations -> Durations
removeLogout (Durations week d) = Durations week $ Map.delete "logout" d

getStartOfWeek :: UTCTime -> UTCTime
getStartOfWeek t = Time.UTCTime day' 0
  where
    day = Time.utctDay t
    (year, week, _) = Time.toWeekDate day
    day' = Time.fromWeekDate year week 1 -- Monday of the same week

data Args = Args
  { startDate :: Maybe UTCTime,
    endDate :: Maybe UTCTime,
    logFile :: FilePath,
    vacationDays :: Natural
  }

parseDate :: Options.ReadM UTCTime
parseDate = Options.eitherReader $ \str ->
  case Time.iso8601ParseM str of
    Nothing -> Left $ "cannot parse date: " <> str
    Just day -> pure (Time.UTCTime day 0)

args :: Options.Parser Args
args =
  Args
    <$> Options.optional
      ( Options.option
          parseDate
          (Options.long "start-date" <> Options.metavar "YYYY-MM-DD")
      )
    <*> Options.optional
      ( Options.option
          parseDate
          (Options.long "end-date" <> Options.metavar "YYYY-MM-DD")
      )
    <*> Options.strArgument (Options.metavar "FILE")
    <*> Options.option
      Options.auto
      ( Options.long "vacations"
          <> Options.value 0
          <> Options.showDefault
      )

parserOptions :: Options.ParserInfo Args
parserOptions =
  Options.info (Options.helper <*> args) $
    Options.fullDesc
      <> Options.progDesc "Count the hours spent on each project"
      <> Options.header "count-hours"

concatDurations :: Duration -> [Durations] -> Durations
concatDurations weekDuration = foldr (<>) (Durations weekDuration mempty)

run :: IO ()
run = do
  Args mstart mend file vacations <- Options.execParser parserOptions
  bytes <- LazyByteString.readFile file
  case Csv.decode Csv.HasHeader bytes of
    Left err -> hPutStrLn stderr err
    Right rows -> do
      now <- Time.getCurrentTime
      let start = fromMaybe (getStartOfWeek now) mstart
      let end = fromMaybe now mend
      let weekDuration = Duration $ 8 * 60 * 60 * fromIntegral (length (weekDays start end) - fromIntegral vacations)
      let inRange row =
            let t = getISOTime (started row)
             in t >= start && t <= end
      let rows' = filter inRange (toList rows <> [Project (ISOTime now) "dummy"])
       in case rows' of
            [] -> do
              hPutStrLn stderr "No rows in range"
              exitFailure
            _ ->
              print $
                removeLogout $
                  concatDurations weekDuration $
                    zipWith (timeDiff weekDuration) rows' (tail rows')

days :: UTCTime -> UTCTime -> [Time.Day]
days start end = takeWhile (\d -> Time.UTCTime d 0 < end) [startDay ..]
  where
    startDay = Time.utctDay start

weekDays :: UTCTime -> UTCTime -> [Time.Day]
weekDays start end = filter isWeekDay (days start end)
  where
    isWeekDay day =
      case Time.dayOfWeek day of
        Time.Monday -> True
        Time.Tuesday -> True
        Time.Wednesday -> True
        Time.Thursday -> True
        Time.Friday -> True
        Time.Saturday -> False
        Time.Sunday -> False
