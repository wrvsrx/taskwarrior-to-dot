{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Cli (
  VisOption (..),
  TotalOption (..),
  AddEventOption (..),
  ModOption (..),
  totalParser,
  parseVisualizeEventCliOption,
) where

import Data.Aeson qualified as A
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (
  Day (..),
  LocalTime (..),
  TimeOfDay (..),
  defaultTimeLocale,
  getCurrentTime,
  getCurrentTimeZone,
  parseTimeM,
  utcToLocalTime,
 )
import Text.ICalendar.Extra.VisualizeEvent (
  CalendarSummaryOption (..),
  ConfigFromFile (..),
 )
import Options.Applicative
import System.Directory (XdgDirectory (..), getXdgDirectory)
import Task (TaskDate (..), dateToDay)
import Utils (TimeRange (..))

data VisOption = VisOption
  { highlights :: [T.Text]
  , deleted :: Bool
  , filter :: Maybe T.Text
  }

data AddEventOption = EventOption
  { summary :: T.Text
  , start :: T.Text
  , end :: T.Text
  , filter' :: Maybe T.Text
  }

dateParser :: Parser TaskDate
dateParser = argument dayReader (metavar "DATE" <> value (RelativeDate 0))
 where
  parseRelativeDate :: String -> Maybe TaskDate
  parseRelativeDate arg = do
    case arg of
      "today" -> Just $ RelativeDate 0
      "tomorrow" -> Just $ RelativeDate 1
      "yesterday" -> Just $ RelativeDate (-1)
      _ -> Nothing
  parseAbsoluteDate :: String -> Maybe TaskDate
  parseAbsoluteDate arg = do
    case parseTimeM True defaultTimeLocale "%Y%m%d" arg <|> parseTimeM True defaultTimeLocale "%Y-%m-%d" arg of
      Just x -> Just $ AbsoluteDate x
      Nothing -> Nothing
  dayReader = eitherReader $ \arg ->
    case parseRelativeDate arg <|> parseAbsoluteDate arg of
      Just x -> Right x
      Nothing -> Left "Failed to parse date. The supported formats are `today`, `tomorrow`, `yesterday`, YYYYMMDD or YYYY-MM-DD."

data TotalOption
  = GetTaskClosure (Maybe T.Text)
  | VisualizeTask VisOption
  | AddEvent AddEventOption
  | ListEvent TaskDate
  | Mod ModOption
  | ListTask (Maybe T.Text)
  | PendingTask (Maybe T.Text)
  | FinishTask (Maybe T.Text)
  | ViewTask (Maybe T.Text)
  | AddTask [T.Text]
  | DeleteTask T.Text
  | VisualizeEvent VisualizeEventOption

data ModOption = ModOption
  { filter :: T.Text
  , modifiers :: [T.Text]
  }

parseHighlights :: String -> Either String [T.Text]
parseHighlights = Right . T.splitOn "," . T.pack

visParser :: Parser VisOption
visParser =
  VisOption
    <$> option
      (eitherReader parseHighlights)
      ( long "highlights"
          <> short 'h'
          <> value []
          <> help "Tags to be highlighted."
      )
    <*> flag False True (long "deleted" <> short 'd' <> help "Show deleted tasks.")
    <*> maybeFilterParser

modParser :: Parser ModOption
modParser =
  ModOption
    <$> argument str (metavar "FILTER")
    <*> many (argument str (metavar "MODIFIER"))

eventParser :: Parser AddEventOption
eventParser =
  EventOption
    <$> argument str (metavar "SUMMARY")
    <*> argument str (metavar "START")
    <*> argument str (metavar "END")
    <*> optional (argument str (metavar "TASK"))

filterParser :: Parser T.Text
filterParser = argument str (metavar "FILTER")

maybeFilterParser :: Parser (Maybe T.Text)
maybeFilterParser = optional filterParser

data VisualizeEventOption = CliOption
  { calendarDirs :: [FilePath]
  , timeRange :: Maybe TimeRange
  , outputPng :: FilePath
  , cacheJSONPath :: Maybe FilePath
  , configPath :: Maybe FilePath
  }
  deriving (Show)

calendarVisualizationParser :: Parser VisualizeEventOption
calendarVisualizationParser = do
  calendarDirs <-
    many $
      strOption
        ( long "calendar-dir"
            <> short 'c'
            <> help "calendar directory (repeatable)"
        )
  timeRange <- optional timeRangeParser
  outputPng :: FilePath <-
    strOption
      ( long "png"
          <> short 'p'
          <> help "output png file path"
          <> metavar "PNG_FILE"
      )
  cacheJSONPath <-
    optional $
      strOption
        ( long "cache"
            <> metavar "CACHE_JSON_PATH"
            <> help "cache json file path, default to $XDG_CACHE_HOME/task-utils/cache.json"
        )
  configPath <-
    optional $
      strOption
        ( long "config"
            <> help "configuration file, default to $XDG_CONFIG_HOME/task-utils/config.json"
            <> metavar "CONFIG"
        )
  pure
    CliOption
      { calendarDirs = calendarDirs
      , timeRange = timeRange
      , outputPng = outputPng
      , cacheJSONPath = cacheJSONPath
      , configPath = configPath
      }
 where
  dateTimeReader = maybeReader $ parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S" :: ReadM LocalTime
  startTimeParser = option dateTimeReader (long "start-time" <> short 's' <> help "start time")
  endTimeParser = option dateTimeReader (long "end-time" <> short 'e' <> help "end time")

  timeRangeParser =
    TimeRangeDay
      <$> dateParser
        <|> TimeRangeRange
      <$> startTimeParser
      <*> optional endTimeParser

parseVisualizeEventCliOption :: VisualizeEventOption -> IO CalendarSummaryOption
parseVisualizeEventCliOption cliOption = do
  defaultCacheFile <- getXdgDirectory XdgCache "task-utils/cache.json"
  defaultConfigFile <- getXdgDirectory XdgConfig "task-utils/config.json"
  timeZone <- getCurrentTimeZone
  time <- getCurrentTime
  let
    currentDay :: Day = utcToLocalTime timeZone time & localDay
  let
    configFile = fromMaybe defaultConfigFile cliOption.configPath
  config :: ConfigFromFile <- A.eitherDecodeFileStrict configFile <&> either error id
  let
    calendarDirs =
      if not (null cliOption.calendarDirs)
        then cliOption.calendarDirs
        else case config.calendarDirs of
          Just x -> x
          Nothing -> error "calendarDirs is not specified either in command line or in config file"
  timeRange <- do
    case cliOption.timeRange of
      Just (TimeRangeDay day) -> do
        day' <- dateToDay day
        return
          ( LocalTime{localDay = day', localTimeOfDay = TimeOfDay 0 0 0}
          , LocalTime{localDay = succ day', localTimeOfDay = TimeOfDay 0 0 0}
          )
      Just (TimeRangeRange startTime maybeEndTime) -> case maybeEndTime of
        Just endTime -> return (startTime, endTime)
        Nothing -> return (startTime, LocalTime{localDay = succ (localDay startTime), localTimeOfDay = TimeOfDay 0 0 0})
      Nothing ->
        return
          ( LocalTime{localDay = currentDay, localTimeOfDay = TimeOfDay 0 0 0}
          , LocalTime{localDay = succ currentDay, localTimeOfDay = TimeOfDay 0 0 0}
          )
  return
    CalendarSummaryOption
      { calendarDirs = calendarDirs
      , timeRange = timeRange
      , outputPng = cliOption.outputPng
      , cacheJSONPath = fromMaybe defaultCacheFile $ cliOption.cacheJSONPath <|> config.cacheJSONPath
      , classifyConfig = config.classifyConfig
      }

totalParser :: Parser TotalOption
totalParser =
  hsubparser
    ( command "visualize-task" (info (VisualizeTask <$> visParser) idm)
        <> command "get-task-closure" (info (GetTaskClosure <$> maybeFilterParser) idm)
        <> command "modify-task" (info (Mod <$> modParser) idm)
        <> command "add-event" (info (AddEvent <$> eventParser) idm)
        <> command "list-event" (info (ListEvent <$> dateParser) idm)
        <> command "list-task" (info (ListTask <$> maybeFilterParser) idm)
        <> command "pending-task" (info (PendingTask <$> maybeFilterParser) idm)
        <> command "finish-task" (info (FinishTask <$> maybeFilterParser) idm)
        <> command "view-task" (info (ViewTask <$> maybeFilterParser) idm)
        <> command "add-task" (info (AddTask <$> many (argument str (metavar "TASK_INFO"))) idm)
        <> command "delete-task" (info (DeleteTask <$> filterParser) idm)
        <> command "visualize-event" (info (VisualizeEvent <$> calendarVisualizationParser) idm)
    )
