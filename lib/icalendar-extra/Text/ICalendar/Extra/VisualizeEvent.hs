{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoFieldSelectors #-}

module Text.ICalendar.Extra.VisualizeEvent (
  visualizeEvent,
  CalendarSummaryOption (..),
  ConfigFromFile (..),
) where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Data.Aeson qualified as A
import Data.Bifunctor (Bifunctor (second), bimap)
import Data.ByteString qualified as BL
import Data.List.NonEmpty (nonEmpty)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text.Lazy qualified as T
import Data.Time (
  LocalTime (..),
  TimeZone,
  getCurrentTimeZone,
  localTimeToUTC,
 )
import Data.Yaml qualified as Y
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.ICalendar.Extra.Classify (ClassifyConfig (..), EventType (..), classifyEvent)
import Text.ICalendar.Extra.Draw (toPng)
import Text.ICalendar.Extra.ParseVDirSyncer (
  CalendarContent (..),
  filterAccordingToTime,
  parseCalendarsUsingCache,
 )
import Text.ICalendar.Extra.Summarize (
  accountEvent,
  checkEvent,
 )
import Text.ICalendar.Extra.Types (
  VisualizeEventWarning (..),
 )
import Text.Pretty.Simple (pShow)

data ConfigFromFile = ConfigFromFile
  { calendarDirs :: Maybe [FilePath]
  , cacheJSONPath :: Maybe FilePath
  , classifyConfig :: ClassifyConfig
  }
  deriving (Show, Generic)

instance A.FromJSON ConfigFromFile
instance A.ToJSON ConfigFromFile

data CalendarSummaryOption = CalendarSummaryOption
  { calendarDirs :: [FilePath]
  , timeRange :: (LocalTime, LocalTime)
  , outputPng :: FilePath
  , cacheJSONPath :: FilePath
  , classifyConfig :: ClassifyConfig
  }

parseAndSummaryEvents :: TimeZone -> CalendarSummaryOption -> ExceptT String (WriterT [VisualizeEventWarning] IO) [(EventType, Double)]
parseAndSummaryEvents timeZone options = do
  let
    cacheDir = takeDirectory options.cacheJSONPath
  l2 $ createDirectoryIfMissing True cacheDir
  calendarContents <- parseCalendarsUsingCache options.cacheJSONPath options.calendarDirs
  let
    events =
      mapMaybe
        ( \case
            EventContent a -> Just a
            TodoContent _ -> Nothing
        )
        calendarContents
    eventsInRange = mapMaybe (filterAccordingToTime (bimap f f options.timeRange)) events
     where
      f = localTimeToUTC timeZone
    classfiedEvent = zip eventsInRange (map (either (error . T.unpack . pShow) id . classifyEvent options.classifyConfig) eventsInRange)
    unknownEvents = filter (\(_, t) -> null t) classfiedEvent
    checkEventRes = checkEvent timeZone eventsInRange
    statistics = accountEvent (map (second (fromMaybe (EventType "unknown"))) classfiedEvent)
    eventsWithTimeCost = M.toList statistics
  case nonEmpty unknownEvents of
    Nothing -> return ()
    Just x -> lift $ tell [UnknownEvents x]
  case checkEventRes of
    Just err -> lift $ tell [err]
    Nothing -> pure ()
  l2 $ toPng options.outputPng eventsWithTimeCost
  return eventsWithTimeCost
 where
  l2 = lift . lift

visualizeEvent :: CalendarSummaryOption -> IO ()
visualizeEvent options = do
  timeZone <- getCurrentTimeZone
  (runResult, warnings) <- runWriterT $ runExceptT $ parseAndSummaryEvents timeZone options
  BL.putStr (Y.encode (runResult, warnings))
