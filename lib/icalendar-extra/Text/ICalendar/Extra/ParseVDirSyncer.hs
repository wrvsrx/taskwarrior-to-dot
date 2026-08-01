{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Text.ICalendar.Extra.ParseVDirSyncer (
  parseVDirSyncerICSFile,
  filterAccordingToTime,
  parseCalendarsUsingCache,
  CalendarContent (..),
) where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.Writer (WriterT, tell)
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Default (def)
import Data.Fixed (Fixed (MkFixed), Pico)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text.Lazy qualified as T
import Data.Time (
  NominalDiffTime,
  TimeZone (..),
  UTCTime,
  ZonedTime (ZonedTime),
  addUTCTime,
  secondsToNominalDiffTime,
  zonedTimeToUTC,
 )
import GHC.Generics (Generic)
import System.Directory (doesFileExist, getModificationTime, listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.ICalendar
import Text.ICalendar.Extra.Types (
  Event (..),
  Todo (..),
  VisualizeEventWarning (..),
 )

dateTimeToUTC :: M.Map T.Text VTimeZone -> DateTime -> Either String UTCTime
dateTimeToUTC _ (FloatingDateTime _) = Left "don't support floating datetime"
dateTimeToUTC _ (UTCDateTime t) = Right t
dateTimeToUTC m (ZonedDateTime t z) = do
  zone <- maybeToEither "can't find timezone" (M.lookup z m)
  let
    standardTimes = vtzStandardC zone
    standardTime = assert (S.size standardTimes == 1) $ S.elemAt 0 standardTimes
    timezone = TimeZone (utcOffsetValue (tzpTZOffsetTo standardTime) `div` 60) False ""
  Right (zonedTimeToUTC (ZonedTime t timezone))

startTimeToUTC :: M.Map T.Text VTimeZone -> DTStart -> Either String UTCTime
startTimeToUTC _ (DTStartDate _ _) = Left "only start date"
startTimeToUTC m (DTStartDateTime t _) = dateTimeToUTC m t

endTimeToUTC :: M.Map T.Text VTimeZone -> DTEnd -> Either String UTCTime
endTimeToUTC _ (DTEndDate _ _) = Left "only end date"
endTimeToUTC m (DTEndDateTime t _) = dateTimeToUTC m t

dateHourMinutesSecond :: Int -> Int -> Int -> Int -> Pico
dateHourMinutesSecond d h m s = MkFixed $ toInteger (s + 60 * (m + 60 * (h + 24 * d)))

durationToNominalDiffTime :: Duration -> Maybe NominalDiffTime
durationToNominalDiffTime (DurationWeek _ _) = Nothing
durationToNominalDiffTime (DurationDate sign d h m s) = assert (sign == Positive) (Just $ secondsToNominalDiffTime (dateHourMinutesSecond d h m s))
durationToNominalDiffTime (DurationTime sign h m s) = assert (sign == Positive) (Just $ secondsToNominalDiffTime (dateHourMinutesSecond 0 h m s))

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

vEventToEvent :: M.Map T.Text VTimeZone -> VEvent -> Either String Event
vEventToEvent timeZoneMap ve = do
  summary <- maybeToEither "no summary" ve.veSummary
  start <- maybeToEither "no start time" ve.veDTStart
  endOrDur <- maybeToEither "no end time or dur" ve.veDTEndDuration
  let
    text = T.unpack (summaryValue summary)
  case endOrDur of
    Left end -> do
      startTime <- case startTimeToUTC timeZoneMap start of
        Left x -> Left (x <> show ve)
        Right x -> Right x
      endTime <- endTimeToUTC timeZoneMap end
      return
        ( Event
            { summary = text
            , startTime = startTime
            , endTime = endTime
            }
        )
    Right durPacked -> do
      ts <- startTimeToUTC timeZoneMap start
      dur <- maybeToEither "fail to parse duration time" (durationToNominalDiffTime $ durationValue durPacked)
      let
        te = addUTCTime dur ts
      return
        ( Event
            { summary = text
            , startTime = ts
            , endTime = te
            }
        )

parseVDirSyncerICSFile :: BL.ByteString -> Either String CalendarContent
parseVDirSyncerICSFile cnt = do
  vCalAAndWarnings <- parseICalendar def "<file path>" cnt
  let
    vCalA = fst vCalAAndWarnings
  vCal <- case vCalA of
    [] -> Left "empty calendar file"
    [x] -> Right x
    _ -> Left "more than one calenders in a file"
  let
    timeZoneMap = vCal.vcTimeZones
    vEvents = vCal.vcEvents & M.toList
    vTodos = vCal.vcTodos & M.toList
  case (vEvents, vTodos) of
    ([], [(_, _)]) -> return $ TodoContent (Todo ())
    ([(_, ve)], []) -> do
      event <- vEventToEvent timeZoneMap ve
      return $ EventContent event
    ([], []) -> Left "no events and todos"
    _ -> Left "wrong vCal content"

-- 左闭右开
filterAccordingToTime :: (UTCTime, UTCTime) -> Event -> Maybe Event
filterAccordingToTime (rangeStart, rangeEnd) (Event summary eventStart eventEnd) =
  if (eventEnd <= rangeStart) || (rangeEnd <= eventStart)
    then Nothing
    else
      Just $
        Event
          { summary = summary
          , startTime = max eventStart rangeStart
          , endTime = min eventEnd rangeEnd
          }

data ContentCache = ContentCache
  { cacheTime :: UTCTime
  , content :: CalendarContent
  , filename :: FilePath
  }
  deriving (Generic)

instance A.FromJSON ContentCache
instance A.ToJSON ContentCache

data CalendarContent = EventContent Event | TodoContent Todo deriving (Show, Generic)

instance A.FromJSON CalendarContent
instance A.ToJSON CalendarContent

-- 分成三部分，A 记为 cache 集合，B 记为文件集合
-- 在 A 且 B 中，
--   时间晚于 cache，那么解析
--   时间等于 cache，那么不动
--   时间早于 cache，那么报 Warning
-- 在 A 不在 B 中，
--   删除
-- 在 B 不在 A 中，
--   解析
-- 可能失败的地方：
--   解析 json 过程
--   解析 ics 过程
parseCalendarsUsingCache :: FilePath -> [FilePath] -> ExceptT String (WriterT [VisualizeEventWarning] IO) [CalendarContent]
parseCalendarsUsingCache cacheJSON calendarDirs = do
  exist <- l2 $ doesFileExist cacheJSON
  unless exist $ l2 $ writeFile cacheJSON "[]"
  cacheContent <- l2 $ BL.readFile cacheJSON
  let
    maybeEventCacheA = cacheContent & A.decode :: Maybe [ContentCache]
  eventCacheA <- case maybeEventCacheA of
    Just x -> return x
    Nothing -> throwE "fail to parse json cache"
  -- (fullPath, mtime) for every .ics across all calendar dirs; the full path
  -- is used as the cache key so files with the same name in different dirs
  -- don't collide.
  icsFileWithModificationTimeB <- l2 $ concat <$> mapM collectIcs calendarDirs
  let
    contentInFileMap = M.fromList icsFileWithModificationTimeB
    contentInCacheMap = M.fromList (map (\x -> (x.filename, (x.cacheTime, x.content))) eventCacheA)
    contentInFileWithoutCache = M.toList $ M.difference contentInFileMap contentInCacheMap
    contentInBothFileAndCache = M.toList $ M.intersectionWith (\fileTime (cacheTime, event) -> (fileTime, cacheTime, event)) contentInFileMap contentInCacheMap
    parseContent :: (FilePath, UTCTime) -> ExceptT String (WriterT [VisualizeEventWarning] IO) ContentCache
    parseContent = \(filePath, fileTime) -> do
      cnt <- l2 $ BL.readFile filePath
      let
        contentEither = parseVDirSyncerICSFile cnt
      case contentEither of
        Right content ->
          return
            ( ContentCache
                { cacheTime = fileTime
                , content = content
                , filename = filePath
                }
            )
        Left err -> throwE ("parse " <> filePath <> " fail with: " <> err)
  contentNotInCacheC <- mapM parseContent contentInFileWithoutCache
  contentInBothFileAndCacheD <-
    mapM
      ( \(filePath, (fileTime, cacheTime, content)) ->
          if fileTime == cacheTime
            then
              return
                ( ContentCache
                    { cacheTime = cacheTime
                    , content = content
                    , filename = filePath
                    }
                )
            else do
              lift $ when (fileTime < cacheTime) $ tell [FileTimeEarlierThanCacheTime filePath]
              parseContent (filePath, fileTime)
      )
      contentInBothFileAndCache
  let
    finalEventsCache = contentNotInCacheC <> contentInBothFileAndCacheD
    finalEvents = map (\x -> x.content) finalEventsCache
  l2 $ BL.writeFile cacheJSON (A.encode finalEventsCache)
  return finalEvents
 where
  l2 = lift . lift
  collectIcs :: FilePath -> IO [(FilePath, UTCTime)]
  collectIcs dir = do
    names <- listDirectory dir
    let
      icss = filter ((== ".ics") . takeExtension) names
    mapM (\name -> getModificationTime (dir </> name) >>= \t -> return (dir </> name, t)) icss
