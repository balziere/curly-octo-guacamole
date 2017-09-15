import Data.Either.Combinators
import Graphics.EasyPlot (plot, PDF, Graph2D(Data2D), TerminalType(PDF))
import DataCollect
import qualified Data.Set as S
import Data.Text (Text(..))
import Data.Either (rights)
import qualified Data.Time as Time
import qualified Data.Time.LocalTime as Local
import qualified Data.Time.Clock.POSIX as POSIX
import Data.Time.Calendar (Day(..),)
import Data.Dates (dayToDateTime,dateWeekDay,weekdayNumber)

timeZone :: Local.TimeZone
timeZone = Local.hoursToTimeZone 2


getNames :: [TelegramPost] -> S.Set Text
getNames = S.fromList . map username

getLists :: S.Set Text -> [TelegramPost] -> [(Text,[Local.ZonedTime])]
getLists a t = map (\x -> 
            (x,map parseTime $ map date $ filter (\y -> 
                        username y == x) t)) ( S.toList a)
 

quickList :: [TelegramPost] -> [(Text,[Local.ZonedTime])]
quickList t = getLists (getNames t) t

parseTime :: Int -> Local.ZonedTime 
parseTime t = Local.utcToZonedTime timeZone $
                POSIX.posixSecondsToUTCTime . fromIntegral $ t

exampleDate :: Local.ZonedTime
exampleDate = parseTime 0 


getDate :: Local.ZonedTime -> Day
getDate = Local.localDay . Local.zonedTimeToLocalTime

getWeekDay :: Day -> Int
getWeekDay = weekdayNumber . dateWeekDay . dayToDateTime



dateHistoDat :: [Local.ZonedTime] -> [Int]
dateHistoDat xs = map (\o ->
                    length $ filter (\oo ->
                                (getWeekDay . getDate $ oo)  == o) xs) [1..7]

dateHistos :: [TelegramPost] -> [(Text,[Int])]
dateHistos t = map (\o ->
                (fst o, dateHistoDat . snd $ o)) $ quickList t


plotHisto :: (Text,[Int]) -> IO Bool
plotHisto (name, xs) = plot (PDF (username ++ ".pdf")) $
            Data2D [Title username] [] $ zip [1..] xs


plotData :: IO ()
plotData= do
    let jsonFile = "Autistengaleere.jsonl" :: FilePath
    f <- getJSON jsonFile
    --print . dateHistos . rights . decodeList $ f
    let histos = dateHistos .rights .decodeLis $ f
    map plotHisto histos


