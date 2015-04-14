import Data.List
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative

main :: IO ()
main = do
  let fileName = "bufascii.txt"
  contents <- readFile fileName
  let l = lines contents
  doWork l

doWork :: [String] -> IO ()
doWork lines = mapM_ putStrLn (future (findDateStr "Dec 19" lines) 2112)

findDateStr :: String -> [String] -> [String]
findDateStr _ [] = []
--findDateStr d xs = filter (\a -> isInfixOf d a) xs
findDateStr d xs = filter (\a -> isInfixOf d a) xs

future :: [String] -> Int -> [String]
future [] _ = []
future s d = filter (\line -> (read (words line !! 2)) >= d) s

thisYear :: IO Integer
thisYear = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian (utctDay now)
    return year
