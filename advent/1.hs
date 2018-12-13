
import Debug.Trace
import Control.Monad
import Text.Read
import qualified Data.Set as Set

getNums :: IO [Integer]
getNums = do
  l <- getContents
  let ls = lines l
  case (forM ls parseNum) of
    Nothing -> error "no parse"
    Just xs -> return xs

parseNum :: String -> Maybe Integer
parseNum (c:cs) = (*) <$> parseSign c <*> (readMaybe cs)

parseSign :: Char -> Maybe Integer
parseSign c | c == '+' = Just 1
parseSign c | c == '-' = Just $ -1
parseSign c = Nothing



main = do
  nums <- cycle <$> getNums
  let running_totals = scanl (+) 0 nums
  print $ loop Set.empty running_totals
  where
    loop s (x:running) | Set.member x s = x
    loop s (x:running) = loop (Set.insert x s) running
