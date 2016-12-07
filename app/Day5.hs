module Day5 where


import Control.Monad.State
import Control.Parallel.Strategies
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import Data.Digest.Pure.MD5
import Data.List
import Data.Maybe
import Safe


md5String :: String -> String
md5String = show . md5 . pack

hashRoomId :: String -> Int -> String
hashRoomId id x = md5String $ id ++ (show x)

maybeCode :: Int -> String -> Maybe Char
maybeCode idx ('0':'0':'0':'0':'0':x:y:_) =
  case readMay (x:"") of
    Just x ->
      if idx  == x then
        Just y
      else
        Nothing
    Nothing -> Nothing
maybeCode _ _ = Nothing

nextCode :: String -> Int -> Int -> Maybe Char
nextCode a b c = maybeCode b $ hashRoomId a c

findNextCode :: String -> Int -> Maybe Char
findNextCode a b = head $ take 1 $ dropWhile ((==) Nothing) $ map (nextCode a b) [0..]

password :: String -> Maybe String
password a = sequence $ (map (findNextCode a) [0..7] `using` parList rdeepseq)
