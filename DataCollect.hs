{-# LANGUAGE OverloadedStrings #-}
module DataCollect
(
TelegramPost(..),
getJSON,
decodeList,
printJSON

) where

import Data.Aeson
import Data.Text
import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BC


data TelegramPost = TelegramPost
    {
    username    :: !Text,
    date        :: Int
    } deriving (Show)

testPost :: TelegramPost
testPost = TelegramPost
    {
    username = "Test",
    date = 1488
    }


instance FromJSON TelegramPost where
    parseJSON =  withObject "TelegramPost" $ \o ->
        TelegramPost  <$> ((o .: "from") >>= (.: "username"))
                    <*> o .: "date"


getJSON :: FilePath -> IO [B.ByteString]
getJSON jsonFile = BC.lines <$> B.readFile jsonFile

printJSON :: [Either String TelegramPost] -> IO [()]
printJSON = mapM (\o -> case o of
                        Left err -> putStrLn err
                        Right ps -> print ps)

decodeTelegram :: B.ByteString -> Either String TelegramPost
decodeTelegram t = eitherDecode t :: Either String TelegramPost

decodeList :: [B.ByteString] -> [Either String TelegramPost]
decodeList = fmap decodeTelegram



io :: IO [()]
io = do
    let jsonFile = "Autistengaleere.jsonl" :: FilePath
    f<- getJSON jsonFile
    printJSON . decodeList $ f
    
