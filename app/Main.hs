{-
Simple program demonstrating how to extract register data from data
downloaded from YNAB (https://www.youneedabudget.com/)
You'll need the following Haskell packages in your .cabal file in addition to "base":

* bytestring
* cassava
* directory
* filepath
* text
* vector
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString (readFile, stripPrefix)
import           Data.Csv ((.:), FromNamedRecord(..), Header, decodeByName)
import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Vector
import           System.Directory (getHomeDirectory)
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           Text.Printf (printf)

data Row =
    Row
        !Text -- Account
        !Text -- Flag
        !Text -- Date
        !Text -- Payee
        !Text -- Category Group/Category
        !Text -- Category Group
        !Text -- Category
        !Text -- Memo
        !Text -- Inflow
        !Text -- Outflow
        !Text -- Cleared
    deriving Show

instance FromNamedRecord Row where
    parseNamedRecord m = Row
        <$> m .: "Account"
        <*> m .: "Flag"
        <*> m .: "Date"
        <*> m .: "Payee"
        <*> m .: "Category Group/Category"
        <*> m .: "Category Group"
        <*> m .: "Category"
        <*> m .: "Memo"
        <*> m .: "Inflow"
        <*> m .: "Outflow"
        <*> m .: "Cleared"

main :: IO ()
main = do
    args <- getArgs
    for_ args $ \arg -> do
        csvPath <- expandHomeDir arg
        dumpCsvFile csvPath

expandHomeDir :: FilePath -> IO FilePath
expandHomeDir ('~' : '/' : t) = do
    homeDir <- getHomeDirectory
    return $ homeDir </> t
expandHomeDir path = pure path

dumpCsvFile :: FilePath -> IO ()
dumpCsvFile csvPath = do
    bs <- stripUtf8BomPrefix <$> ByteString.readFile csvPath
    let rows = case (decodeByName bs :: Either String (Header, Vector Row)) of
                Left e -> error e
                Right (_, rows') -> rows'
    for_ rows $ \(Row account _ _ _ _ _ _ _ inflow outflow _) ->
        putStrLn $ printf "%s: inflow=%s outflow=%s" account inflow outflow

stripUtf8BomPrefix :: ByteString -> ByteString
stripUtf8BomPrefix bs = fromMaybe bs (ByteString.stripPrefix "\xef\xbb\xbf" bs)
