{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-} -- ToJSON/FromJSONクラスのインスタンスにするため

module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

import Control.Monad -- forM_

-- NOAAデータのResult型
data NOAAResult = NOAAResult {
      uid :: T.Text
    , mindate :: T.Text
    , maxdata :: T.Text
    , name :: T.Text
    , datacoverage :: Double
    , resultId :: T.Text -- idはそのまま使えないので下記でカスタムインスタンスを作成する
} deriving Show

instance FromJSON NOAAResult where
    parseJSON (Object v) = NOAAResult
                        <$> v .: "uid"
                        <*> v .: "mindate"
                        <*> v .: "maxdate"
                        <*> v .: "name"
                        <*> v .: "datacoverage"
                        <*> v .: "id"

-- NOAAデータのmetadata型
--- まずはresultset型を定義
data Resultset = Resultset {
      offset :: Int
    , count :: Int
    , limit :: Int
} deriving (Show, Generic)

instance FromJSON Resultset

--- metadata型の定義
data Metadata = Metadata {
    resultset :: Resultset
} deriving (Show, Generic)

instance FromJSON Metadata


-- 全てまとめてNOAAResponse型とする
data NOAAResponse = NOAAResponse {
      metadata :: Metadata
    , results :: [NOAAResult]
} deriving (Show, Generic)

instance FromJSON NOAAResponse


-- 結果の出力関数
printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
    forM_ results $ \result -> do
        let dataName = name result
        print dataName


main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse -- Maybe型コンテキストのため
    printResults noaaResults