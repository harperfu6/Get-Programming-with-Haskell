{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-} -- ToJSON/FromJSONクラスのインスタンスにするため

module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

-- 一般的なjsonに近いHaskellデータ型を定義
data Book = Book {
      title :: T.Text
    , author :: T.Text
    , year :: Int
} deriving (Show, Generic)

-- JSONインスタンスにする
instance FromJSON Book
instance ToJSON Book

--- encode
myBook :: Book
myBook = Book {author="Will Kurt", title="Learn Haskell", year=2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

--- decode
rawJSON :: BC.ByteString
rawJSON = "{\"year\":2017,\"author\":\"Will Kurt\",\"title\":\"Learn Haskell\"}"

--- 型情報が誤ったJSONを受け取る時
wrongJSON :: BC.ByteString
wrongJSON = "{\"year\":2017,\"writer\":\"Will Kurt\",\"title\":\"Learn Haskell\"}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

bookFromWrongJSON :: Maybe Book
---- Nothingが返却される
bookFromWrongJSON = decode wrongJSON

--- 上記だとNothingが返され，何が悪かったか分からないのでEither型を使う場合は
---- eitherDecode関数を使う
bookFromWrongJSONEither :: Either String Book 
bookFromWrongJSONEither = eitherDecode wrongJSON


main :: IO ()
main = print "hi"
