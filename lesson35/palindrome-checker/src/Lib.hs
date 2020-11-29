-- {-# LANGUAGE OverloadedStrings #-}
-- cabalファイルに下記を追加すれば良い
--- extensions: OverloadedStrings
 -- for Data.Text（ダブルクォートで囲んだ文字列をText/ByteStringリテラルとして扱ってくれる）

 module Lib (isPalindrome) where

import qualified Data.Text as T -- Stringはリスト構造で文字列処理には遅いため
import Data.Char (toLower, isSpace, isPunctuation)

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace text = T.filter (not . isSpace) text

stripPunctuation :: T.Text -> T.Text
stripPunctuation text = T.filter (not . isPunctuation) text

preProcess :: T.Text -> T.Text
preProcess = (stripWhiteSpace . stripPunctuation. T.toLower)

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == (T.reverse cleanText)
    where
        cleanText = preProcess text