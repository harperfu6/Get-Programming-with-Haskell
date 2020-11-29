module Lib
    ( isPalindrome
    , preprocess
    ) where

import Data.Char (isPunctuation)


preprocess :: String -> String
-- QuickCheckで失敗を起こすための，ある特定の文字しか削除しないパターン
-- preprocess text = filter (not . (`elem` ['!', '.'])) text
-- QuickCheckで成功するパターン
preprocess text = filter (not . isPunctuation) text


isPalindrome :: String -> Bool
isPalindrome text = cleanText == reverse cleanText
    where
        cleanText = preprocess text