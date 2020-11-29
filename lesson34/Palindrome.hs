-- isPalindromeのみをエクスポート
module Palindrome (isPalindrome) where

-- Data.Charのうち必要な関数のみをインポート
import Data.Char (toLower, isSpace, isPunctuation)


-- 本体
stripWhiteSpace :: String -> String
stripWhiteSpace text = filter (not . isSpace) text

stripPunctuation :: String -> String
stripPunctuation text = filter (not . isPunctuation) text

toLowerCase :: String -> String
toLowerCase text = map toLower text

preprocess :: String -> String
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: String -> Bool
isPalindrome text = cleanText == (reverse cleanText)
    where
        cleanText = preprocess text