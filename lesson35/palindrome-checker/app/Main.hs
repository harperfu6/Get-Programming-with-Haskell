-- {-# LANGUAGE OverloadedStrings #-}
-- Data.Textに対応させるため必要
-- ただし，cabalファイルに下記を追加すれば良い
--- extensions: OverloadedStrings
module Main where

import Lib
import Data.Text as T -- ここでは修飾つきインポートは使用しない
import Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
    text <- TIO.getLine
    let response = if isPalindrome text
                   then "it is!"
                   else "it's not!"
    TIO.putStrLn response

-- .cabalに依存ライブラリを追加
-- stack setup (stackがGHCの正しいバージョンを使用するように設定する)
--- GHCのバージョンはstack.yaml(stack resolver)で設定されている
--- 現在の最新のresolverのバージョンはStackageで確認できる
--- 特定のresolverに関す情情報はStackageのURLにresolverのバージョンを追加する
-- stack build（プロジェクトのビルド）
--- 実行ファイル名は，cabalのexecutableに定義されている
-- stack exec palindrome-checker-exe