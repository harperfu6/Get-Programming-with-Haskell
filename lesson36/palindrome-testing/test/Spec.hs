import Lib
import Data.Char (isPunctuation) -- 「句読点不変」のテストケース作成のため
import Test.QuickCheck


-- ユニットテストのための単純な関数
assert :: Bool -> String -> String -> IO ()
assert test passStatement failStatement = if test
                                          then putStrLn passStatement
                                          else putStrLn failStatement


-- preprocessを呼び出すと，句読点が含まれていないものが生成されるはずである，のテスト
prop_punctuationInvariant text = preprocess text == preprocess noPuncText
    where
        noPuncText = filter (not . isPunctuation) text

-- QuickCheckはプロパティテストの概念に基づいて構築されている
--- 要は，関数が備えているはずのプロパティを指定すると，QuickCheckがそれらの値自自動的に生成して関数でテストし，
--- 指定されたプロパティをその関数が満たしているかどうかを検証する
--- .cabalのtest-suiteのbuild-dependsに下記を追加
--- QuickCheck

-- QuickCheckによるテスト（今回は，isPalindromeにいれるインプットのテストをしている）
--- テスト対象の関数を渡す
--- すると失敗するテストケースを吐き出してくれる
--- Ex:
---- *** Failed! Falsified (after 5 tests and 2 shrinks):     
---- ":"
main :: IO ()
main = do
    -- quickCheck prop_punctuationInvariant
    -- デフォルトでは100ケースなので引数でテストケースの個数を指定する
    quickCheckWith stdArgs {maxSuccess = 1000} prop_punctuationInvariant
    putStrLn "done!"


-- main :: IO ()
-- main = do
--     putStrLn "Running tests.."
--     assert (isPalindrome "racecar") "passed 'racecar'" "FAIL: 'racecar'"
--     assert (isPalindrome "racecar!") "passed 'racecar!'" "FAIL: 'racecar!'"
--     assert ((not . isPalindrome) "cat") "passed 'cat'" "FAIL: 'cat'" -- 間違っていることをテスト
--     assert (isPalindrome "racecar.") "passed 'racecar.'" "FAIL: 'racecar.'"
--     -- ここまで，いろんなパターンをテストしてみたが全て手動で追加していくのは大変，，，
--     -- -> プロパティテストの出番！
--     putStrLn "done!"

-- stack test を実行