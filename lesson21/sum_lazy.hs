-- 入力数をコマンドライン引数に入れなくてもよくする

toInts :: String -> [Int]
toInts = map read . lines

main :: IO ()
main = do
    -- getContentsはEOFに達するまで読み込む．
    userInput <- getContents
    -- -- toIntsはChar型リストをInt型のリストに変換する
    let numbers = toInts userInput
    print (sum numbers)