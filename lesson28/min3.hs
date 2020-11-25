-- コマンドラインから3つの数字を受け取り，最小値を返すツール

-- まず3つの引数から最小値を返す関数を定義する
minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

-- コマンドラインからInt型の値を読み取る
readInt :: IO Int
--- IOコンテキスト内にreadを適用
readInt = read <$> getLine

-- Int型をコマンドラインから読み取り小さいものを返すIOアクション
minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt


main :: IO ()
main = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")