-- Pizza型シノニム
--- (size (diameter), cost)
type Pizza = (Double, Double)


-- 直径に基づいて面積を計算する
areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

-- １平方インチあたりの値段
costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

-- ２枚のピザの比較
comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                      then p1
                      else p2
    where
        costP1 = costPerInch p1
        costP2 = costPerInch p2

-- どちらのピザが安く，１平方インチあたりの値段を，メッセージとして返す
describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza "++ 
                             "is cheper at " ++
                             show costSqInch ++
                             " per square inch"
    where
        costSqInch = costPerInch (size, cost)

-- I/Oに対応したメイン部分
-- IOコンテキストは
--- 現実世界との接点があること
--- データの振る舞いが普通でない可能性があることを示す
main :: IO ()
main = do -- do表記と<-によって，IOコンテキストの中で，型IOを型aのように扱うことができる（まるでIOコンテキストの外にいるかのようにコードを記述できる）
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <- getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
    let pizza2 = (read size2, read cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn (describePizza betterPizza)
-- do表記はモナドのメンバであれば利用でき，コンテキスト内での計算に使う
--- returnはモナドのコンテキストに戻す（上記pustStrLnは戻り値がIO()なのでreturnの代わりとなっている）

-- モナド型クラスを利用すれば，幅広いコンテキストでうまくいく汎用的なプログラムを記述することが可能
-- do表記を利用すれば，基本的な関数は全て元のままで，異なるプログラムを記述できる


