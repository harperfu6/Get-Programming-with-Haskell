import Control.Monad -- for guard

-- Listモナドの書き方を使ってみる
powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1..n] -- <-によりリストの要素に注目がいく
    return (2^value)-- 取り出したListモナド内の要素に関数を適用し，returnでListモナドに戻す

-- もちろん下記のような書き方も可能
-- ただし，Listをデータ構造（コンテナ）として考えており，コンテキストが抽象化されない
powersOfTwoMap :: Int -> [Int]
powersOfTwoMap n = map (\x -> 2^x)[1..n]


-- モナドの書き方をした方が，List内の要素に集中できるので
-- 例えば下記のような場合は，そのような書き方の方が良い
powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1..n]
    let powersOfTwo = 2^value
    let powersOfThree = 2^value
    -- 1つのペアを返しているように見えるが，実際はペアのリストを返している
    return (powersOfTwo, powersOfThree)

-- また，リスト内包を使えばより簡単になる
powersOfTwoAndThreeNaihou :: Int -> [(Int, Int)]
powersOfTwoAndThreeNaihou n = [(powersOfTwo, powersOfThree) | value <- [1..n]
                                                            , let powersOfTwo = 2^value
                                                            , let powersOfThree = 3^value]




-- guard: リストモナド内でfilterをかけられる
-- guardはAlternative型の型クラス制約がある
--- Alternativeの鍵はemptyメソッド．
--- おおよそMonoidのmemptyのような動作をし，Listのemptyは[], MaybeのemptyはNothing，IOのemtpyは無い
evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1..n]
    guard(even value)
    return value