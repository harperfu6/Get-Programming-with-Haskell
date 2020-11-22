-- Monoidを使った確率テーブルを構築する
-- Monoidは，Semigroupに単位元を追加したもの
--- これは，畳み込み関数を使って同じ型のリストを簡単に結合できるようになることを示す
--- mempy: リストでいう []
--- mappend: リストでいう (++)
--- mconcat: foldr mappend mempty で畳み込んでいるだけ

-- 事象と確率のシノニム
type Events = [String]
type Probs = [Double]

-- 確率テーブルデータ型
data PTable = PTable Events Probs

-- PTableを作成し，全ての確率の総和が１になるようにする
createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where
        normalizedProbs = map (\x -> x/totalProbs) probs
        totalProbs = sum probs

-- Show型クラスのインスタンスにしておく
showPair :: String -> Double -> String
--- 文字列の結合は++ではなくmconcatを使う，これはmconcatのみをサポートするテキスト型があるから
showPair event prob = mconcat [event, "|", show prob, "\n"]

-- PTableをShowのインスタンスにする
instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

-- 事象の直積の確率テーブルの作成
--- 2つのリストを組み合わせる関数と，2つのリストを受け取る
cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
-- l2の要素ごとにl1の要素を繰り返す
    where
        nToAdd = length l2
        -- l1を写像し要素のコピーをnToAdd個作成
        --- repeatしてtakeするのは常套手段
        repeatedL1 = map (take nToAdd . repeat) l1
        -- 前行で得られたリストのリストを結合する
        newL1 = mconcat repeatedL1
        -- l2を無限リストにし，zipWithを使って2つのリストを結合
        --- cycleはリストを無限リストにする
        cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    -- 事象の結合時に事象名をハイフンで繋ぐ
    where combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
-- 確率を結合するには，それらを掛け合わせる
combineProbs p1 p2 = cartCombine (*) p1 p2

-- PTableをSemigroupのインスタンスにする
instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1 -- PTableが空の場合
    (<>) (PTable [] []) ptable2 = ptable2 -- PTableが空の場合
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEventts newProbs
        where
            newEventts = combineEvents e1 e2
            newProbs = combineProbs p1 p2

-- Monoid型クラスを実装
--- mappendと<>が同じもの，必要なのは単位元（mempty）を特定すること
--- mconcatは何もしなくても手に入る
instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

-- うまくいくか確かめるためにPTableを2つ作成する
--- 1つ目は公平なコイン，2つ目はカラースピナー（スピナーごとに確率が異なる）
coin :: PTable
coin = createPTable ["heads", "tails"] [0.5, 0.5]

spinner :: PTable
spinner = createPTable ["red", "blue", "green"] [0.1, 0.2, 0.7]

-- テスト
combineTest1 = coin <> spinner
combineTest2 = mconcat [coin, coin, coin]
