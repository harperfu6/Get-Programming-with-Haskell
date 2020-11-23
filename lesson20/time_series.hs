import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

-- 時系列データ
file1 :: [(Int, Double)]
file1 = [(1,200.1),(2,199.5),(3,199.4),(4,198.9),(5,199.0),(6,200.2),(9,200.3),(10,201.2),(12,202.9)]

file2::[(Int,Double)]
file2=[(11,201.6),(12,201.5),(13,201.5),(14,203.5),(15,204.9),(16,207.1),(18,210.5),(20,208.8)]

file3::[(Int,Double)]
file3=[(10,201.2),(11,201.6),(12,201.5),(13,201.5),(14,203.5),(17,210.5),(24,215.1),(25,218.7)]

file4::[(Int,Double)]
file4=[(26,219.8),(27,220.5),(28,223.8),(29,222.8),(30,223.8),(31,221.7),(32,222.3),(33,220.8),(34,219.4),(35,220.1),(36,220.6)]


-- 時系列型の定義
--- 値は数値以外にも可能性があるので変数にしておく
--- 時間軸をインデックスにもつ
data TS a = TS [Int] [Maybe a]

-- TS型を作成する
createTS :: [Int] -> [a] -> TS a
-- 引数は有効な値からなる限定的な集合のみを表すと想定している
createTS times values = TS completeTimes extendedValues
    where
        completeTimes = [minimum times .. maximum times]
        timeValueMap = Map.fromList (zip times values)
        extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

-- createTSに渡せるようにfile*から取り出すヘルパー関数を定義する
fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
    where
        splitPairs = (unzip tvPairs)
        times = fst splitPairs
        values = snd splitPairs

-- Showインスタンスにしておく
--- まず時間と値のペアを表示する関数を作成する
showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
        where rows = zipWith showTVPair times values


-- 全てのファイルをTS型に変換する
ts1 :: TS Double
ts1 = fileToTS file1

ts2 :: TS Double
ts2 = fileToTS file2

ts3 :: TS Double
ts3 = fileToTS file3

ts4 :: TS Double
ts4 = fileToTS file4


-- SemigroupとMonoidでTS型のデータを組み合わせる
--- やりたいことは，TS a -> TS a -> TS a
--- Semigroupの<>は（Semigroup a => a -> a -> a）なので上記の一般化である
-- さて，TS型をどう組み合わせるか
--- 1つ目の課題: ファイルごとに時間軸がきちんと区切られている保証はない
--- 2つ目の課題: 別のファイルに同じデータ点が含まれる場合がある
--- 解決策-> インデックスを活用し，時間軸の整合性を保つ．重複インデックスは後ろのファイルの値を参照する

-- また，時系列データの組み合わせは Map k v となるが，
-- TSの値は(k Maybe v)である，したがって Map k v に挿入できるようにするヘルパー関数を用意する
insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
--- Maybe値が欠損している場合は元のMapを返す
insertMaybePair myMap (_,Nothing) = myMap
--- 実際の値がある場合はJustコンテキストから取り出してMapを挿入する
insertMaybePair myMap (key,(Just value)) = Map.insert key value myMap

-- 2つのTS型を組み合わせて新しいTS型を作成する
combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where
        -- bothTimes = mconcat [t1, t2]
        completeTimes = [(minimum t1)..(maximum t2)]
        tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
        updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
        combinedValues = map (\v -> Map.lookup v updatedMap) completeTimes

-- 上記がSemigroupの<>実装となる
instance Semigroup (TS a) where
    (<>) = combineTS

-- 複数結合することを考えてMonoidインスタンスにする
--- 要は，[Ts a] -> Ts a
instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

-- Monoidを使って全てのファイルを結合する
tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]


----------------------------------------
-- ここからはTS型を使って時系列分析する
----------------------------------------

-- 平均
mean :: (Real a) => [a] -> Double
mean xs = total/count
    where
        -- 除算するためにrealToFracを使う
        total = (realToFrac . sum) xs
        count = (realToFrac . length) xs

-- TS型用
--- 全てNothingなどの可能性があるので，Maybe型で返す
meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing -- 値が空の場合
meanTS (TS times values) = if all (== Nothing) values then Nothing
                           else Just avg
    where
        justVals = filter isJust values -- Justのみ取り出す
        cleanVals = map (\(Just x) -> x) justVals
        avg = mean cleanVals

-- 比較関数の定義
--- 比較関数のための型シノニム
type CompareFunc a = (a -> a -> a)
type TSCompareFunc a = ((Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a))

makeTSCompare :: Eq a => CompareFunc a -> TSCompareFunc a
--- newFuncを生成して返す
makeTSCompare func = newFunc
    where
        --- 両方の値がNothingの場合
        newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
        --- いずれか値がNothingの場合
        newFunc (_, Nothing) (i, val) = (i, val)
        newFunc (i, val) (_, Nothing) = (i, val)
        --- 比較関数としての動作
        newFunc (i1, Just val1) (i2, Just val2) = if (func val1 val2) == val1
                                                  then (i1, Just val1)
                                                  else (i2, Just val2)

-- TS型の全ての値を比較すジジェネリック関数
compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS func (TS [] []) = Nothing
compareTS func (TS times values) = if all (== Nothing) values
                                   then Nothing
                                   else Just best
    where
        pairs = zip times values
        best = foldl (makeTSCompare func) (0, Nothing) pairs

--- 比較関数を作ってしまえば最小値/最大値は簡単に作れる
minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max


-- 時系列分析のための応用関数

-- 前日との差をとるdiff関数
--- 型は，TS a -> TS a で良さそう
--- さらに差を撮るにはNum方である必要がありそうなので，Num a => TS a -> TS a とする
--- さらに取り扱う値はMaybeとなるので，diffPair :: Num a => Maybe a -> Maybe a -> Maybe a とする
diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair _ Nothing = Nothing
diffPair (Just x) (Just y) = Just (x - y)

--- TSのdiffをとる
diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues) -- 差をとるので先頭には必ずNothingがくる
    where
        shiftValues = tail values
        diffValues = zipWith diffPair shiftValues values

-- 移動平均
--- diffと違うのはウィンドウ幅をとり広い範囲での平均をとること（従って少しなめからになる）
--- 型は，maTS :: (Real a) => TS a -> Int -> TS Double になりそう

--- 早速実装していくが，移動平均データとなる[Maybe a]リストに対処する部分を抽象化すると論理的に考えやすくなる
meanMaybe :: (Real a) => [Maybe a] -> Maybe Double
meanMaybe vals = if any (== Nothing) vals
                 then Nothing
                 else (Just avg)
    --- fromJustもData.Maybeの関数であり，(\(Just x) -> x)に相当
    where avg = mean (map fromJust vals)

movingAvg :: (Real a) => [Maybe a] -> Int -> [Maybe Double]
movingAvg [] n = []
movingAvg vals n = if length nextVals == n -- 端っこの方だと移動平均を算出するための幅が取れない
                   then (meanMaybe nextVals):(movingAvg restVals n)
                   else []
    where
        nextVals = take n vals
        restVals = tail vals

--- 最終的なTSが「中心化」されるようにする
--- 要は移動平均は端っこの方はNothingが入るようにする
maTS :: (Real a) => TS a -> Int -> TS Double
maTS (TS [] []) n = TS [] []
maTS (TS times values) n = TS times smoothedValues
    where
        ma = movingAvg values n
        nothings = take (n `div` 2) (repeat Nothing)
        smoothedValues = mconcat [nothings, ma, nothings]





-- テスト
test = do
    print testFileToTS1 
    print testMeanTSAll
    print testMakeTSCompare
    print testMinTS
    print testMaxTS
    print testMeanDiffTS
    print testMaTS


testFileToTS1 = fileToTS file1
testMeanTSAll = meanTS tsAll
testMakeTSCompare = makeTSCompare max (3, Just 200) (4, Just 10)
testMinTS = minTS tsAll
testMaxTS = maxTS tsAll
testMeanDiffTS = meanTS (diffTS tsAll)
testMaTS = maTS tsAll 5