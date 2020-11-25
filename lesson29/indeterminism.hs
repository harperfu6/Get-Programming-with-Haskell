-- リストの「コンテナ」と「コンテキスト」の両面をもつ様子をみる
--- コンテナ: 保持したいデータ型をつなぎ合わせたもの
--- コンテキスト: 可能性の集合を表す．考えうる限りの値を保持するという「非決定論的な計算」をする
---- 例えば[Int]に対して考えられる値は多数存在するので，
---- 関数（Int -> Int -> Int）を適用する際は，その演算の結果として考えられるものを全て計算する必要がある

-- クイズ番組の例
--- ドアと箱の賞金の組み合わせ
doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrize :: [Int]
boxPrize = [500,20000]

-- 決定論的なコンテキストではドアへの賞金のパスは1つしかない
-- totalPrize :: Int
-- totalPrize (+) doorPrize boxPrize

-- 非決定論的なコンテキストでは可能性を考える
--- [Int]はコンテナではなく，非決定論的なIntを表す
--- pureは決定論的な(+)を，リストの非決定論的なコンテキストに配置することを意味する
totalPrize :: [Int]
totalPrize = pure (+) <*> doorPrize <*> boxPrize


----------------------------
-- 最初のN個の素数の生成
----------------------------
-- 1. まず２からnまでのリストを定義する
-- 2. 非素数（合成数）を全て洗い出す
-- 3. リストから合成数でない要素を全て抜き出す

-- ここで合成数を算出するが，
-- 基本的には[2..n]の各要素に同じリストを乗算すれば合成数は簡単に作成できる
-- > pure (*) <*> [2..4] <*> [2..4]
-- [4,6,8,6,9,12,8,12,16]
-- ただしこのリストは範囲外(4より上)の数字と重複する数字が含まれるので非効率
-- だが気にせず書き下すと下記の通り
primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite twoThroughN
    where
        twoThroughN = [2..n]
        composite = pure (*) <*> twoThroughN <*> twoThroughN
        isNotComposite = not . (`elem` composite)


----------------------------
-- 大量のテストデータを素早く生成
----------------------------
data User = User {
      name :: String
    , gamerId :: Int
    , score :: Int
} deriving Show

-- ユーザ名のリストがあり，一般的なユーザ名と特定の状況で問題になりそうなユーザ名が含まれているとする
-- Listをコンテキストとして考えた場合，testNamesは考えうる名前のリストを表す

--- testNamesはデータの一部のユーザ名
testNames :: [String]
testNames = [
     "John Smith"
    ,"Robert'); DROP TABLEStudents;--"
    ,"Christina NULL"
    ,"Randall Munroe"]

-- testIdsは様々なID値
testIds :: [Int]
testIds = [1337, 0123, 999999]

-- testScoresは様々なスコア
testScores :: [Int]
testScores = [0, 100000, -99999]

-- 非決定論的な計算で組み合わせを計算するには4*3*3=36個のエントリが必要
-- リストのApplicativeの性質を利用すれば簡単
testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores