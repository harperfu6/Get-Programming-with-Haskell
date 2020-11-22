import qualified Data.Map as Map
import Data.Maybe -- for isJust
import Data.List -- for intercalete

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

-- Mapのkeyとなるもの
ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs


possibleDrawers :: [Int]
possibleDrawers = [1..50]

-- Map.Mapはkeyに該当するものがない場合に備え，Maybe型のvalueを返す
getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog

-- 「欠損値を含んでいる可能性がある」Organリスト
availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

-- 指定したOrganインスタンスを数える
countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ available = length (filter (\x -> x == Just organ) available)


-- NothingをOrganリストに入れるのは煩わしいのでfilter処理を入れる
justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isJust availableOrgans

-- Justが表示されるのも煩わしいので外して表示する
showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

-- 最後の仕上げでリストの見た目を"xx, xx, xx"となるようにする
cleanList :: String
cleanList = intercalate "," organList

-------------
--- 脳はバット（Vat）に入れる
--- 心臓は保冷器（Cooler）に入れる
--- 脾臓と腎臓は袋（Bag）に入れる。
-- さらに
--- バットと保冷器は研究室（Lab）に置く
--- 袋はキッチン（Kitchen）に置く。
-------------
-- 格納するコンテナに格納するもの（Organ）を引数に取らせる
data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ


-- 置く場所
data Location = Lab | Kitchen | Bathroom deriving Show

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

-- 渡されたパーツを正しいコンテナに入れた上で，正しい場所におく
process :: Organ -> (Location, Container)
process organ = placeInLocation (organToContainer organ)

-- コンテナと場所を受け取り，レポートを出力する
report :: (Location, Container) -> String
report (location, contaier) = show contaier ++ " in the " ++ show location

processRequest :: Int -> Map.Map Int Organ -> String
-- 下記だとorganがMaybe型なのでエラーとなる，よってprocessAndReportを定義する
-- processRequest id catalog = report (process organ)
processRequest id catalog = processAndReport organ
    where organ = Map.lookup id catalog

processAndReport :: (Maybe Organ) -> String
processAndReport (Just organ) = report (process organ)
processAndReport Nothing = "error, id not found"


-- テスト
test = do
    print testCountOrgan1
    print testCountOrgan2
    print testProcessRequest1
    print testProcessRequest2

testCountOrgan1 = countOrgan Brain availableOrgans == 1
testCountOrgan2 = countOrgan Heart availableOrgans == 2

testProcessRequest1 = processRequest 13 organCatalog == "Brain in a vat in the Lab"
testProcessRequest2 = processRequest 12 organCatalog == "error, id not found"