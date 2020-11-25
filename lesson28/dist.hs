-- 2つの都市の名前を入力すると，それらの都市の距離が返されるコマンドラインアプリケーション
-- 2つの都市はDBに登録されていない可能性があるので，Maybe型となる
import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [
     ("Arkham",(42.6054,70.7829))
    ,("Innsmouth",(42.8250,70.8150))
    ,("Carcosa",(29.9714,90.7694))
    ,("NewYork",(40.7776,73.9691))]


-- 緯度経度から直線距離を計算する
--- haversineを使用する
--- 緯度経度をラジアンに変換する必要がある
toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
    where
        rlat = toRadians lat
        rlong = toRadians long

-- ２点間の距離
haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
    where
        (rlat1, rlong1) = latLongToRads coords1
        (rlat2, rlong2) = latLongToRads coords2
        dlat = rlat2 - rlat1
        dlong = rlong2 - rlong1
        a = (sin (dlat/2))^2 + cos rlat1 * cos rlat2 * (sin (dlong/2))^2
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
        earthRadius = 3961.0


-- ユーザから入力を受け付けるコマンドラインアプリケーション
--- 予期せぬことが起こる場合がある

-- 欠損の可能性をもつ距離を出力するIOアクション
printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

-- あとは要素をつなぎ合わせるだけなので，locationDBから取り出す場合下記のような型になることが想定される
-- Maybe LatLong -> Maybe LatLong -> Maybe Double
--- ここで，引数を(Nothing,_)や(_,Nothing)のようにパターンマッチングするのも良いが下記問題を抱えている
--- ・haversineのような関数が複数ある場合に毎回別途定義する必要がある
--- ・Maybe以外の別コンテキスト（IOなど）に対しても毎回別途定義する必要が
--- ここでFunctorの<$>を使ってみることを考えてみるが，例えば
--- maybeInc = (+) <$> Just 1 は
--- maybeInc :: Maybe (Integer -> Integer)
--- となるためコンテキストとなってしまう，要はコンテキスト内で（Integer -> Integer -> Integer）のようにしたい
--- そのためのアダプタがApplicativeの<*>である
--- 要はコンテキスト内部で関数適用できる仕組みを提供する．例えば
--- > (++) <$> Just "cats" <*> Just " and dogs"
--- Just "cats and dogs"
--- > (++) <$> Nothing <*> Just " and dogs"
--- Nothing
--- のように(++)をコンテキス内で適用し続けられるため，複数引数をとることができるようになる

main :: IO ()
main = do
    putStrLn "Enter the starting city name"
    startingInput <- getLine
    let startCity = Map.lookup startingInput locationDB
    putStrLn "Enter the destination city name"
    destInput <- getLine
    let destCity = Map.lookup destInput locationDB
    let distance = haversine <$> startCity <*> destCity
    printDistance distance





-- テスト
test = do
    print testHaversine

testHaversine = haversine (40.7776,73.9691)(42.6054,70.7829)