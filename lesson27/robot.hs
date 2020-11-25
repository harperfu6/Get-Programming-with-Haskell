-- ロボット部品を製造する仕事
import qualified Data.Map as Map -- for partsDB

-- ロボット部品
data RobotPart = RobotPart {
      name :: String
    , description :: String
    , cost :: Double
    , count :: Int
} deriving Show

-- ロボット部品の例
leftArm :: RobotPart
leftArm = RobotPart {
      name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.00
    , count = 3
}

rightArm :: RobotPart
rightArm = RobotPart {
      name = "right arm"
    , description = "right arm for face punching!"
    , cost = 1025.00
    , count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
      name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
}

-- RobotPartに含まれている情報をHTMLスニペットとしてレンダリングする
type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [
      "<h2>", partName, "</h2>"
    , "<p><h3>desc</h3>", partDesc
    , "</p><p><h3>cost</h3>", partCost
    , "</p><p><h3>count</h3>", partCount
    , "</p>"
    ]
    where
        partName = name part
        partDesc = description part
        partCost = show (cost part)
        partCount = show (count part)


------------------------------------
-- RobotPartをHTMLスニペットに変換したいと考える
-- 4つのシナリオを見てみる

-- Functor型クラスは「写像できるもの」として考えることがポイント
------------------------------------

-- Map型を使ってpartsDBを作成する（RobotPartの内部データベース）
--- MapはList型から生成され，Maybe型を返すため，Functorの例にはもってこい
partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where
        keys = [1,2,3]
        vals = [leftArm,rightArm,robotHead]
        keyVals = zip keys vals

-- partsDBをベースとするWebサイトがあるとする
--- Webページに挿入したい部品のIDを含んだリクエストを使用するのが合理的
--- HTMLを受け取り，Webページのテンプレートに挿入するIOアクションを定義
--- また，HTMLスニペットを生成するデータモデルも多数存在することが想定され，
--- これらはエラーになるかもしれないので欠損値に対応できるようにする．
--- つまり型は，Maybe Html -> IO ()
--- 従ってpartsDBからMaybe Htmlとして渡す仕組みが必要

-- partsDBからRobotPartを取得する
partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

-- MaybeはFunctorなので，<$>を使ってRobotPartをHTMLに変換する一方でMaybeのコンテキストに留めることができる
-- これで，Maybe Htmlとして渡せる
partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

-- 全ての部品からなるインデックスページを作成したとする．
--- 部品リストはpartsDBから取り出す
allParts :: [RobotPart]
allParts = map snd (Map.toList partsDB)

-- ListはFunctorのインスタンス．リストのfmapはmap関数のことである．
--- <$>を使って部品のリストにrenderHtmlを適用する
allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts
-- これはつまり下記と同じである
-- allPartsHtml = map renderHtml allParts

-- partsDBの値をRobotPartではなくHTMLとして扱った方が今回は楽
--- Functorを使えば，Mapコンテキストの中で変換できる
--- MapのFunctorが関心をもつのは，Mapの値だけである
htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB


-- IOコンテキストから得られるRobotPartをシミュレートする
-- returnはモナドコンテキストにいれる．今回はIOなのでIOコンテキストにいれる
leftArmIO :: IO RobotPart
leftArmIO = return leftArm

-- IO RobotPartを，IO Htmlに変換する
htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO