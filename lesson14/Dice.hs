-- 型インスタンスを表示できるようにしておく
-- data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Show)
-- インスタンを表示するだけでは面白くないので下記でShowを実装する
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6

-- 各面の番号を英語で表示できるようにする
-- Showを実装する
-- SixSideedDieのshowとするためにインスタンス内でメソッドとして定義する
-- show関数として定義してしまうと，定義が重複してしまう
instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

-- サイコロの比較のためのクラス実装をする
-- EqはOrdのスーパークラスなので，
--- Ordの全てのインスタンスがEqのインスタンスである必要がある（等号が定義できないと比較もできないよね，ということ）
--- なのでまずEqを定義しておく
--- Eqを定義するのに必要なメソッドは実装はHackage|Hoogleで確認する
---- Minimal complete definition
---- 今回は (==) | (/=) のいずれか実装すれば良さそう
--- :infoは頼もしいが，完全なドキュメントではない
instance Eq SixSidedDie where
    (==) S1 S1 = True
    (==) S2 S2 = True
    (==) S3 S3 = True
    (==) S4 S4 = True
    (==) S5 S5 = True
    (==) S6 S6 = True
    (==) _ _ = False

--- 続いてOrdを実装する
---- hoogleからcompareのみ実装すれば良さそう
---- ただし実装が大変なのでとりあえず保留....
instance Ord SixSidedDie where
    compare S6 S6 = EQ
    compare S6 _ = GT
    compare _ S6 = LT
    compare S5 S5 = EQ

-- これまで自分で定義してきたが，正直derivingで派生させる方が賢明
--- data で定義した順番がOrdの順番になる
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Eq, Ord)

-- そもそもサイコロのような順番があるものはEnum型を利用するのが分がある
--- Enumが実装すべきは，toEnum & fromEnum
instance Enum SixSidedDie where
    toEnum 0 = S1
    toEnum 1 = S2
    toEnum 2 = S3
    toEnum 3 = S4
    toEnum 4 = S5
    toEnum 5 = S6
    toEnum _ = error "No such value"

    fromEnum S1 = 0
    fromEnum S2 = 1
    fromEnum S3 = 2
    fromEnum S4 = 3
    fromEnum S5 = 4
    fromEnum S6 = 5

--- Enumも自分で定義するより，derivingで派生させる方が安全
---- 自分で定義した場合．[S1..]のような定義でエラーが起きる
---- これは実装すべきメソッドが足りていないから？
data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum)