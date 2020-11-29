import Control.Monad -- for guard
import Control.Applicative -- for Alternative

-- 下記特徴をもつクエリツールを作成する
--- リレーショナルデータでクエリを実行するためのおなじみにのインタフェースをHaskell実実装する
--- 強く型指定されている
--- 遅延評価を使用することで，クエリを実行せずにやりとりできる
--- 他のHaskell関数からシームレスに呼び出すことができる

----------------------------
-- 生徒/教師/科目/履修からなるデータ
----------------------------
-- 名前（生徒/教師）の定義
data Name = Name {
      firstName :: String
    , lastName :: String
}
instance Show Name where
    -- 名前の間にスペースを空けて表示するように指定
    show (Name first last) = mconcat [first, " ", last]

-- 生徒の学年の定義
--- Enumは連続的な順序をつけられ，+1/-1などの演算が可能になる
data GradeLevel = Freshman | Sophomore | Junior | Senior deriving (Eq, Ord, Enum, Show)

-- 生徒データ型
data Student = Student {
      studentId :: Int
    , gradeLevel :: GradeLevel
    , studentName :: Name
} deriving Show

-- 生徒データリスト
students :: [Student]
students = [ (Student 1 Senior (Name "Audre" "Lorde"))
            ,(Student 2 Junior (Name "Leslie" "Silko"))
            ,(Student 3 Freshman (Name "Judith" "Butler"))
            ,(Student 4 Senior (Name "Guy" "Debord"))
            ,(Student 5 Sophomore (Name "Jean" "Baudrillard"))
            ,(Student 6 Junior (Name "Julia" "Kristeva"))]

-- 教師データ型
data Teacher = Teacher {
      teacherId :: Int
    , teacherName :: Name
} deriving Show

-- 教師データリスト
teachers :: [Teacher]
teachers = [ Teacher 100 (Name "Simone" "DeBeauvior")
            ,Teacher 200 (Name "Susan" "Sontag")]

-- 科目データ型
data Course = Course {
      courseId :: Int
    , courseTitle :: String
    , teacher :: Int
} deriving Show

-- 科目データリスト
courses :: [Course]
courses =[ Course 101 "French" 100
         , Course 201 "English" 200]





-- クエリの基本関数である，(select,where)/joinなどを作っていく
-- 関数の競合を避けるため，先頭に_をつける

-- select
-- レコードから必要なカラムを取得
--- fmapと型が類似しているが，型シグネチャのせいでfmapほどの威力は無い．
_select :: (a->b) -> [a] -> [b]
_select prop vals = do
    val <- vals
    return (prop val)

-- where
_where :: (a -> Bool) -> [a] -> [a]
_where test vals = do
    val <- vals
    guard (test val)
    return val

--- whereテスト用の，特定の文字列で始まっているかを判定するためのヘルパー
startsWith :: Char -> String -> Bool
startsWith char string = char == (head string)


-- join
--- aとbそれぞれのテーブルから共通のカラムにするための関数を適用させる
--- 基本的な方針としては，それぞれのテーブルのレコードの直積を計算し，条件を満たすものを取得
--- 共通カラムは等しいか判定する必要があるので，Eqの型クラスをつける
_join :: Eq c => [a] -> [b] -> (a -> c) -> (b -> c) -> [(a, b)]
_join data1 data2 prop1 prop2 = do
    d1 <- data1
    d2 <- data2
    let dpairs = (d1, d2)
    guard ((prop1 (fst dpairs)) == (prop2 (snd dpairs)))
    return dpairs



-- クエリっぽく書けるように関数を定義する
--- 下記のように書けることを目指す
-- (_select (teacherName . fst))
-- (_join teachers courses teacherId teacher)
-- (_where ((== "English") . courseTitle . snd))

-- クエリ上の処理を外側から書き下していくだけ
_hinq selectQuery joinQuery whereQuery = (
    \joinData ->
        (\whereResult ->
            selectQuery whereResult)
        (whereQuery joinData)
    ) joinQuery

-- _hinqを使った例
finalResult :: [Name]
finalResult = _hinq
                (_select (teacherName . fst))
                (_join teachers courses teacherId teacher)
                (_where ((== "English") . courseTitle . snd))

--- 教師のファーストネームを取り出す
--- ただし現状だとwhere文も渡さないといけない（Haskellではデフォルト引数を取れない）
teacherFirstName :: [String]
teacherFirstName = _hinq
                    (_select firstName)
                    finalResult
                    (_where (\_ -> True))


-- _select, _where, _joinは全てモナドに対応できることを利用して，
-- （これまでリストのDBをモナド形式で書いてきたのはそのためでもある）
--- guardを使う場合は，Alternativeの型クラス制約をつける
--- _select :: Monad m => (a -> b) -> m a -> m b
--- _where :: (Monad m, Alternative m) => (a -> Bool) -> m a -> m b
--- _join :: (Monad m, Alternative m ,Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
-- where文をなしでもクエリを叩けるようなコンストラクトも用意する
--- 下記右について，引数は左から，_select, _join, _whereについてである
--- もう1つのインスタンスは_where文については無い
data HINQ m a b = HINQ (m a -> m b) (m a) (m a -> m a)
                | HINQ_ (m a -> m b) (m a)

runHINQ :: (Monad m, Alternative m) => HINQ m a b -> m b
runHINQ (HINQ sClause jClause wClause) = _hinq sClause jClause wClause
runHINQ (HINQ_ sClause jClause) = _hinq sClause jClause (_where (\_ -> True))

-- HINQクエリの実行
--- アウトプット型は，リストモナド 入力（テーブル）型 出力型
query1 :: HINQ [] (Teacher, Course) Name
query1 = HINQ
            (_select (teacherName . fst))
            (_join teachers courses teacherId teacher)
            (_where ((== "English") . courseTitle . snd))







-- テスト
main = do
    print testSelect1
    print testSelect2
    print testWhere
    print testJoin


testSelect1 = _select (firstName . studentName) students
testSelect2 = _select gradeLevel students
testWhere = _where (startsWith 'J' . firstName) (_select studentName students)
testJoin = _join teachers courses teacherId teacher