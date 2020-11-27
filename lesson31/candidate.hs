import qualified Data.Map as Map -- for candidateDB
-- 会社に応募してきた人のデータを処理して，面接に合格したかを判断する

-- コードレビュー/組織文化どの適合性の型
-- Ordは左から小さい順となる
data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

-- 応募者の学歴
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

-- 応募者の面接内容
data Candidate = Candidate {
      candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree
} deriving Show

-- 応募者が最低条件をクリアしているか
viable :: Candidate -> Bool
viable candidate = all (== True) tests
    where
        tests = [passedCoding, passedCultureFit, educationMin]
        passedCoding = codeReview candidate > B
        passedCultureFit = cultureFit candidate > C
        educationMin = education candidate >= MS


-- 応募者のデータを手動で入力できるコマンドラインツール
--- まずは入力を読み取りIO型を返すもの
readInt :: IO Int
readInt = getLine >>= (return . read)

readGrade :: IO Grade
readGrade = getLine >>= (return . read)

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

-- コマンドラインから応募者のデータを読み取る
readCandidate :: IO Candidate
readCandidate = do
    putStrLn "enter id:"
    cId <- readInt
    putStrLn "enter code grade:"
    codeGrade <- readGrade
    putStrLn "enter culture fit grade:"
    cultureGrade <- readGrade
    putStrLn "enter education:"
    degree <- readDegree
    return (Candidate{
          candidateId = cId
        , codeReview = codeGrade
        , cultureFit = cultureGrade
        , education = degree
    })

-- 応募者データを受け取り，応募者が合格したかどうかをチェック
assesCandidateIO :: IO String
assesCandidateIO = do
    candidate <- readCandidate
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement


-- 応募者のMapを操作する
--- 応募者サンプルデータ
candidate1 :: Candidate
candidate1 = Candidate {
      candidateId = 1
    , codeReview = A
    , cultureFit = A
    , education = BA
}

candidate2 :: Candidate
candidate2 = Candidate {
      candidateId = 2
    , codeReview = C
    , cultureFit = A
    , education = PhD
}

candidate3 :: Candidate
candidate3 = Candidate {
      candidateId = 3
    , codeReview = A
    , cultureFit = B
    , education = MS
}

-- candidateDBに追加する
candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

-- Map型にしたことによって，Maybeで返されるので，それ用のassesを用意する
-- ここで重要なのが，
-- IOコンテキストの場合と本質的に何も変わっていないこと．
-- つまり，コンテキストの中身を取り出してまたコンテキストにいれる（viableとreturn部分）は同じである
-- しかも今回は欠損値のことを全く考えず問題に取り組める
--- ex: assesCandidataMaybe 4 はNothingとなる
assesCandidateMaybe :: Int -> Maybe String
assesCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement

-- 今度はListコンテキストで処理してみる
candidates :: [Candidate]
candidates = [candidate1, candidate2, candidate3]

-- Listコンテキスト用のasses
-- ここで重要なのが，
-- Listの場合mapを使って書けるが，Listコンテキストとして記述した方がモナドとしての一般化ができるということ
--- 実行例:
--- > assesCandidataList candidates
--- ["failed","failed","passed"]
assesCandidateList :: [Candidate] -> [String]
assesCandidateList candidate = do
    candidate <- candidate
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement


-- ここですごいのが，
-- これまでのコンテキストをモナドの型制約さえあれば，モナドとして一般化できること
-- 実行例:
-- 
assesCandidate :: Monad m => m Candidate -> m String
assesCandidate candidates = do
    candidate <- candidates
    let passed = viable candidate
    let statement = if passed
                    then "passed"
                    else "failed"
    return statement