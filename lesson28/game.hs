-- Applicativeの一般的な用途の1つが，データを作成したいものの，
-- そのデータに必要な情報が全てMaybeやIOなどのコンテキストに含まれている場合

data User = User {
      name :: String
    , gamerId :: Int
    , score :: Int
} deriving Show

-- Maybeコンテキスト内でユーザを作成する
--- ソースから必要な情報を収集したいが，データが欠損している可能性がある場合
serverUsername :: Maybe String
serverUsername = Just "Sue"

serverGamerId :: Maybe Int
serverGamerId = Just 1337

serverScore :: Maybe Int
serverScore = Just 9001

-- ここからユーザを作成するには
-- User <$> serverUsername <*> serverGamerId <*> serverScore

-- Applicativeを使ってIO型からユーザを作成する
readInt :: IO Int
readInt = read <$> getLine

-- ポイントはdata Userにコンテキストを含めていない，ということ
-- Applicativeにより，同じコードを使って様々なコンテキストでユーザを簡単に作成できる
main :: IO ()
main = do
    putStrLn "Enter a username, gamerId and score"
    user <- User <$> getLine <*> readInt <*> readInt
    print user