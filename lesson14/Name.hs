-- 下記だとNameはHaskellからすると(String, String)にすぎないので
--- Ord実装するとStringが実装しているOrdと競合してエラーとなる
-- そういう場合は新しくデータ型を宣言する
data Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
    compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

names :: [Name]
names = [
          Name ("Emil", "Cioran")
        , Name ("Eugene", "Thacker")
        , Name ("Friedrich", "Nietzshce")
        ]