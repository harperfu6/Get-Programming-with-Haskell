data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown deriving (Show, Eq)

-- Semigroupは同じ型のインスタンスを組み合わせるもの
-- 型クラスのインスタンスのユーザは，型クラスの法則が守られていることを想定するため，
-- 型クラスの法則が重要
instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b
        | a == b = a
        | all (`elem` [Red, Blue, Purple]) [a,b] = Purple
        | all (`elem` [Blue, Yellow, Green]) [a,b] = Green
        | all (`elem` [Red, Yellow, Orange]) [a,b] = Orange
        | otherwise = Brown