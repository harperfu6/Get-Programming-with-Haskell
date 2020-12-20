-- Either型を使ったisPrime
--- MaybeよりもNothingの解釈性が向上する

-- エラー型の定義
data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
    show TooLarge = "Value exceed max bound"
    show InvalidValue = "Value is not a valid candidate for prime checking"


primes :: [Int]
primes = sieve [2 .. 10000] -- 素数リストの上限をつけておく

-- 素数リストの抽出
--- エラトステネスのふるい
sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
    where
        noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Either PrimeError Bool
isPrime n
    | n < 2 = Left InvalidValue 
    | n > 10000 = Left TooLarge
    | otherwise = Right (n `elem` primes)

-- Either型をString型にしてみやすくする関数
displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError


main :: IO ()
main = do
    print "Enter a number to test for primality"
    n <- read <$> getLine
    let result = isPrime n
    print (displayResult result)