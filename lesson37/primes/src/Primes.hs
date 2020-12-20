module Primes where

primes :: [Int]
primes = sieve [2 .. 10000] -- 素数リストの上限をつけておく

-- 素数リストの抽出
--- エラトステネスのふるい
sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
    where
        noFactors = filter (not . (== 0) . (`mod` nextPrime)) rest

-- 素数かどうか
--- ただし，入力が負の場合や上限値を超えている場合をハンドリングする必要がある
--- 単にFalseを返すのは意味合いが異なるのでMaybeとして実装する
--- (テスト後のステップ)テストによって0の扱いを決める必要が出てきたので修正する
isPrime :: Int -> Maybe Bool
isPrime n
    | n < 2 = Nothing -- 0以下からの変更
    | n >= last primes = Nothing
    | otherwise = Just (n `elem` primes)

-- 素因数分解する
--- まずは安全でない関数から
--- 数字と素数リストをわたし，素数リストのうち割り切れないものを削除する
unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) =
    if n `mod` next == 0
    then next:unsafePrimeFactors (n `div` next) (next:primes)
    else unsafePrimeFactors n primes

-- 上記安全でない関数をラッピングする
--- isPrimeで実装した観点で非安全である
primeFactors :: Int -> Maybe [Int]
primeFactors n
    | n < 2 = Nothing
    | n > last primes = Nothing
    | otherwise = Just (unsafePrimeFactors n primesLessThanN)
    where primesLessThanN = filter (<= n) primes