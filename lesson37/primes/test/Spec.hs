import Test.QuickCheck
import Primes
import Data.Maybe -- isJustを使うため

-- 素数として返却した値がJust値かどうか
-- prop_validPrimesOnly val = if val < 0 || val >= last primes
prop_validPrimesOnly val = if val < 2 || val >= last primes
                            then result == Nothing
                            else isJust result
    where result = isPrime val

-- 素数判定されたものが本当に素数かどうか
prop_primesArePrime val = if result == Just True
                            then length divisors == 0
                            else True
    where
        result = isPrime val
        -- 素数判定されたものが，それ以下の数値リストで割り切れるものが1つもないか
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val-1)]

-- 素数「でない」判定されたものが本当に合成数かどうか
prop_nonPrimesAreComposite val = if result == Just False
                                    then length divisors > 0
                                    else True
    where
        result = isPrime val
        divisors = filter((==0) . (val `mod`)) [2 .. (val-1)]

-- 素因数分解の積が元の値になるかどうか
prop_factorsMakeOriginal val =
    if result == Nothing
        then True
        else product (fromJust result) == val
    where result = primeFactors val

-- 素因数分解について本当に全て素数かのテスト
prop_allFactorsPrime val =
    if result == Nothing
        then True
        else all (== Just True) resultPrime
    where
        result = primeFactors val
        resultPrime = map isPrime (fromJust result)


main :: IO ()
main = do
    quickCheck prop_validPrimesOnly -- デフォルトは100例のテスト
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
    quickCheck prop_factorsMakeOriginal
    quickCheck prop_allFactorsPrime
