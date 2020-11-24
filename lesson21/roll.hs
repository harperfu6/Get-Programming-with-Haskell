import System.Random

minDie :: Int
minDie = 1

maxDie :: Int
maxDie = 6

main :: IO ()
main = do
    -- randomRIOは毎回同じ値を返さないので参照透過性が崩れており関数ではない．
    --- getLineやputStrLnと同様I/Oアクションである
    dieRoll <- randomRIO (minDie, maxDie)
    putStrLn (show dieRoll)