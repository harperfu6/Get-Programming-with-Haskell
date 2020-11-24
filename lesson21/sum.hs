-- コマンドライン引数を受け取り，受け取った数だけ数を受け取り，足す
import System.Environment
import Control.Monad -- for replicateM

main :: IO ()
main = do
    args <- getArgs
    let linesToRead = if length args > 0
                      then read (head args)
                      else 0 :: Int
    -- 繰り返しの回数とIOアクションを受け取り，アクションをその回数だけ繰り返す
    numbers <- replicateM linesToRead getLine
    let ints = map read numbers :: [Int]
    print (sum ints)

    ---------
    -- *Main> :set args 2
    -- *Main> main
    -- 4
    -- 10
    -- 14
    -- *Main>
    ---------