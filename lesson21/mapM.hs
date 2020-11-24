import System.Environment

main :: IO ()
main = do
    args <- getArgs
    -- まず，argsは通常のリストではなく，pusStrLnは通常の関数ではない
    -- そこでIOコンテキストでListを操作するMapが必要
    -- mapM putStrLn args
    -- しかしmapMは最終的にIOコンテキストリストをアウトプットし，IOコンテキスト単体（IO ()）に合わないので，
    -- 結果を破棄することを示す(_)をつけたバージョンを使う
    mapM_ putStrLn args

    -- ghciでのコマンドライン引数の設定の仕方
    --- ghci> :set args foo bar
    --- ghci> main