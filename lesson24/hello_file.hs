import System.IO -- ファイルの読み書き

main :: IO ()
main = do
    -- openFile :: FilePath -> IOMonad -> IO Handle
    helloFile <- openFile "hello.txt" ReadMode
    firstLine <- hGetLine helloFile -- handle getLine
    putStrLn firstLine

    -- write
    goodbyeFile <- openFile "goodbye.txt" WriteMode
    hPutStrLn goodbyeFile firstLine

    -- handle (IO Handle) close
    hClose helloFile 
    hClose goodbyeFile 
    putStrLn "done!"