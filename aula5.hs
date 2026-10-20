ioSucc :: IO ()
ioSucc = do
    putStrLn "Write an Integer:"
    input <- getLine
    let num = (read input) :: Int
    putStrLn ("The successor is " ++ show (num + 1))

wc :: FilePath -> IO (Int,Int,Int)
wc fp = do
    text <- readFile fp
    return (length $ lines text, length $ words text, length text)

myGetLine :: IO String
myGetLine = do
    x <- getChar
    if (x=='\n')
        then return []
        else do
            xs <- myGetLine
            return $ x:xs


getNLine :: Int -> IO String
getNLine 1 = getLine
getNLine n 
    | n<1 = error "indice não positivo"
    | otherwise = do
        _ <- getLine
        getNLine (n-1)