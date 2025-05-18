module Main where
    import Parser.Lexer (alexScanTokens)

    main :: IO ()
    main = do
        s <- getContents
        print $ alexScanTokens s
