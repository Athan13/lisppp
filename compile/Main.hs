module Main where
    import qualified Parser.Lexer as Lexer
    import qualified Parser.Parser as Parser

    parse :: String -> Either String Parser.Program
    parse s = Lexer.runAlex s Parser.blisp

    main :: IO ()
    main = do
        s <- getContents
        case parse s of
            Left err -> print $ "Error: " ++ err
            Right success -> print success
