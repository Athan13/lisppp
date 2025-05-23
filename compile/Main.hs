module Main where
    import qualified Parser.Lexer as Lexer
    import qualified Parser.Parser as Parser
    import qualified Compiler.Compile as Compile

    parse :: String -> Either String Parser.Program
    parse s = Lexer.runAlex s Parser.blisp

    compile s = do
        ast <- parse s
        compiled <- Compile.compile ast
        return compiled
    
    main :: IO ()
    main = do
        s <- getContents
        case compile s of
            Left err -> print $ "Error: " ++ err
            Right success -> print success
