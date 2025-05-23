module Main where
    import qualified Parser.Lexer as Lexer
    import qualified Parser.Parser as Parser
    import qualified Compiler.Compile as Compile

    import System.Environment

    parse :: String -> Either String Parser.Program
    parse s = Lexer.runAlex s Parser.blisp

    compile :: String -> Either String [Compile.BFInstruction]
    compile s = do
        ast <- parse s
        compiled <- Compile.compile ast
        return compiled
    
    main :: IO ()
    main = do
        args <- getArgs
        case args of
            ["-o", out_file] -> do
                s <- getContents
                case compile s of
                    Left err -> print $ "Error: " ++ err
                    Right success -> writeFile out_file $ Compile.showInstructions success
            _ -> putStrLn "Usage:\n\tcabal run compile -- <in-file.lisp> -o <out-file.b>"
