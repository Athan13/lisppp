module Main where
    import qualified Parser.Lexer as Lexer
    import qualified Parser.Parser as Parser
    import qualified Compiler.Compile as Compile

    import System.Environment
    import System.Directory (listDirectory, doesFileExist)
    import System.FilePath ((</>))

    import Control.Monad (filterM)

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
            in_file:"-o":out_file:_ -> do
                let dir = "blisp/stdlib"
                entries <- listDirectory dir
                let fullPaths = map (dir </>) entries
                files <- filterM doesFileExist fullPaths
                contents <- mapM readFile files
                s <- if in_file == "-i" then getContents else readFile in_file
                case compile (concat contents ++ s) of
                    Left err -> print $ "Error: " ++ err
                    Right success -> writeFile out_file $ Compile.showInstructions success
            _ -> putStrLn "Usage:\n\tcabal run compile -- [in-file.lisp | -i] -o <out-file.b>"
