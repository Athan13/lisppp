module Main where
    import System.Environment (getArgs)

    import Interp (interp)

    main :: IO ()
    main = do
        args <- getArgs
        case args of
            file : tape_size : args_rest | not ("--help" `elem` args_rest) -> do
                let debug = "-v" `elem` args_rest
                let print_chars = "-c" `elem` args_rest
                brainfuck <- readFile file
                result <- interp (read tape_size) brainfuck (debug, print_chars)
                case result of
                    Left e  -> putStrLn $ "Interpreter error:\n\t" ++ show e
                    Right _ -> putStrLn "\nSuccessfully terminated."
            _ -> putStrLn "Usage:\n\tcabal run bracketfuck -- <file-name> <tape-size> [-c] [-v]\
\\n\n\t -c \t displays the output of the program as a character rather than a 8-bit unsigned int.\
\\n \t -v \t displays the state of the program after every instruction.\
\\n \t --help  displays this message."
